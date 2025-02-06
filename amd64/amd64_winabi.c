#include "amd64_all.h"

#include <stdbool.h>

typedef enum ArgPassStyle {
  APS_Invalid = 0,
  APS_Register,
  APS_InlineOnStack,
  APS_CopyAndPointerInRegister,
  APS_CopyAndPointerOnStack,
} ArgPassStyle;

typedef struct ArgClass {
  Typ* type;
  ArgPassStyle style;
  int align;
  uint size;
  int cls;
  Ref ref;
} ArgClass;

typedef struct ExtraAlloc ExtraAlloc;
struct ExtraAlloc {
  Ins instr;
  ExtraAlloc* link;
};

#define ALIGN_DOWN(n, a) ((n) & ~((a)-1))
#define ALIGN_UP(n, a) ALIGN_DOWN((n) + (a)-1, (a))

// Number of stack bytes required be reserved for the callee.
#define SHADOW_SPACE_SIZE 32

int amd64_winabi_rsave[] = {RCX,  RDX,   R8,    R9,    R10,   R11,   RAX,  XMM0,
                            XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7, XMM8,
                            XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, -1};
int amd64_winabi_rclob[] = {RBX, R12, R13, R14, R15, RSI, RDI, -1};

MAKESURE(winabi_arrays_ok,
         sizeof amd64_winabi_rsave == (NGPS_WIN + NFPS + 1) * sizeof(int) &&
             sizeof amd64_winabi_rclob == (NCLR_WIN + 1) * sizeof(int));

// layout of call's second argument (RCall)
//
// bit 0: rax returned
// bit 1: xmm0 returns
// bits 2,3: 0
// bits 4567: rcx, rdx, r8, r9 passed
// bits 89ab: xmm0,1,2,3 passed
// bits c..1f: 0

bits amd64_winabi_retregs(Ref r, int p[2]) {
  assert(rtype(r) == RCall);

  bits b = 0;
  int num_int_returns = r.val & 1;
  int num_float_returns = r.val & 2;
  if (num_int_returns == 1) {
    b |= BIT(RAX);
  } else {
    b |= BIT(XMM0);
  }
  if (p) {
    p[0] = num_int_returns;
    p[1] = num_float_returns;
  }
  return b;
}

static uint popcnt(bits b) {
  b = (b & 0x5555555555555555) + ((b >> 1) & 0x5555555555555555);
  b = (b & 0x3333333333333333) + ((b >> 2) & 0x3333333333333333);
  b = (b & 0x0f0f0f0f0f0f0f0f) + ((b >> 4) & 0x0f0f0f0f0f0f0f0f);
  b += (b >> 8);
  b += (b >> 16);
  b += (b >> 32);
  return b & 0xff;
}

bits amd64_winabi_argregs(Ref r, int p[2]) {
  assert(rtype(r) == RCall);

  // On SysV, these are counts. Here, a count isn't sufficient, we actually need
  // to know which ones are in use because they're not necessarily contiguous.
  int int_passed = (r.val >> 4) & 15;
  int float_passed = (r.val >> 8) & 15;

  bits b = 0;
  b |= (int_passed & 1) ? BIT(RCX) : 0;
  b |= (int_passed & 2) ? BIT(RDX) : 0;
  b |= (int_passed & 4) ? BIT(R8) : 0;
  b |= (int_passed & 8) ? BIT(R9) : 0;
  b |= (float_passed & 1) ? BIT(XMM0) : 0;
  b |= (float_passed & 2) ? BIT(XMM1) : 0;
  b |= (float_passed & 4) ? BIT(XMM2) : 0;
  b |= (float_passed & 8) ? BIT(XMM3) : 0;
  if (p) {
    // TODO: The only place this is used is live.c. I'm not sure what should be
    // returned here wrt to using the same counter for int/float regs on win.
    // For now, try the number of registers in use even though they're not
    // contiguous.
    p[0] = popcnt(int_passed);
    p[1] = popcnt(float_passed);
  }
  return b;
}

typedef struct RegisterUsage {
  // Counter for both int/float as they're counted together. Only if the bool's
  // set in regs_passed is the given register *actually* needed for a value
  // (i.e. needs to be saved, etc.)
  int num_regs_passed;

  // Indexed first by 0=int, 1=float, use KBASE(cls).
  // Indexed second by register index in calling convention, so for integer,
  // 0=RCX, 1=RDX, 2=R8, 3=R9, and for float XMM0, XMM1, XMM2, XMM3.
  bool regs_passed[2][4];

  bool rax_returned;
  bool xmm0_returned;
} RegisterUsage;

static int register_usage_to_call_arg_value(RegisterUsage reg_usage) {
  return (reg_usage.rax_returned << 0) |  //
         (reg_usage.xmm0_returned << 1) |  //
         (reg_usage.regs_passed[0][0] << 4) |          //
         (reg_usage.regs_passed[0][1] << 5) |          //
         (reg_usage.regs_passed[0][2] << 6) |          //
         (reg_usage.regs_passed[0][3] << 7) |          //
         (reg_usage.regs_passed[1][0] << 8) |          //
         (reg_usage.regs_passed[1][1] << 9) |          //
         (reg_usage.regs_passed[1][2] << 10) |         //
         (reg_usage.regs_passed[1][3] << 11);
}

// Assigns the argument to a register if there's any left according to the
// calling convention, and updates the regs_passed bools. Otherwise marks the
// value as needing stack space to be passed.
static void assign_register_or_stack(RegisterUsage* reg_usage,
                                     ArgClass* arg,
                                     bool is_float,
                                     bool by_copy) {
  if (reg_usage->num_regs_passed == 4) {
    arg->style = by_copy ? APS_CopyAndPointerOnStack : APS_InlineOnStack;
  } else {
    reg_usage->regs_passed[is_float][reg_usage->num_regs_passed] = true;
    ++reg_usage->num_regs_passed;
    arg->style = by_copy ? APS_CopyAndPointerInRegister : APS_Register;
  }
}

static bool type_is_by_copy(Typ* type) {
  return type->isdark || (type->size != 1 && type->size != 2 &&
                          type->size != 4 && type->size != 8);
}

// This function is used for both arguments and parameters.
// begin_instr should either point at the first Oarg or Opar, and end_instr
// should point past the last one (so to the Ocall for arguments, or to the
// first 'real' instruction of the function for parameters).
static void classify_arguments(RegisterUsage* reg_usage,
                               Ins* begin_instr,
                               Ins* end_instr,
                               ArgClass* arg_classes) {

  ArgClass* arg = arg_classes;
  // For each argument, determine how it will be passed (int, float, stack)
  // and update the `reg_usage` counts. Additionally, fill out arg_classes for
  // each argument.
  for (Ins* instr = begin_instr; instr < end_instr; ++instr, ++arg) {
    switch (instr->op) {
      case Oarg:
      case Opar:
        assign_register_or_stack(reg_usage, arg, KBASE(instr->cls),
                                 /*by_copy=*/false);
        arg->cls = instr->cls;
        arg->align = 3;
        arg->size = 8;
        break;
      case Oargc:
      case Oparc: {
        int typ_index = instr->arg[0].val;
        Typ* type = &typ[typ_index];
        // Note that only these sizes are passed by register, even though e.g. a
        // 5 byte struct would "fit", it still is passed by copy-and-pointer.
        bool by_copy = type_is_by_copy(type);
        assign_register_or_stack(reg_usage, arg, /*is_float=*/false, by_copy);
        arg->cls = Kl;
        if (!by_copy && type->size <= 4) {
          arg->cls = Kw;
        }
        arg->align = 3;
        arg->size = type->size;
        break;
      }
      case Oarge:
      case Opare:
        die("not implemented");
      case Oargv:
        die("todo; varargs!");
    }
  }
}

static bool is_integer_type(int ty) {
  assert(ty >= 0 && ty < 4 && "expecting Kw Kl Ks Kd");
  return KBASE(ty) == 0;
}

static Ref register_for_arg(int cls, int counter) {
  assert(counter < 4);
  if (is_integer_type(cls)) {
    return TMP(amd64_winabi_rsave[counter]);
  } else {
    return TMP(XMM0 + counter);
  }
}

static Ins* lower_call(Fn* func,
                       Blk* block,
                       Ins* call_instr,
                       ExtraAlloc** pextra_alloc) {
  (void)pextra_alloc;

  // Call arguments are instructions. Walk through them to find the end of the
  // call+args that we need to process (and return the instruction past the body
  // of the instruction for continuing processing).
  Ins* instr_past_args = call_instr - 1;
  for (; instr_past_args >= block->ins; --instr_past_args) {
    if (!isarg(instr_past_args->op)) {
      break;
    }
  }
  Ins* earliest_arg_instr = instr_past_args + 1;

  // Don't need an ArgClass for the call itself, so one less than the total
  // number of instructions we're dealing with.
  uint num_args = call_instr - earliest_arg_instr;
  ArgClass* arg_classes = alloc(num_args * sizeof(ArgClass));

  // Ocall's two arguments are the the function to be called in 0, and, if the
  // the function returns a non-basic type, then arg[1] is a reference to the
  // type of the return.
  // TODO: doesn't do anything with `env`, I don't understand that feature yet.
  RegisterUsage reg_usage = {0};
  if (req(call_instr->arg[1], R)) {  // req checks if Refs are equal; `R` is 0.
    classify_arguments(&reg_usage, earliest_arg_instr, call_instr, arg_classes);
  } else {
    die("todo; ret");
  }

  // We now know which arguments are on the stack and which are in registers, so
  // we can allocate the correct amount of space to stash the stack-located ones
  // into.
  uint stack_usage = 0;
  for (uint i = 0; i < num_args; ++i) {
    ArgClass* arg = &arg_classes[i];
    // stack_usage only accounts for pushes that are for values that don't have
    // enough registers. Large struct copies are alloca'd separately, and then
    // only have (potentially) 8 bytes to add to stack_usage here.
    if (arg->style == APS_InlineOnStack) {
      if (arg->align > 4) {
        err("win abi cannot pass alignments > 16");
      }
      stack_usage += arg->size;
    } else if (arg->style == APS_CopyAndPointerOnStack) {
      stack_usage += 8;
    }
  }
  stack_usage = ALIGN_UP(stack_usage, 16);

  // Note that here we're logically 'after' the call (due to emitting
  // instructions in reverse order), so we're doing a negative stack
  // allocation to clean up after the call.
  Ref stack_size_ref =
      getcon(-(int64_t)(stack_usage + SHADOW_SPACE_SIZE), func);
  emit(Osalloc, Kl, R, stack_size_ref, R);

  ExtraAlloc* return_pad = NULL;
  if (req(call_instr->arg[1], R)) {
    // If only a basic type returned from the call.
    if (is_integer_type(call_instr->cls)) {
      emit(Ocopy, call_instr->cls, call_instr->to, TMP(RAX), R);
      reg_usage.rax_returned = true;
    } else {
      emit(Ocopy, call_instr->cls, call_instr->to, TMP(XMM0), R);
      reg_usage.xmm0_returned = true;
    }
  } else {
    // TODO: hidden first arg for structs by value here
    die("todo; hidden first arg for struct return");
  }

  // Emit the actual call instruction. There's no 'to' value by this point
  // because we've lowered it into register manipulation (that's the `R`),
  // arg[0] of the call is the function, and arg[1] is register usage is
  // documented as above (copied from SysV).
  emit(Ocall, call_instr->cls, R, call_instr->arg[0],
       CALL(register_usage_to_call_arg_value(reg_usage)));

  int reg_counter = 0;

  // TODO: pass hidden first arg here in rcx

  // This is where we actually do the load of values into registers or into
  // stack slots.
  Ref arg_stack_slots = newtmp("abi.args", Kl, func);
  uint slot_offset = SHADOW_SPACE_SIZE;
  ArgClass* arg = arg_classes;
  for (Ins* instr = earliest_arg_instr; instr != call_instr; ++instr, ++arg) {
    switch (arg->style) {
      case APS_Register: {
        Ref into = register_for_arg(arg->cls, reg_counter++);
        if (instr->op == Oargc) {
          // If this is a small struct being passed by value. The value in the
          // instruction in this case is a pointer, but it needs to be loaded
          // into the register.
          emit(Oload, arg->cls, into, instr->arg[1], R);
        } else {
          // Otherwise, a normal value passed in a register.
          emit(Ocopy, instr->cls, into, instr->arg[0], R);
        }
        break;
      }
      case APS_InlineOnStack: {
        Ref slot = newtmp("abi.off", Kl, func);
        if (instr->op == Oargc) {
          // This is a small struct, so it's not passed by copy, but the
          // instruction is a pointer. So we need to copy it into the stack
          // slot. (And, remember that these are emitted backwards, so store,
          // then load.)
          Ref smalltmp = newtmp("abi.smalltmp", arg->cls, func);
          emit(Ostorel, 0, R, smalltmp, slot);
          emit(Oload, arg->cls, smalltmp, instr->arg[1], R);
        } else {
          // Stash the value into the stack slot.
          emit(Ostorel, 0, R, instr->arg[0], slot);
        }
        emit(Oadd, Kl, slot, arg_stack_slots, getcon(slot_offset, func));
        slot_offset += arg->size;
        break;
      }
      case APS_CopyAndPointerInRegister:
      case APS_CopyAndPointerOnStack: {
        // Alloca a space to copy into, and blit the value from the instr to the
        // copied location.
        ExtraAlloc* arg_copy = alloc(sizeof(ExtraAlloc));
        Ref copy_ref = newtmp("abi.copy", Kl, func);
        arg_copy->instr =
            (Ins){Oalloc8, Kl, copy_ref, {getcon(arg->size, func)}};
        arg_copy->link = (*pextra_alloc);
        *pextra_alloc = arg_copy;
        emit(Oblit1, 0, R, INT(arg->size), R);
        emit(Oblit0, 0, R, instr->arg[1], copy_ref);

        // Now load the pointer into the correct register or stack slot.
        if (arg->style == APS_CopyAndPointerInRegister) {
          Ref into = register_for_arg(arg->cls, reg_counter++);
          emit(Ocopy, Kl, into, copy_ref, R);
        } else {
          assert(arg->style == APS_CopyAndPointerOnStack);
          Ref slot = newtmp("abi.off", Kl, func);
          emit(Ostorel, 0, R, copy_ref, slot);
          emit(Oadd, Kl, slot, arg_stack_slots, getcon(slot_offset, func));
          slot_offset += 8;
        }
        break;
      }
      case APS_Invalid:
        die("unreachable");
    }
  }

  if (stack_usage) {
    // The last (first in call order) thing we do is allocate the the stack
    // space we're going to fill with temporaries.
    emit(Osalloc, Kl, arg_stack_slots,
         getcon(stack_usage + SHADOW_SPACE_SIZE, func), R);
  } else {
    // When there's no usage for temporaries, we can add this into the other
    // alloca, but otherwise emit it separately (not storing into a reference)
    // so that it doesn't get removed later for being useless.
    emit(Osalloc, Kl, R, getcon(SHADOW_SPACE_SIZE, func), R);
  }

  return instr_past_args;
}

static void lower_block_return(Fn* func, Blk* block) {
  int jmp_type = block->jmp.type;

  if (!isret(jmp_type) || jmp_type == Jret0) {
    return;
  }

  // Save the argument, and set the block to be a void return because once it's
  // lowered it's handled by the the register/stack manipulation.
  Ref ret_arg = block->jmp.arg;
  block->jmp.type = Jret0;

  RegisterUsage reg_usage = {0};

  if (jmp_type == Jretc) {
    Typ* type = &typ[func->retty];
    if (type_is_by_copy(type)) {
      assert(rtype(func->retr) == RTmp);
      emit(Ocopy, Kl, TMP(RAX), func->retr, R);
      emit(Oblit1, 0, R, INT(type->size), R);
      emit(Oblit0, 0, R, ret_arg, func->retr);
    } else {
      emit(Oload, Kl, TMP(RAX), ret_arg, R);
    }
    reg_usage.rax_returned = true;
  } else {
    int k = jmp_type - Jretw;
    if (is_integer_type(k)) {
      emit(Ocopy, k, TMP(RAX), ret_arg, R);
      reg_usage.rax_returned = true;
    } else {
      emit(Ocopy, k, TMP(XMM0), ret_arg, R);
      reg_usage.xmm0_returned = true;
    }
  }
  block->jmp.arg = CALL(register_usage_to_call_arg_value(reg_usage));
}

static void lower_args_for_block(Fn* func,
                                 Blk* block,
                                 ExtraAlloc** pextra_alloc) {
  if (block->visit) {
    return;
  }

  // global temporary buffer used by emit. Reset to the end, and predecremented
  // when adding to it.
  curi = &insb[NIns];

  lower_block_return(func, block);

  if (block->ins) {
    // Work backwards through the instructions, either copying them unchanged,
    // or modifying as necessary.
    for (Ins* instr = &block->ins[block->nins - 1]; instr >= block->ins;) {
      switch (instr->op) {
        case Ocall:
          instr = lower_call(func, block, instr, pextra_alloc);
          break;
        case Ovastart:
        case Ovaarg:
          die("todo!");
        case Oarg:
        case Oargc:
          die("unreachable");
        default:
          emiti(*instr);
          --instr;
          break;
      }
    }
  }

  // This it the start block, which is processed last. Add any allocas that
  // other blocks needed.
  bool is_start_block = block == func->start;
  if (is_start_block) {
    for (ExtraAlloc* ea = *pextra_alloc; ea; ea = ea->link) {
      emiti(ea->instr);
    }
  }

  // emit/emiti add instructions from the end to the beginning of the temporary
  // global buffer. dup the final version into the final block storage.
  block->nins = &insb[NIns] - curi;
  idup(&block->ins, curi, block->nins);
}

static Ins* find_end_of_func_parameters(Blk* start_block) {
  Ins* i;
  for (i = start_block->ins; i < &start_block->ins[start_block->nins]; ++i) {
    if (!ispar(i->op)) {
      break;
    }
  }
  return i;
}

// Copy from registers/stack into values.
static void lower_func_parameters(Fn* func) {
  // This is half-open, so end points after the last Opar.
  Blk* start_block = func->start;
  Ins* start_of_params = start_block->ins;
  Ins* end_of_params = find_end_of_func_parameters(start_block);

  size_t num_params = end_of_params - start_of_params;
  ArgClass* arg_classes = alloc(num_params * sizeof(ArgClass));
  ArgClass arg_ret = {0};

  // global temporary buffer used by emit. Reset to the end, and predecremented
  // when adding to it.
  curi = &insb[NIns];

  RegisterUsage reg_usage = {0};
  if (func->retty >= 0) {
    bool by_copy = type_is_by_copy(&typ[func->retty]);
    assign_register_or_stack(&reg_usage, &arg_ret, /*is_float=*/false, by_copy);
    if (by_copy) {
      Ref ret_ref = newtmp("abi.ret", Kl, func);
      emit(Ocopy, Kl, ret_ref, TMP(RCX), R);
      func->retr = ret_ref;
    }
  }
  classify_arguments(&reg_usage, start_of_params, end_of_params, arg_classes);
  func->reg = amd64_winabi_argregs(
      CALL(register_usage_to_call_arg_value(reg_usage)), NULL);

  // Copy from the registers or stack slots into the named parameters. Depending
  // on how they're passed, they either need to be copied or loaded.
  ArgClass* arg = arg_classes;
  int reg_counter = 0;
  uint slot_offset = SHADOW_SPACE_SIZE / 4 + 4;
  for (Ins* instr = start_of_params; instr < end_of_params; ++instr, ++arg) {
    switch (arg->style) {
      case APS_Register: {
        Ref from = register_for_arg(arg->cls, reg_counter++);
        emit(Ocopy, instr->cls, instr->to, from, R);
        break;
      }

      case APS_InlineOnStack:
        emit(Ocopy, Kl, instr->to, SLOT(-slot_offset), R);
        slot_offset += 2;
        break;

      case APS_CopyAndPointerOnStack:
        emit(Oload, Kl, instr->to, SLOT(-slot_offset), R);
        slot_offset += arg->size / 4;
        break;

      case APS_CopyAndPointerInRegister: {
        // Because this has to be a copy (that we own), it is sufficient to just
        // copy the register to the target.
        Ref from = register_for_arg(Kl, reg_counter++);
        emit(Ocopy, Kl, instr->to, from, R);
        break;
      }

      case APS_Invalid:
        die("unreachable");
    }
  }

  int num_created_instrs = &insb[NIns] - curi;
  int num_other_after_instrs = start_block->nins - num_params;
  int new_total_instrs = num_other_after_instrs + num_created_instrs;
  Ins* new_instrs = alloc(new_total_instrs * sizeof(Ins));
  Ins* instr_p = icpy(new_instrs, curi, num_created_instrs);
  icpy(instr_p, end_of_params, num_other_after_instrs);
  start_block->nins = new_total_instrs;
  start_block->ins = new_instrs;
}

// The main job of this function is to lower generic instructions into the
// specific details of how arguments are passed, and parameters are
// interpreted for win x64. A useful reference is
// https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention .
//
// Some of the major differences from SysV if you're comparing the code
// (non-exhaustive):
// - only 4 int and 4 float regs are used
// - when an int register is assigned a value, its associated float register is
//   left unused (and vice versa). i.e. there's only one counter as you assign
//   arguments to registers.
// - any structs that aren't 1/2/4/8 bytes in size are passed by pointer, not
//   by copying them into the stack. So e.g. if you pass something like
//   `struct { void*, int64_t }` by value, it first needs to be copied to
//   another alloca (in order to maintain value semantics at the language
//   level), then the pointer to that copy is treated as a regular integer
//   argument (which then itself may *also* be copied to the stack in the case
//   there's no integer register remaining.)
void amd64_winabi_abi(Fn* func) {
  fprintf(stderr, "-------- BEFORE amd64_winabi_abi:\n");
  printfn(func, stderr);

  // Reset |visit| flags for each block of the function. These are a
  // flag/counter to know when a processing step has already done something to
  // the block.
  for (Blk* block = func->start; block; block = block->link) {
    block->visit = 0;
  }

  // The first thing to do is lower incoming parameters to this function.
  lower_func_parameters(func);

  // This is the second larger part of the job. We walk all blocks, and rewrite
  // instructions returns, calls, and handling of varargs into their win x64
  // specific versions. Any other instructions are just passed through unchanged
  // by using `emiti`.

  // Skip over the entry block, and do it at the end so that our later
  // modifications can add allocations to the start block. In particular, we
  // need to add stack allocas for copies when structs are passed or returned by
  // value.
  ExtraAlloc* extra_alloc = NULL;
  for (Blk* block = func->start->link; block; block = block->link) {
    lower_args_for_block(func, block, &extra_alloc);
  }
  lower_args_for_block(func, func->start, &extra_alloc);

  fprintf(stderr, "-------- AFTER amd64_winabi_abis:\n");
  printfn(func, stderr);

  if (debug['A']) {
    fprintf(stderr, "\n> After ABI lowering:\n");
    printfn(func, stderr);
  }
}
