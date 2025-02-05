#include "amd64_all.h"

#include <stdbool.h>

typedef enum ArgPassStyle {
  APS_Register,
  APS_InlineOnStack,
  APS_CopyAndPointer,
} ArgPassStyle;

typedef struct ArgClass {
  Typ* type;
  ArgPassStyle style;
  int align;
  uint size;
  int cls;
  Ref ref;
} ArgClass;

typedef struct RAlloc RAlloc;
struct RAlloc {
  Ins instr;
  RAlloc* link;
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

#if 0
static void
classify(AClass *a, Typ *t, uint s)
{
	Field *f;
	int *cls;
	uint n, s1;

	for (n=0, s1=s; n<t->nunion; n++, s=s1)
		for (f=t->fields[n]; f->type!=FEnd; f++) {
			assert(s <= 16);
			cls = &a->cls[s/8];
			switch (f->type) {
			case FEnd:
				die("unreachable");
			case FPad:
				/* don't change anything */
				s += f->len;
				break;
			case Fs:
			case Fd:
				if (*cls == Kx)
					*cls = Kd;
				s += f->len;
				break;
			case Fb:
			case Fh:
			case Fw:
			case Fl:
				*cls = Kl;
				s += f->len;
				break;
			case FTyp:
				classify(a, &typ[f->len], s);
				s += typ[f->len].size;
				break;
			}
		}
}

static void
typclass(AClass *a, Typ *t)
{
	uint sz, al;

	sz = t->size;
	al = 1u << t->align;

	/* the ABI requires sizes to be rounded
	 * up to the nearest multiple of 8, moreover
	 * it makes it easy load and store structures
	 * in registers
	 */
	if (al < 8)
		al = 8;
	sz = (sz + al-1) & -al;

	a->type = t;
	a->size = sz;
	a->align = t->align;

	if (t->isdark || (sz != 1 && sz != 2 && sz != 4 && sz != 8) || sz == 0) {
		/* large or unaligned structures are required
		 * to be copied and passed by pointer. */
		a->inmem = 1;
		return;
	}

	a->cls[0] = Kx;
	a->cls[1] = Kx;
	a->inmem = 0;
	classify(a, t, 0);
}

static int
retr(Ref *reg, AClass *aret)
{
	int k;

	assert(aret->size <= 8);
	k = KBASE(aret->cls[0]);
	*reg = (k == 0 ? TMP(RAX) : TMP(XMM0));
	return 1 << (2 * k);
}

static void
selret(Blk *b, Fn *fn)
{
	int j, k, ca;
	Ref r0, reg;
	AClass aret;

	j = b->jmp.type;

	if (!isret(j) || j == Jret0)
		return;

	r0 = b->jmp.arg;
	b->jmp.type = Jret0;

	if (j == Jretc) {
		typclass(&aret, &typ[fn->retty]);
		if (aret.inmem) {
			assert(rtype(fn->retr) == RTmp);
			emit(Ocopy, Kl, TMP(RAX), fn->retr, R);
			emit(Oblit1, 0, R, INT(aret.type->size), R);
			emit(Oblit0, 0, R, r0, fn->retr);
			ca = 1;
		} else {
			ca = retr(&reg, &aret);
			assert(aret.size <= 8);
			emit(Oload, Kl, reg, r0, R);
		}
	} else {
		k = j - Jretw;
		if (KBASE(k) == 0) {
			emit(Ocopy, k, TMP(RAX), r0, R);
			ca = 1;
		} else {
			emit(Ocopy, k, TMP(XMM0), r0, R);
			ca = 1 << 2;
		}
	}

	b->jmp.arg = CALL(ca);
}

static int
argsclass(Ins *i0, Ins *i1, AClass *ac, int op, AClass *aret, Ref *env)
{
	int varc, envc, nint, ni, nsse, ns, n, *pn;
	AClass *a;
	Ins *i;

	if (aret && aret->inmem)
		nint = 3; /* hidden argument */
	else
		nint = 4;
	nsse = 8;
	varc = 0;
	envc = 0;
	for (i=i0, a=ac; i<i1; i++, a++)
		switch (i->op - op + Oarg) {
		case Oarg:
			if (KBASE(i->cls) == 0)
				pn = &nint;
			else
				pn = &nsse;
			if (*pn > 0) {
				--*pn;
				a->inmem = 0;
			} else
				a->inmem = 2;
			a->align = 3;
			a->size = 8;
			a->cls[0] = i->cls;
			break;
		case Oargc:
			n = i->arg[0].val;
			typclass(a, &typ[n]);
			if (a->inmem) {
				if (nint > 0) {
					--nint;
				}
				a->align = 3;
				a->size = 8;
				a->cls[0] = i->cls;
				continue;
			}
			abort(); // this is probably wrong for win
			/*
			ni = ns = 0;
			for (n=0; (uint)n*8<a->size; n++)
				if (KBASE(a->cls[n]) == 0)
					ni++;
				else
					ns++;
			if (nint >= ni && nsse >= ns) {
				nint -= ni;
				nsse -= ns;
			} else
				a->inmem = 1;
				*/
			break;
		case Oarge:
			envc = 1;
			if (op == Opar)
				*env = i->to;
			else
				*env = i->arg[0];
			break;
		case Oargv:
			varc = 1;
			break;
		default:
			die("unreachable");
		}

	if (varc && envc)
		err("winabi does not support variadic env calls");

	return ((varc|envc) << 12) | ((4-nint) << 4) | ((8-nsse) << 8);
}

static Ref
rarg(int ty, int *ni, int *ns)
{
	if (KBASE(ty) == 0)
		return TMP(amd64_winabi_rsave[(*ni)++]);
	else
		return TMP(XMM0 + (*ns)++);
}

static void
selcall(Fn *fn, Ins *i0, Ins *i1, RAlloc **rap)
{
	Ins *i;
	AClass *ac, *a, aret = {0};
	int ca, ni, ns, al;
	uint stk, off;
	Ref r, r1, r2, reg[2], env;
	RAlloc *ra;
	RAlloc *copya;

	env = R;
	ac = alloc((i1-i0) * sizeof ac[0]);

	if (!req(i1->arg[1], R)) {
		assert(rtype(i1->arg[1]) == RType);
		typclass(&aret, &typ[i1->arg[1].val]);
		ca = argsclass(i0, i1, ac, Oarg, &aret, &env);
	} else
		ca = argsclass(i0, i1, ac, Oarg, 0, &env);

#if 0
	for (stk=0, a=&ac[i1-i0]; a>ac;)
		if ((--a)->inmem) {
			if (a->align > 4)
				err("win abi requires alignments of 16 or less");
			r1 = newtmp("ptrcpy", Kl, fn);
			copya = alloc(sizeof *copya);
			al = aret.align >= 2 ? aret.align - 2 : 0;
			copya->i = (Ins){Oalloc+al, Kl, r1, {getcon(a->size, fn)}};
			copya->link = (*rap);
			*rap = copya;
			stk += 8;
			if (a->align == 4)
				stk += stk & 15;
		}
	stk += stk & 15;
#endif
	/* shadow space. TODO: would perhaps be better to do this when !fn->leaf in
	 * the head of the function, rather than before/after every call. */
	stk = 32;
	r = getcon(-(int64_t)stk, fn);
	emit(Osalloc, Kl, R, r, R);

	if (!req(i1->arg[1], R)) {
		if (aret.inmem) {
			/* get the return location from eax
			 * it saves one callee-save reg */
			r1 = newtmp("abi", Kl, fn);
			emit(Ocopy, Kl, i1->to, TMP(RAX), R);
			ca += 1;
		} else {
			/* todo, may read out of bounds.
			 * gcc did this up until 5.2, but
			 * this should still be fixed.
			 */
			if (aret.size > 8) {
				r = newtmp("abi", Kl, fn);
				aret.ref[1] = newtmp("abi", aret.cls[1], fn);
				emit(Ostorel, 0, R, aret.ref[1], r);
				emit(Oadd, Kl, r, i1->to, getcon(8, fn));
			}
			aret.ref[0] = newtmp("abi", aret.cls[0], fn);
			emit(Ostorel, 0, R, aret.ref[0], i1->to);
			ca += retr(reg, &aret);
			if (aret.size > 8)
				emit(Ocopy, aret.cls[1], aret.ref[1], reg[1], R);
			emit(Ocopy, aret.cls[0], aret.ref[0], reg[0], R);
			r1 = i1->to;
		}
		/* allocate return pad */
		ra = alloc(sizeof *ra);
		/* specific to NAlign == 3 */
		al = aret.align >= 2 ? aret.align - 2 : 0;
		ra->i = (Ins){Oalloc+al, Kl, r1, {getcon(aret.size, fn)}};
		ra->link = (*rap);
		*rap = ra;
	} else {
		ra = 0;
		if (KBASE(i1->cls) == 0) {
			emit(Ocopy, i1->cls, i1->to, TMP(RAX), R);
			ca += 1;
		} else {
			emit(Ocopy, i1->cls, i1->to, TMP(XMM0), R);
			ca += 1 << 2;
		}
	}

        printref(i1->arg[0], fn, stderr); fprintf(stderr, "\n");
	emit(Ocall, i1->cls, R, i1->arg[0], CALL(ca));

	if (!req(R, env))
		emit(Ocopy, Kl, TMP(RAX), env, R);
	else if ((ca >> 12) & 1) /* vararg call */
		emit(Ocopy, Kw, TMP(RAX), getcon((ca >> 8) & 15, fn), R);

	ni = ns = 0;
	if (ra && aret.inmem)
		emit(Ocopy, Kl, rarg(Kl, &ni, &ns), ra->i.to, R); /* pass hidden argument */

	for (i=i0, a=ac; i<i1; i++, a++) {
		if (i->op >= Oarge)
			continue;
		r1 = rarg(a->cls[0], &ni, &ns);
		/*if (i->op == Oargc) {
			if (a->size > 8) {
				r2 = rarg(a->cls[1], &ni, &ns);
				r = newtmp("abi", Kl, fn);
				emit(Oload, a->cls[1], r2, r, R);
				emit(Oadd, Kl, r, i->arg[1], getcon(8, fn));
			}
			emit(Oload, a->cls[0], r1, i->arg[1], R);
		} else*/
			emit(Ocopy, i->cls, r1, i->arg[0], R);
	}

	if (!stk)
		return;

	r = newtmp("abi", Kl, fn);
	for (i=i0, a=ac, off=0; i<i1; i++, a++) {
		if (i->op >= Oarge || !a->inmem)
			continue;
		r1 = newtmp("abi", Kl, fn);
		if (i->op == Oargc) {
			if (a->align == 4)
				off += off & 15;
			emit(Oblit1, 0, R, INT(a->type->size), R);
			emit(Oblit0, 0, R, i->arg[1], r1);
		} else
			emit(Ostorel, 0, R, i->arg[0], r1);
		emit(Oadd, Kl, r1, r, getcon(off, fn));
		off += a->size;
	}

	emit(Osalloc, Kl, r, getcon(stk, fn), R);
	if (off == 0) {
		/* hack to not drop the salloc even if it's otherwise unused */
		emit(Ocopy, Kl, r, r, R);
	}
}

static int
selpar(Fn *fn, Ins *i0, Ins *i1)
{
	AClass *ac, *a, aret = {0};
	Ins *i;
	int ni, ns, s, al, fa;
	Ref r, env;

	env = R;
	ac = alloc((i1-i0) * sizeof ac[0]);
	curi = &insb[NIns];
	ni = ns = 0;

	if (fn->retty >= 0) {
		typclass(&aret, &typ[fn->retty]);
		fa = argsclass(i0, i1, ac, Opar, &aret, &env);
	} else
		fa = argsclass(i0, i1, ac, Opar, 0, &env);
	fn->reg = amd64_winabi_argregs(CALL(fa), 0);

	for (i=i0, a=ac; i<i1; i++, a++) {
		if (i->op != Oparc || a->inmem)
			continue;
		if (a->size > 8) {
			r = newtmp("abi", Kl, fn);
			a->ref[1] = newtmp("abi", Kl, fn);
			emit(Ostorel, 0, R, a->ref[1], r);
			emit(Oadd, Kl, r, i->to, getcon(8, fn));
		}
		a->ref[0] = newtmp("abi", Kl, fn);
		emit(Ostorel, 0, R, a->ref[0], i->to);
		/* specific to NAlign == 3 */
		al = a->align >= 2 ? a->align - 2 : 0;
		emit(Oalloc+al, Kl, i->to, getcon(a->size, fn), R);
	}

	if (fn->retty >= 0 && aret.inmem) {
		r = newtmp("abi", Kl, fn);
		emit(Ocopy, Kl, r, rarg(Kl, &ni, &ns), R);
		fn->retr = r;
	}

	for (i=i0, a=ac, s=4; i<i1; i++, a++) {
		switch (a->inmem) {
		case 1:
			if (a->align > 4)
				err("win abi requires alignments of 16 or less");
			if (a->align == 4)
				s = (s+3) & -4;
			fn->tmp[i->to.val].slot = -s;
			s += a->size / 4;
			continue;
		case 2:
			emit(Oload, i->cls, i->to, SLOT(-s), R);
			s += 2;
			continue;
		}
		if (i->op == Opare)
			continue;
		r = rarg(a->cls[0], &ni, &ns);
		if (i->op == Oparc) {
			emit(Ocopy, a->cls[0], a->ref[0], r, R);
			if (a->size > 8) {
				r = rarg(a->cls[1], &ni, &ns);
				emit(Ocopy, a->cls[1], a->ref[1], r, R);
			}
		} else
			emit(Ocopy, i->cls, i->to, r, R);
	}

	if (!req(R, env))
		emit(Ocopy, Kl, env, TMP(RAX), R);

	return fa | (s*4)<<12;
}

static Blk *
split(Fn *fn, Blk *b)
{
	Blk *bn;

	++fn->nblk;
	bn = newblk();
	bn->nins = &insb[NIns] - curi;
	idup(&bn->ins, curi, bn->nins);
	curi = &insb[NIns];
	bn->visit = ++b->visit;
	strf(bn->name, "%s.%d", b->name, b->visit);
	bn->loop = b->loop;
	bn->link = b->link;
	b->link = bn;
	return bn;
}

static void
chpred(Blk *b, Blk *bp, Blk *bp1)
{
	Phi *p;
	uint a;

	for (p=b->phi; p; p=p->link) {
		for (a=0; p->blk[a]!=bp; a++)
			assert(a+1<p->narg);
		p->blk[a] = bp1;
	}
}

static void
selvaarg(Fn *fn, Blk *b, Ins *i)
{
	Ref loc, lreg, lstk, nr, r0, r1, c4, c8, c16, c, ap;
	Blk *b0, *bstk, *breg;
	int isint;

	c4 = getcon(4, fn);
	c8 = getcon(8, fn);
	c16 = getcon(16, fn);
	ap = i->arg[0];
	isint = KBASE(i->cls) == 0;

	/* @b [...]
		   r0 =l add ap, (0 or 4)
		   nr =l loadsw r0
		   r1 =w cultw nr, (48 or 176)
		   jnz r1, @breg, @bstk
	   @breg
		   r0 =l add ap, 16
		   r1 =l loadl r0
		   lreg =l add r1, nr
		   r0 =w add nr, (8 or 16)
		   r1 =l add ap, (0 or 4)
		   storew r0, r1
	   @bstk
		   r0 =l add ap, 8
		   lstk =l loadl r0
		   r1 =l add lstk, 8
		   storel r1, r0
	   @b0
		   %loc =l phi @breg %lreg, @bstk %lstk
		   i->to =(i->cls) load %loc
	*/

	loc = newtmp("abi", Kl, fn);
	emit(Oload, i->cls, i->to, loc, R);
	b0 = split(fn, b);
	b0->jmp = b->jmp;
	b0->s1 = b->s1;
	b0->s2 = b->s2;
	if (b->s1)
		chpred(b->s1, b, b0);
	if (b->s2 && b->s2 != b->s1)
		chpred(b->s2, b, b0);

	lreg = newtmp("abi", Kl, fn);
	nr = newtmp("abi", Kl, fn);
	r0 = newtmp("abi", Kw, fn);
	r1 = newtmp("abi", Kl, fn);
	emit(Ostorew, Kw, R, r0, r1);
	emit(Oadd, Kl, r1, ap, isint ? CON_Z : c4);
	emit(Oadd, Kw, r0, nr, isint ? c8 : c16);
	r0 = newtmp("abi", Kl, fn);
	r1 = newtmp("abi", Kl, fn);
	emit(Oadd, Kl, lreg, r1, nr);
	emit(Oload, Kl, r1, r0, R);
	emit(Oadd, Kl, r0, ap, c16);
	breg = split(fn, b);
	breg->jmp.type = Jjmp;
	breg->s1 = b0;

	lstk = newtmp("abi", Kl, fn);
	r0 = newtmp("abi", Kl, fn);
	r1 = newtmp("abi", Kl, fn);
	emit(Ostorel, Kw, R, r1, r0);
	emit(Oadd, Kl, r1, lstk, c8);
	emit(Oload, Kl, lstk, r0, R);
	emit(Oadd, Kl, r0, ap, c8);
	bstk = split(fn, b);
	bstk->jmp.type = Jjmp;
	bstk->s1 = b0;

	b0->phi = alloc(sizeof *b0->phi);
	*b0->phi = (Phi){
		.cls = Kl, .to = loc,
		.narg = 2,
		.blk = vnew(2, sizeof b0->phi->blk[0], PFn),
		.arg = vnew(2, sizeof b0->phi->arg[0], PFn),
	};
	b0->phi->blk[0] = bstk;
	b0->phi->blk[1] = breg;
	b0->phi->arg[0] = lstk;
	b0->phi->arg[1] = lreg;
	r0 = newtmp("abi", Kl, fn);
	r1 = newtmp("abi", Kw, fn);
	b->jmp.type = Jjnz;
	b->jmp.arg = r1;
	b->s1 = breg;
	b->s2 = bstk;
	c = getcon(isint ? 48 : 176, fn);
	emit(Ocmpw+Ciult, Kw, r1, nr, c);
	emit(Oloadsw, Kl, nr, r0, R);
	emit(Oadd, Kl, r0, ap, isint ? CON_Z : c4);
}

static void
selvastart(Fn *fn, int fa, Ref ap)
{
	Ref r0, r1;
	int gp, fp, sp;

	gp = ((fa >> 4) & 15) * 8;
	fp = 48 + ((fa >> 8) & 15) * 16;
	sp = fa >> 12;
	r0 = newtmp("abi", Kl, fn);
	r1 = newtmp("abi", Kl, fn);
	emit(Ostorel, Kw, R, r1, r0);
	emit(Oadd, Kl, r1, TMP(RBP), getcon(-176, fn));
	emit(Oadd, Kl, r0, ap, getcon(16, fn));
	r0 = newtmp("abi", Kl, fn);
	r1 = newtmp("abi", Kl, fn);
	emit(Ostorel, Kw, R, r1, r0);
	emit(Oadd, Kl, r1, TMP(RBP), getcon(sp, fn));
	emit(Oadd, Kl, r0, ap, getcon(8, fn));
	r0 = newtmp("abi", Kl, fn);
	emit(Ostorew, Kw, R, getcon(fp, fn), r0);
	emit(Oadd, Kl, r0, ap, getcon(4, fn));
	emit(Ostorew, Kw, R, getcon(gp, fn), ap);
}

#endif

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
  // to know which ones are in use because they're not necessarily
  // "contiguous".
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
    // For now, try the number of registers in use.
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

// This function is used for both arguments and parameters.
static RegisterUsage classify_arguments(Ins* earliest_arg_instr,
                                        Ins* call_instr,
                                        ArgClass* arg_classes,
                                        ArgClass* return_class) {
  RegisterUsage reg_usage = {0};
  assert(!return_class && "todo");

  ArgClass* arg = arg_classes;
  // For each argument, determine how it will be passed (int, float, stack)
  // and update the `reg_usage` counts. Additionally, fill out arg_classes for
  // each argument.
  for (Ins* instr = earliest_arg_instr; instr < call_instr; ++instr, ++arg) {
    switch (instr->op) {
      case Oarg:
      case Opar:
        if (reg_usage.num_regs_passed == 4) {
          arg->style = APS_InlineOnStack;
        } else {
          reg_usage.regs_passed[KBASE(instr->cls)][reg_usage.num_regs_passed] =
              true;
          ++reg_usage.num_regs_passed;
          arg->style = APS_Register;
        }
        arg->align = 3;
        arg->size = 8;
        arg->cls = instr->cls;
        break;
      case Oargc:
      case Oparc:
        break;
      case Oarge:
      case Opare:
        die("not implemented");
      case Oargv:
        die("todo; varargs!");
    }
  }

  return reg_usage;
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
                       RAlloc** pralloc) {
  (void)pralloc;

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
  RegisterUsage reg_usage;
  if (req(call_instr->arg[1], R)) {  // req checks if Refs are equal; `R` is 0.
    reg_usage =
        classify_arguments(earliest_arg_instr, call_instr, arg_classes, NULL);
  } else {
    abort();
  }

  // We now know which arguments are on the stack and which are in registers, so
  // we can allocate the correct amount of space to stash the stack-located ones
  // into.
  uint stack_usage = 0;
  for (uint i = 0; i < num_args; ++i) {
    ArgClass* arg = &arg_classes[i];
    if (arg->style != APS_Register) {
      if (arg->align > 4) {
        err("win abi cannot pass alignments > 16");
      }
      stack_usage += arg->size;
      // TODO: SysV does something strange with align 16 here that I'm not sure
      // is right? need some tests to figure out what's going on.
    }
  }
  stack_usage = ALIGN_UP(stack_usage, 16);

  // Note that here we're logically 'after' the call (due to emitting
  // instructions in reverse order), so we're doing a negative stack
  // allocation to clean up after the call.
  Ref stack_size_ref =
      getcon(-(int64_t)(stack_usage + SHADOW_SPACE_SIZE), func);
  emit(Osalloc, Kl, R, stack_size_ref, R);

  RAlloc* return_pad = NULL;
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
    abort();
  }

  // Emit the actual call instruction. There's no 'to' value by this point
  // because we've lowered it into register manipulation (that's the `R`),
  // arg[0] of the call is the function, and arg[1] is register usage is
  // documented as above (copied from SysV).
  emit(Ocall, call_instr->cls, R, call_instr->arg[0],
       CALL(register_usage_to_call_arg_value(reg_usage)));

  int reg_counter = 0;

  // TODO: pass hidden first arg here in rcx

  // This is where we actually do the load of values into registers.
  ArgClass* arg = arg_classes;
  for (Ins* instr = earliest_arg_instr; instr != call_instr; ++instr, ++arg) {
    switch (arg->style) {
      case APS_Register: {
        Ref into = register_for_arg(arg->cls, reg_counter++);
        if (instr->op == Oargc) {
          // If this is a struct being passed by value.
          abort();
        } else {
          emit(Ocopy, instr->cls, into, instr->arg[0], R);
        }
        break;
      }
      default:
        die("todo");
    }
  }

  if (stack_usage) {
    Ref temps_base_ref = newtmp("abi.temps", Kl, func);

    // The last (first in call order) thing we do is allocate the the stack space
    // we're going to fill with temporaries.
    emit(Osalloc, Kl, temps_base_ref,
         getcon(stack_usage + SHADOW_SPACE_SIZE, func), R);
  } else {
    // When there's no usage for temporaries, we can add this into the other
    // alloca, but otherwise emit it separately (not storing into a reference)
    // so that it doesn't removed later for being useless.
    emit(Osalloc, Kl, R, getcon(SHADOW_SPACE_SIZE, func), R);
  }

  return instr_past_args;
}

static void lower_args_for_block(Fn* func, Blk* block, RAlloc** pralloc) {
  if (block->visit) {
    return;
  }

  // global temporary buffer used by emit. Reset to the end, and predecremented
  // when adding to it.
  curi = &insb[NIns];

  // lower_block_returns(func, block);

  // Work backwards through the instructions, either copying them unchanged, or
  // modifying as necessary.
  for (Ins* instr = &block->ins[block->nins - 1]; instr >= block->ins;) {
    switch (instr->op) {
      case Ocall:
        instr = lower_call(func, block, instr, pralloc);
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

  // This it the start block, which is processed last. Add any allocas that
  // other blocks needed.
  bool is_start_block = block == func->start;
  if (is_start_block) {
    for (RAlloc* ralloc = *pralloc; ralloc; ralloc = ralloc->link) {
      emiti(ralloc->instr);
    }
  }

  // emit/emiti add instructions from the end to the beginning of the temporary
  // global buffer. dup the final version into the final block storage.
  block->nins = &insb[NIns] - curi;
  idup(&block->ins, curi, block->nins);
}

// The main job of this function is to lower generic instructions into the
// specific details of how arguments are passed, and parameters are
// interpreted for win x64. A useful reference is
// https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention .
//
// Some of the major differences from SysV if you're comparing the code
// (non-exhaustive of course):
// - only 4 int and 4 float regs are used
// - when an int register is assigned a value, its associated float register is
//   left unused (and vice versa). i.e. there's only one counter as you assign
//   arguments to registers.
// - any structs that aren't 1/2/4/8 bytes in size are passed by pointer, not
//   by copying them into the stack. So e.g. if you pass something like
//   `struct { void*, int64_t }` by value, it first needs to be copied to
//   another alloca (in order to maintain value semantics at the language
//   level), then the pointer to that copy is treated as a regular integer
//   argument (which then itself *also* be copied to the stack in the case
//   there's no integer register remaining.)
void amd64_winabi_abi(Fn* fn) {
  fprintf(stderr, "-------- BEFORE amd64_winabi_abi:\n");
  printfn(fn, stderr);

  // Reset |visit| flags for each block of the function. These are a
  // flag/counter to know when a processing step has already done something to
  // the block.
  for (Blk* block = fn->start; block; block = block->link) {
    block->visit = 0;
  }

  // The first thing to do is lower incoming parameters to this function.

  // This is the second larger part of the job. We walk all blocks, and rewrite
  // instructions returns, calls, and handling of varargs into their win x64
  // specific versions. Any other instructions are just passed through unchanged
  // by using `emiti`.

  // Skip over the entry block, and do it at the end so that our later
  // modifications can add allocations to the start block. In particular, we
  // need to add stack allocas for copies when structs are passed or returned by
  // value.
  RAlloc* ralloc = NULL;
  for (Blk* block = fn->start->link; block; block = block->link) {
    lower_args_for_block(fn, block, &ralloc);
  }
  lower_args_for_block(fn, fn->start, &ralloc);

  fprintf(stderr, "-------- AFTER amd64_winabi_abis:\n");
  printfn(fn, stderr);

#if 0
  /* lower parameters */
  for (b = fn->start, i = b->ins; i < &b->ins[b->nins]; i++) {
    if (!ispar(i->op)) {
      break;
    }
  }
  fa = selpar(fn, b->ins, i);
  n = b->nins - (i - b->ins) + (&insb[NIns] - curi);
  i0 = alloc(n * sizeof(Ins));
  ip = icpy(ip = i0, curi, &insb[NIns] - curi);
  ip = icpy(ip, i, &b->ins[b->nins] - i);
  b->nins = n;
  b->ins = i0;
#endif
#if 0

	/* lower calls, returns, and vararg instructions */
	ral = 0;
	b = fn->start;
	do {
		if (!(b = b->link))
			b = fn->start; /* do it last */
		if (b->visit)
			continue;
		curi = &insb[NIns];
		selret(b, fn);
		for (i=&b->ins[b->nins]; i!=b->ins;)
			switch ((--i)->op) {
			default:
				emiti(*i);
				break;
			case Ocall:
				for (i0=i; i0>b->ins; i0--)
					if (!isarg((i0-1)->op))
						break;
				selcall(fn, i0, i, &ral);
				i = i0;
				break;
			case Ovastart:
				selvastart(fn, fa, i->arg[0]);
				break;
			case Ovaarg:
				selvaarg(fn, b, i);
				break;
			case Oarg:
			case Oargc:
				die("unreachable");
			}
		if (b == fn->start)
			for (; ral; ral=ral->link)
				emiti(ral->i);
		b->nins = &insb[NIns] - curi;
		idup(&b->ins, curi, b->nins);
	} while (b != fn->start);
#endif

  if (debug['A']) {
    fprintf(stderr, "\n> After ABI lowering:\n");
    printfn(fn, stderr);
  }
}
