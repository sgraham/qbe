@echo off
setlocal enabledelayedexpansion

call "cl.exe" ^
    /nologo /Zi abi.c alias.c cfg.c copy.c emit.c fold.c getopt.c live.c load.c main.c mem.c parse.c rega.c simpl.c spill.c ssa.c util.c ^
    arm64/arm64_abi.c arm64/arm64_emit.c arm64/arm64_isel.c arm64/arm64_targ.c ^
    amd64/amd64_emit.c amd64/amd64_isel.c amd64/amd64_sysv.c amd64/amd64_targ.c amd64/amd64_winabi.c ^
    rv64/rv64_abi.c rv64/rv64_emit.c rv64/rv64_isel.c rv64/rv64_targ.c ^
    /link /out:sqbe.exe || exit /b

::set FN=test/vararg2.ssa
::call python tools/test.py %FN%
::sqbe -dPMNCFAILSR -t amd64_win %FN% 2> dump.txt

::exit /b

call python tools/test.py test/abi1.ssa
call python tools/test.py test/abi2.ssa
call python tools/test.py test/abi3.ssa
call python tools/test.py test/abi4.ssa
::call python tools/test.py test/abi5.ssa
call python tools/test.py test/abi6.ssa
call python tools/test.py test/abi7.ssa
::call python tools/test.py test/abi8.ssa
call python tools/test.py test/alias1.ssa
call python tools/test.py test/align.ssa
call python tools/test.py test/cmp1.ssa
call python tools/test.py test/collatz.ssa
::call python tools/test.py test/conaddr.ssa
call python tools/test.py test/cprime.ssa
call python tools/test.py test/cup.ssa
::call python tools/test.py test/dark.ssa
call python tools/test.py test/double.ssa
call python tools/test.py test/dynalloc.ssa
call python tools/test.py test/echo.ssa
::call python tools/test.py test/env.ssa
call python tools/test.py test/eucl.ssa
call python tools/test.py test/euclc.ssa
call python tools/test.py test/fixarg.ssa
call python tools/test.py test/fold1.ssa
call python tools/test.py test/fpcnv.ssa
call python tools/test.py test/isel1.ssa
call python tools/test.py test/isel2.ssa
call python tools/test.py test/isel3.ssa
call python tools/test.py test/isel4.ssa
call python tools/test.py test/isel5.ssa
call python tools/test.py test/ldbits.ssa
call python tools/test.py test/ldhoist.ssa
call python tools/test.py test/load1.ssa
call python tools/test.py test/load2.ssa
call python tools/test.py test/load3.ssa
call python tools/test.py test/loop.ssa
call python tools/test.py test/mandel.ssa
call python tools/test.py test/max.ssa
call python tools/test.py test/mem1.ssa
::call python tools/test.py test/mem2.ssa
::call python tools/test.py test/mem3.ssa
call python tools/test.py test/philv.ssa
call python tools/test.py test/prime.ssa
call python tools/test.py test/puts10.ssa
call python tools/test.py test/queen.ssa
call python tools/test.py test/rega1.ssa
call python tools/test.py test/spill1.ssa
call python tools/test.py test/strcmp.ssa
call python tools/test.py test/strspn.ssa
call python tools/test.py test/sum.ssa
::call python tools/test.py test/tls.ssa
call python tools/test.py test/vararg1.ssa
call python tools/test.py test/vararg2.ssa
