@echo off

call "cl.exe" ^
    /nologo /Zi abi.c alias.c cfg.c copy.c emit.c fold.c getopt.c live.c load.c main.c mem.c parse.c rega.c simpl.c spill.c ssa.c util.c ^
    arm64/arm64_abi.c arm64/arm64_emit.c arm64/arm64_isel.c arm64/arm64_targ.c ^
    amd64/amd64_emit.c amd64/amd64_isel.c amd64/amd64_sysv.c amd64/amd64_targ.c amd64/amd64_winabi.c ^
    rv64/rv64_abi.c rv64/rv64_emit.c rv64/rv64_isel.c rv64/rv64_targ.c ^
    /link /out:qbe.exe &&^
call python test.py test/puts10.ssa
