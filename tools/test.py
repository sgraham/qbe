#!/usr/bin/env python3

import glob
import os
import subprocess
import sys


def extract(what, contents):
    state = "BEFORE"
    result = []
    for line in contents.splitlines():
        if line.startswith("# >>> %s" % what):
            state = "IN"
            continue
        elif state == "IN" and line.startswith("# <<<"):
            result.append("")
            return result
        if state == "IN":
            trimmed = line[2:] if line[0] == "#" and line[1] == " " else line
            if trimmed:
                trimmed = trimmed[:-1] if trimmed[-1] == "#" else trimmed
            result.append(trimmed)
    return None


def skip(contents, target):
    lines = contents.splitlines()
    if lines and lines[0].startswith("# skip") and lines[0].find(target) >= 0:
        return True
    return False


def test(fn):
    with open(fn, "r") as f:
        contents = f.read()
    print("%s... " % fn, end="")
    output = extract("output", contents)
    driver = extract("driver", contents)

    if sys.platform == "win32":
        target = "amd64_win"
    elif sys.platform == "linux":
        target = "amd64_sysv"
    else:
        port

    if skip(contents, target):
        print("skip")
        return

    subprocess.run(["./sqbe.exe", "-t", target, "-o", "tmp.s", fn], check=True)
    to_build = ["tmp.s"]
    if driver:
        with open("driver.c", "w", newline="\n") as f:
            f.write("\n".join(driver))
        to_build.append("driver.c")

    subprocess.run(["clang", "-g", "-o", "tmp.exe"] + to_build, check=True)
    proc = subprocess.run(
        ["./tmp.exe", "a", "b", "c"],
        capture_output=True,
        universal_newlines=True,
        check=True,
    )
    if output:
        out = proc.stdout
        wanted = "\n".join(output)
        if out == wanted:
            print("ok")
        else:
            print("--- FAILED")
            print("GOT:")
            print("'''%s'''" % out)
            print("WANTED:")
            print("'''%s'''" % wanted)
    else:
        print("ok")


def main():
    if len(sys.argv) < 2:
        for fn in glob.glob("test/*.ssa"):
            if os.path.split(fn)[1].startswith("_"):
                continue
            test(fn)
    else:
        test(sys.argv[1])


if __name__ == "__main__":
    main()
