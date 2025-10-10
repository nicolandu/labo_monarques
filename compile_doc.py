#!/usr/bin/env python3
import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path

SRC = Path("tex")
FIGS = "figures"
TMP = Path("tmp")
OUT = Path("out")
OUTNAME_MAGIC = r"%filename "
OUTNAME_FMT = r"{}.pdf"


def clean():
    try:
        shutil.rmtree(TMP)
        print(f"    [INFO] Deleted '{TMP}'")
    except Exception as e:
        print(f"    [WARN] Failed to delete '{TMP}': {e}")


def compile(name):
    src_file = SRC / f"{name}.tex"
    tmp_file = TMP / f"{name}.tex"

    if not src_file.exists():
        print(f"    [ERROR] '{src_file}' not found.")
        sys.exit(1)

    shutil.copytree(SRC, TMP)
    shutil.copytree(Path(FIGS), TMP / FIGS)

    with open(tmp_file, "r") as f:
        outname = f.readline()
    if outname.startswith(OUTNAME_MAGIC):
        outname = outname.lstrip(OUTNAME_MAGIC).rstrip()
    else:
        outname = name

    outname = OUTNAME_FMT.format(outname)

    commands = [
        ["lualatex", "-interaction=nonstopmode", tmp_file.name],
        ["biber", "--input-directory", SRC.resolve(), name],
        ["lualatex", "-interaction=nonstopmode", tmp_file.name],
        ["lualatex", "-interaction=nonstopmode", tmp_file.name],
    ]

    for cmd in commands:
        try:
            subprocess.run(cmd, cwd=TMP, check=True, shell=False)
        except subprocess.CalledProcessError:
            print("    [ERROR] Command '", " ".join(cmd), f"' failed.", sep="")
            sys.exit(1)

    src = TMP / f"{name}.pdf"
    dst = OUT / outname
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    shutil.move(src, dst)  # Seems to be more atomic than os.replace
    print(f"    [INFO] Moved '{src}' -> '{dst}'")


def main():
    error = False
    parser = argparse.ArgumentParser(description="Compile LaTeX project.")
    parser.add_argument("file", help="LaTeX project to compile.")
    parser.add_argument("--noclean", action="store_true", help="Do not clean up after compilation.")
    parser.add_argument("--clean", action="store_true", help="Only clean without compiling.")

    args = parser.parse_args()

    if args.clean and (args.file or args.noclean):
        parser.error("--clean should be used standalone.")
    elif args.clean:
        clean()
        print("    [INFO] Cleaned without compiling, exiting.")
        sys.exit(0)
    else:
        name = args.file

    clean()
    compile(name)

    if not args.noclean:
        clean()
    if error:
        print("    [WARN] Job done with errors.")
    else:
        print("    [INFO] Job done, no error.")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("    [INFO] Ctrl-C received, aborting.")
        sys.exit(1)
