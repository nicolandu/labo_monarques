#!/usr/bin/env python3
import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path

SRC = Path("tex")
FIGS_NAME = "figures"
FIGS = Path(FIGS_NAME)
TMP = Path("tmp")
OUT = Path("out")
OUTNAME_MAGIC = r"%filename "
OUTNAME_FMT = r"{}.pdf"

my_env = os.environ.copy()
my_env["TEXINPUTS"] = f"{FIGS.resolve()}{os.pathsep}{TMP.resolve()}{os.pathsep}"


def clean(silent=False):
    try:
        shutil.rmtree(TMP)
        print(f"    [INFO] Deleted '{TMP}'")
    except Exception as e:
        if not silent:
            print(f"    [WARN] Failed to delete '{TMP}': {e}")


def compile(name):
    src_file = SRC / f"{name}.tex"
    tmp_file = TMP / f"{name}.tex"

    if not src_file.exists():
        print(f"    [ERROR] '{src_file}' not found.")
        sys.exit(1)

    shutil.copytree(SRC, TMP)
    shutil.copytree(FIGS, TMP / FIGS_NAME)

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
        subprocess.run(cmd, cwd=TMP, check=True, shell=False, env=my_env)

    src = TMP / f"{name}.pdf"
    dst = OUT / outname
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    shutil.move(src, dst)  # Seems to be more atomic than os.replace
    print(f"    [INFO] Moved '{src}' -> '{dst}'")


def main():
    parser = argparse.ArgumentParser(description="Compile LaTeX project.")
    parser.add_argument("file", help="LaTeX project to compile.")

    args = parser.parse_args()

    clean(silent=True)
    try:
        compile(args.file)
    except subprocess.CalledProcessError as e:
        print("    [ERROR] Command '", " ".join(e.cmd), f"' failed.", sep="")
        clean()
        sys.exit(1)

    clean()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("    [INFO] Ctrl-C received, aborting.")
        clean()
        sys.exit(1)
