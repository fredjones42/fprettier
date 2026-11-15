"""Check the book's before/after listings against a fprettier build.

usage: check_book_examples.py BOOK.tex OUTDIR FPRETTIER_EXE
Writes each `before` listing to OUTDIR/<line>.f90, runs it with default
options, and reports whether the following `after` listing matches. A pair
made with other options reports DIFF; rerun OUTDIR/<line>.f90 with the options
the book gives for it.
"""
import pathlib
import shutil
import re
import subprocess
import sys

tex = pathlib.Path(sys.argv[1]).read_text(encoding="utf-8").split("\n")
out = pathlib.Path(sys.argv[2])
exe = shutil.which(sys.argv[3]) or sys.argv[3]

blocks = []  # (style, 1-based line of \begin, text)
i = 0
while i < len(tex):
    m = re.match(r"\\begin\{lstlisting\}\[style=(\w+)", tex[i])
    if m:
        j = i + 1
        while not tex[j].startswith("\\end{lstlisting}"):
            j += 1
        blocks.append((m.group(1), i + 1, "\n".join(tex[i + 1 : j]) + "\n"))
        i = j
    i += 1

last = None
for style, ln, text in blocks:
    if style == "before":
        last = ln
        (out / f"{ln}.f90").write_text(text, encoding="utf-8", newline="\n")
    elif style == "after":
        (out / f"{ln}.expected").write_text(text, encoding="utf-8", newline="\n")
        if last is None:
            print(f"after {ln}: no preceding before")
            continue
        r = subprocess.run(
            [exe, "-s", str(out / f"{last}.f90")],
            capture_output=True, text=True, encoding="utf-8",
        )
        got = r.stdout.replace("\r\n", "\n")
        print(f"before {last} -> after {ln}:", "OK" if got == text else "DIFF")
