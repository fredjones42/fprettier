"""Idempotence and content sweep over line lengths and option sets.

usage: sweep.py NEW_EXE OLD_EXE FILE...
For each file, length and option set: format twice with NEW and report when
the second run differs, when the non-blank content changed (ignoring `&`),
and how many over-limit lines NEW and OLD leave.
"""
import shutil
import subprocess
import sys

new, old, files = sys.argv[1], sys.argv[2], sys.argv[3:]
new = shutil.which(new) or new
old = old if old == "-" else shutil.which(old) or old
LENGTHS = [40, 50, 60, 72, 80, 100, 132]
OPTS = [[], ["--no-indent"], ["--strict-indent"], ["-i", "2"], ["-w", "4"],
        ["--no-whitespace"], ["--no-indent-mod"]]


def run(exe, args, text):
    r = subprocess.run([exe, *args, "-"], input=text, capture_output=True,
                       text=True, encoding="utf-8")
    return r.stdout.replace("\r\n", "\n")


def content(text):
    return "".join(ch for ch in text if not ch.isspace() and ch != "&")


def over(text, limit):
    return sum(len(l) > limit for l in text.split("\n"))


unstable = lost = 0
tot_new = tot_old = unstable_old = 0
for f in files:
    src = open(f, encoding="utf-8").read().replace("\r\n", "\n")
    for l in LENGTHS:
        for o in OPTS:
            args = ["-l", str(l), *o]
            a = run(new, args, src)
            b = run(new, args, a)
            oa = run(old, args, src) if old != "-" else ""
            unstable_old += old != "-" and oa != run(old, args, oa)
            tot_new += over(a, l)
            tot_old += over(oa, l)
            if a != b:
                unstable += 1
                print(f"UNSTABLE {f} {args}", flush=True)
            if content(a).lower() != content(src).lower():
                lost += 1
                print(f"CONTENT  {f} {args}")
runs = len(files) * len(LENGTHS) * len(OPTS)
print(f"{runs} runs: {unstable} unstable (old: {unstable_old}), {lost} content changes")
print(f"over-limit lines: new {tot_new}, old {tot_old}")
