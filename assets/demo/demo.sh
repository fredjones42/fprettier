#!/usr/bin/env bash
# Regenerates the terminal recording in ../sort-use.gif.
# Run it under a terminal recorder, e.g.  t-rec -o sort-use ./demo.sh
set -euo pipefail
cd "$(dirname "$0")"
export PATH="$(cd ../.. && pwd)/target/release:$PATH"

# Type $1 out one character at a time, then run it.
run() {
  printf '$ '
  while IFS= read -rN1 c; do printf '%s' "$c"; sleep .04; done <<<"$1"
  eval "$1"
  sleep "${PAUSE:-2}"
}

sleep 1
run 'cat solver.f90'
run 'fprettier --diff --sort-use --sort-use-only solver.f90'
sleep 2
