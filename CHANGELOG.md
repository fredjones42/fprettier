# Changelog

All notable changes to fprettier are recorded here.

## Unreleased

**Bug Fixes:**
- Fixed a line split to fit the line length moving on the second run when the statement already had continuation lines. The fragment broken off took the column of the line it came from, or of the line below it, rather than the one the aligner gives it, so a break inside a bracket was re-aligned on the next run, and the lines after it with it: `3, 4), 4) + &` came out at the statement's indent and went under the open bracket a run later. Each fragment now goes where the aligner puts it, and the lines after a break are re-aligned at once, a comment on a line of its own among them
- Fixed the lines of a statement losing their indentation when one of them was split and indentation is off (`--no-indent`)
- Fixed a fragment split off a line taking the column of a fypp or cpp directive that follows it inside the statement, so `arg3, &` came out at column 1 above `#:if c>d` and was aligned a run later
- Fixed a comment detached from an over-long line by the indentation pass being put in that line's column and moved on the next run. It now goes where a comment on a line of its own belongs: in the continuation column if the statement goes on, else at the scope's indent, which is one level in when the line opens a scope (`if (i <= 2) then ! comment`). This showed with `--no-whitespace`, and whenever a line fit as written but not once indented
- Fixed a `*` or `/` that opens a continuation line being closed up when multiply/divide spacing is on, so `/ c` under `r = (a - b) &` became `/c`. A line split before the operator therefore lost its blank on the next run. The operand on the line above now counts, as it does for `+` and `-`
- Fixed a trailing blank after the closing `/` of a DATA value list when multiply/divide spacing is on

## Version 0.5.6

**Bug Fixes:**
- Fixed `--enable-replacements` needing two runs to settle when the spacing rules treat the two spellings differently: with relational spacing off, `--c-relations` turned `a .lt. b` into `a < b` on the first run and `a<b` on the second. Operators are now converted before the whitespace pass rather than after it, which also replaces the padding added in 0.5.5
- Fixed a fypp or cpp directive between the lines of a continued statement having its operators converted along with the statement's, so `#:if c > d` became `#:if c .gt. d`, which is not Python
- Fixed a continuation line with a leading `&` getting no whitespace formatting when indentation is off (`--no-indent`), so `& .and. c<=d` was left as written. fprettify formats it
- Fixed a keyword argument on a continuation line with a leading `&` being spaced as an assignment, so `call f(a, &` followed by `& k=3)` became `& k = 3)`. The bracket opened on the first line was not carried over
- Fixed the blank before a fypp `#!` comment that follows code being removed, so `x = 1 #! note` became `x = 1#! note`
- Fixed a line losing its indent in the whitespace pass when it opens with anything but a letter or digit, such as `(`, `[`, `-`, `==`, `.and.` or a fypp `#!` comment. The indent pass put this right, so it showed only with `--no-indent`, where a `#!` comment also moved left a column on every run

## Version 0.5.5

**Bug Fixes:**
- Fixed `/=` being split into `/ =` when multiply/divide spacing is on (`-w 3` and up, or `multdiv = true`), so `a /= 0` became `a / =0`, which does not compile. The `/` was spaced as a division before the relational operator was seen (#5)
- Fixed a relational operator converted to its dotted form by `--enable-replacements` being left without blanks when relational spacing is off, as in `b == 1` becoming `b.eq.1`. Replacement runs after the whitespace pass, which had closed up the `==`; the next run then spaced it. It now gets its blanks at once (#6)
- Fixed operator case being applied to the operands of a dotted operator written without blanks, so `myVar.lt.1` became `MYVAR.LT.1`, while `1.eq.b` was not converted at all (#6)

Thanks to Vikas Sharma for reporting both issues, with their causes.

## Version 0.5.4

**Bug Fixes:**
- Fixed an indented NAMELIST with a leading-`&` continuation, such as `    namelist /g5/ a, &` followed by `    & /g6/ b`, having that continuation pushed right on every run. The namelist rule collapsed runs of spaces across the whole line, the indent included, so the continuation was measured against a first line at column 1. The indent is now left alone

## Version 0.5.3

**Bug Fixes:**
- Fixed a namelist group losing its spaces when it opens a continuation line, as in `namelist /g1/ a, &` followed by `/g2/b`. As with `only:` in 0.5.2, the rule looked for `namelist` on that line rather than on the statement. The single `is_use` flag the callers pass became a statement kind covering both

## Version 0.5.2

**Bug Fixes:**
- Fixed `only:` losing its space when it opens a continuation line of a USE statement, as in `use iso_c_binding, &` followed by `only:c_int`. Each continuation line is formatted on its own, and the `only:` rule looked for `use` on that line rather than on the statement

## Version 0.5.1

**Bug Fixes:**
- Fixed a continued declaration whose initializer is an array constructor with a type-spec, such as `x(3) = [character(len=7) :: 'a', &`, aligning its continuation past the constructor's `::` instead of the `[`. That `::` was taken for the declaration's own, and the push left the alignment stack out of step with the closing `]`
- Fixed continuation lines of a statement after a semicolon being forced to column 1 (as fprettify does), so `case (1); t = x + &` left its continuation unindented. They are now aligned like any other continuation
- Fixed a fypp `#!` comment on a directive line being split off when the line was over the length limit, leaving a bare `#` that fypp rejects. A comment on a preprocessor line now always stays on it
- Fixed a comment detached from an over-long unindented line being left at column 0. It is now indented with its code
- Fixed a lopsided blank before an operator whose spacing rule is off, such as `a(i,j) +b` at `-w 0`. Output at the default whitespace level is unchanged

**Other:**
- Added a regression test pinning that indentation inspection reads the output of the whitespace pass
- Corrected the parser module documentation, which said semicolon-separated statements are split into separate logical lines
- Added the book *Learn fprettier: A Guide for Developers* in `book/`

## Version 0.5.0

**Fortran 2023 coverage:**
- Brought the keyword and intrinsic tables up to Fortran 2023
- `CRITICAL`, `CHANGE TEAM` and `ENUMERATION TYPE` now open an indented scope, as does the `BLOCK DATA` program unit
- Recognized construct names on `ELSE`, `ELSE IF`, `CASE`, `CASE DEFAULT`, `TYPE IS`, `CLASS IS`, `CLASS DEFAULT`, `RANK` and `ASSOCIATE`, and on named enums; five patterns stopped at end-of-statement, so the constructs went unindented
- Recognized parameterized derived types and defined-I/O interfaces
- Spaced the `?` and `:` of an F2023 conditional expression, which separate a scalar-logical-expr the same way a relational operator does
- Spaced user-defined operators (R1004/R1024) like the intrinsic dotted ones, so `n = 1 .plus. 2` no longer comes out as `n = 1.plus.2`, which reads as the literal `1.` followed by a name

**New Features:**
- Renamed `--whitespace-assignment` to `--whitespace-assignments`, matching its `whitespace_dict` key and every other `--whitespace-*` flag. The old spelling still works
- Added warnings for input that already exceeds the free source form limits: 10 000 characters per line (F2023 6.3.2.1) and 1 000 000 per statement (6.3.2.6). Each offense is reported on stderr with its line number, and the file is still formatted
- `--line-length` is now rejected above 10 000, the most free source form permits

**Bug Fixes:**
- Fixed a labeled `DO` not closing at its terminating label, including the shared-terminator case
- Fixed an `END` that closes nothing popping the scope stack anyway, so everything after it was under-indented
- Fixed `IF` being stacked against a `CRITICAL`, a named `ASSOCIATE` or a parameterized derived type
- Fixed the assignment alignment column running away: an `=` on every continuation line pushed another, and each one added the last
- Fixed a long statement being left on one line when its tail could not be broken, discarding the breaks already found
- Fixed the fragments of a split line landing in different columns
- Fixed a statement's length being bounded by its continuation count rather than its length
- Fixed tabs inside a character literal being expanded
- Fixed a space being inserted before the parenthesis of a defined-I/O generic spec
- Fixed relational operator replacement rewriting operators inside comments. This was unreachable through the formatter, which splits comments off first, but `replace_relational_operators` is public

**Other:**
- Added a CI workflow gating pushes and pull requests on the test suite, clippy, rustfmt, rustdoc and the MSRV
- Moved the revision history out of the README into this file

## Version 0.4.0

**New Features:**
- Added `--sort-use` and `--sort-use-only`, off by default: sort `use` statements alphabetically within a group, and sort the names in a `use ... only:` list
- Added `--check`: list files that would be reformatted and exit 1 if any (for CI)
- Added `--diff`/`-d`: print a unified diff of formatting changes instead of modifying files
- Added `--case-types` and the matching `! fprettier: --case-types` directive, controlling the case of `kind=` values, literal kind suffixes (`2_int64`) and exponent letters (`1.0e3`). This category was previously reachable only through a `[case_dict]` table in `fprettier.toml`
- Documented the configuration surface in the README: config file discovery and precedence, every `fprettier.toml` key, the whitespace levels and their per-rule overrides, the case categories, directives, and the `!&` deactivation markers

**Bug Fixes:**
- Fixed formatting not converging for multi-line fypp directives: continuation lines shifted further right on every run, so formatting the same file twice gave different output
- Fixed a panic when standard output is a closed pipe, such as `fprettier --check -r . | head`. This affected `--diff` and `--stdout` as well
- Fixed signed exponents (`1.0e-3`, `2.5d+8`) not being case-converted, while unsigned ones were

**Other:**
- Requires Rust 1.87 or later, now declared as `rust-version` in `Cargo.toml`

## Version 0.3.0

**Breaking Changes:**
- Default indentation level changed from 3 to 4 spaces. To preserve the previous behavior, set `indent = 3` in `fprettier.toml` or pass `-i 3` on the command line.

## Version 0.2.0

**New Features:**
- Added support for C preprocessor directives (`#if`, `#ifdef`, `#endif`, etc.) -- preprocessor lines are preserved without formatting and indentation is handled correctly within preprocessor blocks

**Bug Fixes:**
- Fixed alignment in the presence of statement labels
- Fixed FORMAT statement continuation alignment
- Fixed leading `+`/`-` on continuation lines being incorrectly treated as binary operators instead of unary
- Fixed spacing around `.NOT.` operator
- Fixed spacing of SELECT TYPE/CASE statements
- Fixed indentation of one-line `do`/`end do` constructs
- Fixed OpenMP (`!$OMP`) continuation line handling
- Fixed spacing after comma before concatenation operator (`//`)
- Fixed indentation with pre-ampersand continuation style
- Fixed `END IF` recognition after semicolon on same line
- Various other whitespace and spacing improvements

## Version 0.1.0

- Initial release with core formatting capabilities:
  - Indentation normalization
  - Keyword case conversion
  - Whitespace normalization around operators
  - Line continuation handling
  - Alignment of trailing comments and inline assignments
  - TOML configuration file support
  - In-file directive overrides (`! fprettier:`)
  - Parallel processing via Rayon for large codebases
  - Fypp preprocessor directive support
