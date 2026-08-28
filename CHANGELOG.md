# Changelog

All notable changes to fprettier are recorded here.

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
