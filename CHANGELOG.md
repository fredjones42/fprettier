# Changelog

All notable changes to fprettier are recorded here.

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
