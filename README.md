# fprettier

fprettier is an auto-formatter for Fortran 90+ code.

fprettier is inspired by [fprettify](https://github.com/fortran-lang/fprettify) but strives to be:

- :zap: Fast, especially for large codebases
- :wrench: Actively maintained, with prompt responses to issues

## Installation

First, install Rust and Cargo by following the instructions at:

[https://rustup.rs](https://rustup.rs)

Then install `fprettier`:

```sh
cargo install fprettier
```

## Usage

Run `fprettier` with no arguments to display the usage information.

Format files in-place:

```sh
fprettier -r src/
```

Verify formatting without modifying files — prints the files that would change
and exits non-zero if there are any, so CI needs just one step:

```sh
fprettier --check -r src/
```

Show what would change as a unified diff, without modifying files:

```sh
fprettier --diff -r src/
```

## Revision History

### Version 0.4.0

**New Features:**
- Added `--check`: list files that would be reformatted and exit 1 if any (for CI)
- Added `--diff`/`-d`: print a unified diff of formatting changes instead of modifying files

### Version 0.3.0

**Breaking Changes:**
- Default indentation level changed from 3 to 4 spaces. To preserve the previous behavior, set `indent = 3` in `fprettier.toml` or pass `-i 3` on the command line.

### Version 0.2.0

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

### Version 0.1.0

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
