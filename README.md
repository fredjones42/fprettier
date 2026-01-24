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

## Revision History

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
