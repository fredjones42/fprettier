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

## Configuration

Settings come from three places. Each overrides the one before it:

1. `fprettier.toml` files
2. command-line flags
3. an `! fprettier:` directive in the file being formatted

### Config files

fprettier looks for `fprettier.toml` in your home directory, then in every
directory from the filesystem root down to the file being formatted. Files
found later win, so a `fprettier.toml` beside the source overrides one at the
top of the repository. Only the keys a file actually sets are applied.
`--config PATH` uses one file and skips the search.

```toml
indent = 4                      # spaces per indent level
line_length = 132               # maximum line length
whitespace = 2                  # whitespace level, 0-4 (see below)
impose_indent = true            # apply indentation
impose_whitespace = true        # apply whitespace formatting
strict_indent = true            # re-indent IF/DO even when already aligned
indent_fypp = true              # indent fypp preprocessor blocks
indent_mod = true               # indent module/program/submodule bodies
format_decl = false             # normalize spacing around ::
comment_spacing = 1             # spaces before an inline comment
normalize_comment_spacing = false   # apply comment_spacing everywhere
enable_replacements = false     # convert between .lt. and < style operators
c_relations = false             # with enable_replacements, prefer < over .lt.
```

### Whitespace levels

`whitespace` selects a preset: 0 adds no spacing, 1 covers commas,
assignments, relational and logical operators, `print`/`read` statements,
intrinsic calls and `::`; 2 adds `+` and `-`; 3 adds `*` and `/`; 4 adds `%`
and `//`. The default is 2.

Override individual rules with `[whitespace_dict]`, which wins over the level:

```toml
whitespace = 2

[whitespace_dict]
comma = true            # after , and ;
assignments = true      # around = and =>
relational = true       # around <, >, ==, /=, .eq., ...
logical = true          # around .and., .or., ...
plusminus = true        # around binary + and -
multdiv = false         # around binary * and /
print = true            # in print/read statements
type = false            # around the % component selector
intrinsics = true       # before intrinsic call parentheses
decl = true             # around ::
concat = false          # around //
```

Each key has a matching flag, for example `--whitespace-multdiv=true`.

### Case conversion

Each category takes `0` (leave alone), `1` (lowercase) or `2` (uppercase).
Nothing is converted by default.

```toml
[case_dict]
keywords = 1        # if, then, subroutine, integer, ...
procedures = 1      # intrinsics called as functions: sin(x), size(a)
operators = 1       # .and., .eq., .true., ...
constants = 1       # iso_fortran_env and iso_c_binding names: output_unit, c_int
types = 2           # kind= values, literal kind suffixes and exponent letters
```

`types` is narrower than its name suggests. It covers `real(kind=dp)`,
the suffix in `2_int64`, and the exponent letter in `1.0e-3` — not derived
type names, and not `real(dp)` written without `kind=`:

```fortran
real(kind=dp) :: a = 2.5e-8_real64    ! types = 2
real(kind=DP) :: a = 2.5E-8_REAL64
```

On the command line, the first four are one flag and `types` is its own,
because a fifth value would be read as a file name:

```sh
fprettier --case 1 1 1 1 --case-types 2 -r src/
```

### In-file directives

A comment anywhere in a file overrides everything else for that file:

```fortran
! fprettier: --indent 2 --line-length 100
! fprettier: --case 1 1 1 1 --case-types 2
```

Directives accept `-i`/`--indent`, `-l`/`--line-length`, `-w`/`--whitespace`,
`--case`, `--case-types`, `--no-indent`, `--enable-indent`, `--no-whitespace`
and `--enable-whitespace`. Only the first directive in a file is used.

### Turning formatting off

`!&` at the end of a line leaves that line alone. `!&<` and `!&>` mark a block
to leave alone:

```fortran
matrix = [1, 0, &   !&<
          0, 1]     !&>
```

## Revision History

### Version 0.4.0

**New Features:**
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
