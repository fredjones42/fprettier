//! Fortran source code formatting.
//!
//! This module contains the core formatting logic organized into submodules:
//! - [`indenter`]: Manages indentation levels based on Fortran scope (IF/DO/MODULE/etc.)
//! - [`aligner`]: Handles continuation line alignment relative to opening delimiters
//! - [`whitespace`]: Applies whitespace rules around operators and punctuation
//! - [`case_convert`]: Converts case of keywords, procedures, operators, and type suffixes
//! - [`continuation`]: Processes line continuations and manual alignment markers
//! - [`line_split`]: Splits long lines and detaches inline comments
//! - [`replacements`]: Converts between Fortran (.lt.) and C-style (<) operators

pub mod aligner;
pub mod case_convert;
pub mod continuation;
pub mod indenter;
pub mod line_split;
pub mod replacements;
pub mod whitespace;

pub use aligner::F90Aligner;
pub use case_convert::{convert_case, CaseSettings};
pub use continuation::{
    get_manual_alignment, prepend_ampersands, remove_pre_ampersands, should_auto_align,
    PreAmpersandResult,
};
pub use indenter::{F90Indenter, IndentParams};
pub use line_split::{
    auto_split_line, find_split_position, split_inline_comment, split_long_lines,
};
pub use replacements::replace_relational_operators;
pub use whitespace::{format_line, format_line_with_level};
