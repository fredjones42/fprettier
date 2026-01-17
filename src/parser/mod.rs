//! Fortran source code parsing utilities.
//!
//! This module provides the infrastructure for reading and tokenizing Fortran source:
//! - [`CharFilter`]: Iterator adapter that identifies strings, comments, and code regions
//! - [`InputStream`]: Joins continuation lines (`&`) and splits semicolon-separated statements
//! - [`patterns`]: Precompiled regex patterns for Fortran syntax elements
//!
//! The parser handles Fortran's line-continuation semantics, string literals (both
//! single and double quoted), and inline comments while preserving the original
//! structure for accurate reformatting.

pub mod char_filter;
pub mod patterns;
pub mod stream;

pub use char_filter::{CharFilter, StringDelimiter};
pub use stream::{FortranLine, InputStream};
