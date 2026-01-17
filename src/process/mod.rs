//! File processing and formatting pipeline.
//!
//! This module orchestrates the two-pass formatting process:
//!
//! **Pass 1 - Analysis:**
//! - Parse the input into logical lines (joining continuations)
//! - Track scope changes (IF/DO/MODULE/etc.) to determine indentation
//! - Identify manual alignment markers and deactivation directives
//!
//! **Pass 2 - Formatting:**
//! - Apply indentation based on scope depth
//! - Format whitespace around operators and punctuation
//! - Align continuation lines relative to opening delimiters
//! - Split long lines and convert case as configured
//!
//! The main entry point is [`format_file`] which processes a buffered reader
//! and writes formatted output to any `Write` implementation.

pub mod pipeline;

pub use pipeline::format_file;
