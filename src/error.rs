//! Error types and result aliases for fprettier.
//!
//! This module defines the error handling infrastructure:
//! - [`Result<T>`]: Type alias for `anyhow::Result<T>` used throughout the crate

use anyhow::Result as AnyhowResult;

pub type Result<T> = AnyhowResult<T>;
