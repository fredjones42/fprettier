//! fprettier - Auto-formatter for modern Fortran source code
//!
//! A Fortran code formatter with high-performance parallelization.

#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::struct_excessive_bools)]

pub mod cli;
pub mod config;
pub mod directive;
pub mod error;
pub mod format;
pub mod parser;
pub mod process;
pub mod scope;

// Re-export commonly used types
pub use cli::{build_cli, parse_args, parse_args_from, CliArgs};
pub use config::Config;
pub use directive::{find_directive, parse_directive, DirectiveOverrides};
pub use error::Result;
