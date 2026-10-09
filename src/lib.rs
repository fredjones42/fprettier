//! fprettier - Auto-formatter for modern Fortran source code
//!
//! A Fortran code formatter with high-performance parallelization.

pub mod cli;
pub mod config;
pub mod directive;
pub mod format;
pub mod parser;
pub mod process;
pub mod scope;

// Re-export commonly used types
pub use cli::{CliArgs, build_cli, parse_args};
pub use config::Config;
pub use directive::{DirectiveOverrides, find_directive, parse_directive};
