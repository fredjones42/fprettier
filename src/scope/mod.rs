//! Fortran scope tracking and indentation control.
//!
//! This module tracks nested Fortran constructs to determine proper indentation:
//! - Block constructs: IF/THEN, DO, SELECT CASE, WHERE, FORALL
//! - Program units: MODULE, PROGRAM, SUBMODULE, BLOCK DATA
//! - Subprograms: SUBROUTINE, FUNCTION, INTERFACE
//! - Type definitions: TYPE, ENUM, ASSOCIATE, BLOCK, CRITICAL, CHANGE TEAM
//! - Preprocessor: Fypp directives (#:if, #:for, #:def, etc.)
//!
//! The [`ScopeParser`] uses regex patterns to detect scope-opening and scope-closing
//! statements, while [`ScopeType`] categorizes each scope for indentation rules.

pub mod parser_builder;
pub mod types;

pub use parser_builder::{build_scope_parser, ParserRe, ScopeParser};
pub use types::ScopeType;
