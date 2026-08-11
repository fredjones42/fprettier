//! Inline directive parsing for `! fprettier:` comments
//!
//! Supports in-file configuration overrides via special comments:
//! `! fprettier: --indent 4 --no-whitespace`

use std::collections::BTreeMap;
use std::sync::LazyLock;

use regex::Regex;

use crate::format::case_convert::{CaseMode, CASE_KEYS};

/// Pattern to match fprettier directives
static FPRETTIER_DIRECTIVE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)^\s*!\s*fprettier:\s*(.*)\s*$").unwrap());

/// Parsed directive options that can override config
#[derive(Debug, Default, Clone)]
pub struct DirectiveOverrides {
    pub indent: Option<usize>,
    pub line_length: Option<usize>,
    /// Whitespace level (0-4). Values > 4 are rejected by config validation.
    pub whitespace: Option<u8>,
    pub impose_indent: Option<bool>,
    pub impose_whitespace: Option<bool>,
    pub case_keywords: Option<CaseMode>,
    pub case_procedures: Option<CaseMode>,
    pub case_operators: Option<CaseMode>,
    pub case_constants: Option<CaseMode>,
    pub case_types: Option<CaseMode>,
}

impl DirectiveOverrides {
    /// Check if any overrides are set
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.indent.is_none()
            && self.line_length.is_none()
            && self.whitespace.is_none()
            && self.impose_indent.is_none()
            && self.impose_whitespace.is_none()
            && self.case_keywords.is_none()
            && self.case_procedures.is_none()
            && self.case_operators.is_none()
            && self.case_constants.is_none()
            && self.case_types.is_none()
    }

    /// Get case overrides as a `BTreeMap` (for use with `CaseSettings::from_dict`)
    #[must_use]
    pub fn get_case_dict(&self) -> BTreeMap<String, CaseMode> {
        let modes = [
            self.case_keywords,
            self.case_procedures,
            self.case_operators,
            self.case_constants,
            self.case_types,
        ];
        CASE_KEYS
            .iter()
            .zip(modes)
            .filter_map(|(key, mode)| mode.map(|m| ((*key).to_string(), m)))
            .collect()
    }
}

/// Check if a line contains an fprettier directive
#[must_use]
pub fn is_directive_line(line: &str) -> bool {
    FPRETTIER_DIRECTIVE_RE.is_match(line)
}

/// Parse an fprettier directive line and return option overrides
///
/// # Arguments
/// * `line` - The line containing the directive
///
/// # Returns
/// * `Some(DirectiveOverrides)` if the line is a valid directive
/// * `None` if the line is not a directive
#[must_use]
pub fn parse_directive(line: &str) -> Option<DirectiveOverrides> {
    let caps = FPRETTIER_DIRECTIVE_RE.captures(line)?;
    let args_str = caps.get(1)?.as_str();

    // Parse the arguments like CLI args
    parse_directive_args(args_str)
}

/// Parse directive arguments into overrides
fn parse_directive_args(args_str: &str) -> Option<DirectiveOverrides> {
    let mut overrides = DirectiveOverrides::default();
    let tokens: Vec<&str> = args_str.split_whitespace().collect();
    let mut i = 0;
    let parse_mode = |tok: &&str| -> Option<CaseMode> {
        tok.parse::<u8>()
            .ok()
            .and_then(|v| CaseMode::try_from(v).ok())
    };

    while i < tokens.len() {
        let token = tokens[i];
        match token {
            "-i" | "--indent" => {
                i += 1;
                if i < tokens.len() {
                    overrides.indent = tokens[i].parse().ok();
                }
            }
            "-l" | "--line-length" => {
                i += 1;
                if i < tokens.len() {
                    overrides.line_length = tokens[i].parse().ok();
                }
            }
            "-w" | "--whitespace" => {
                i += 1;
                if i < tokens.len() {
                    overrides.whitespace = tokens[i].parse().ok();
                }
            }
            "--no-whitespace" | "--disable-whitespace" => {
                overrides.impose_whitespace = Some(false);
            }
            "--enable-whitespace" => {
                overrides.impose_whitespace = Some(true);
            }
            "--no-indent" | "--disable-indent" => {
                overrides.impose_indent = Some(false);
            }
            "--enable-indent" => {
                overrides.impose_indent = Some(true);
            }
            "--case" => {
                // Format: --case 1 2 0 0 (keywords, procedures, operators, constants)
                // Try to read up to 4 values from subsequent tokens
                // Invalid values are ignored, consistent with the lenient directive parser
                overrides.case_keywords = tokens.get(i + 1).and_then(parse_mode);
                overrides.case_procedures = tokens.get(i + 2).and_then(parse_mode);
                overrides.case_operators = tokens.get(i + 3).and_then(parse_mode);
                overrides.case_constants = tokens.get(i + 4).and_then(parse_mode);
                // Skip the values we consumed (up to 4)
                let mut skip = 0;
                for j in 1..=4 {
                    if i + j < tokens.len() && !tokens[i + j].starts_with('-') {
                        skip = j;
                    } else {
                        break;
                    }
                }
                i += skip;
            }
            "--case-types" => {
                overrides.case_types = tokens.get(i + 1).and_then(parse_mode);
                i += 1;
            }
            _ => {
                // Unknown option, skip
            }
        }
        i += 1;
    }

    if overrides.is_empty() {
        None
    } else {
        Some(overrides)
    }
}

/// Scan input for fprettier directives and return the first found
///
/// This reads the file looking for `! fprettier:` lines.
/// Only the first directive is used (subsequent ones are ignored).
pub fn find_directive<R: std::io::BufRead>(input: &mut R) -> Option<DirectiveOverrides> {
    let mut buffer = String::new();

    while input.read_line(&mut buffer).ok()? > 0 {
        if is_directive_line(&buffer) {
            return parse_directive(&buffer);
        }
        buffer.clear();
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_directive_line() {
        assert!(is_directive_line("! fprettier: --indent 4"));
        assert!(is_directive_line("  ! fprettier: --no-whitespace"));
        assert!(is_directive_line("! FPRETTIER: --indent 2"));
        assert!(!is_directive_line("! this is a regular comment"));
        assert!(!is_directive_line("x = 1"));
    }

    #[test]
    fn test_parse_directive_indent() {
        let overrides = parse_directive("! fprettier: --indent 4").unwrap();
        assert_eq!(overrides.indent, Some(4));
    }

    #[test]
    fn test_parse_directive_line_length() {
        let overrides = parse_directive("! fprettier: -l 80").unwrap();
        assert_eq!(overrides.line_length, Some(80));
    }

    #[test]
    fn test_parse_directive_no_whitespace() {
        let overrides = parse_directive("! fprettier: --no-whitespace").unwrap();
        assert_eq!(overrides.impose_whitespace, Some(false));
    }

    #[test]
    fn test_parse_directive_multiple() {
        let overrides = parse_directive("! fprettier: --indent 2 -l 120 --no-indent").unwrap();
        assert_eq!(overrides.indent, Some(2));
        assert_eq!(overrides.line_length, Some(120));
        assert_eq!(overrides.impose_indent, Some(false));
    }

    #[test]
    fn test_parse_directive_case() {
        let overrides = parse_directive("! fprettier: --case 1 2 0 1").unwrap();
        assert_eq!(overrides.case_keywords, Some(CaseMode::Lower));
        assert_eq!(overrides.case_procedures, Some(CaseMode::Upper));
        assert_eq!(overrides.case_operators, Some(CaseMode::NoChange));
        assert_eq!(overrides.case_constants, Some(CaseMode::Lower));
    }

    #[test]
    fn test_parse_directive_case_types() {
        let overrides = parse_directive("! fprettier: --case-types 2").unwrap();
        assert_eq!(overrides.case_types, Some(CaseMode::Upper));
        assert_eq!(overrides.case_keywords, None);

        // --case leaves types alone, so the two can be combined
        let overrides = parse_directive("! fprettier: --case 1 1 1 1 --case-types 2").unwrap();
        assert_eq!(overrides.case_keywords, Some(CaseMode::Lower));
        assert_eq!(overrides.case_types, Some(CaseMode::Upper));
        assert_eq!(overrides.get_case_dict().len(), 5);
    }

    #[test]
    fn test_parse_invalid_directive() {
        // Empty directive
        let overrides = parse_directive("! fprettier:");
        assert!(overrides.is_none());
    }
}
