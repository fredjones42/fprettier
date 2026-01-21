/// `F90Aligner` - Handles continuation line alignment
///
/// Aligns continuation lines based on:
/// 1. Bracket nesting: Align to position after `(`, `(/`, `[`
/// 2. Assignment: Align to position after `=` or `=>`
/// 3. Declaration: Align to position after `::`
/// 4. USE statement: Align to position after `:`
/// 5. Default: Use fixed relative indent
use std::sync::LazyLock;

use regex::Regex;

use crate::error::Result;
use crate::parser::patterns::{PRIVATE_RE, PUBLIC_RE, REL_OP_RE, USE_RE, VAR_DECL_RE};
use crate::parser::CharFilter;

// Regex for :: directly before line break (declaration alignment)
static DECL_BEFORE_BREAK_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"::\s*(&)[\s]*(?:!.*)?$").unwrap());
// Regex for : directly before line break (USE statement alignment)
static USE_BEFORE_BREAK_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r":\s*(&)[\s]*(?:!.*)?$").unwrap());
// Regex for = directly before line break (assignment alignment)
static ASSIGN_BEFORE_BREAK_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"=\s*(&)[\s]*(?:!.*)?$").unwrap());
// Regex for => directly before line break (pointer assignment alignment)
static PTR_ASSIGN_BEFORE_BREAK_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"=>\s*(&)[\s]*(?:!.*)?$").unwrap());
// Regex for delimiter directly before line break
static DEL_BEFORE_BREAK_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?:\(|(?:\(/)|(?:\[))\s*(&)[\s]*(?:!.*)?$").unwrap());

/// Aligner for continuation lines
pub struct F90Aligner {
    line_indents: Vec<usize>,
    level: usize,                     // Bracket nesting level
    bracket_indent_stack: Vec<usize>, // Stack of alignment positions
}

impl Default for F90Aligner {
    fn default() -> Self {
        Self::new()
    }
}

impl F90Aligner {
    /// Create a new `F90Aligner`
    #[must_use]
    pub fn new() -> Self {
        Self {
            line_indents: Vec::new(),
            level: 0,
            bracket_indent_stack: Vec::new(),
        }
    }

    /// Initialize for a new logical line
    fn init_line(&mut self) {
        self.line_indents = vec![0];
        self.level = 0;
        self.bracket_indent_stack = vec![0];
    }

    /// Process lines of a Fortran logical line
    ///
    /// `logical_line` is the joined logical line, `lines` are the physical lines,
    /// `relative_indent` is the relative indentation size
    pub fn process_logical_line(
        &mut self,
        logical_line: &str,
        lines: &[String],
        relative_indent: usize,
    ) -> Result<()> {
        self.init_line();

        // Detect line type
        let is_decl = VAR_DECL_RE.is_match(logical_line)
            || PUBLIC_RE.is_match(logical_line)
            || PRIVATE_RE.is_match(logical_line);
        let is_use = USE_RE.is_match(logical_line);

        // Process each physical line
        for (pos, line) in lines.iter().enumerate() {
            self.align_line_continuations(line, is_decl, is_use, relative_indent);

            // For continuation lines, append current alignment
            if pos + 1 < lines.len() {
                self.line_indents
                    .push(self.bracket_indent_stack.last().copied().unwrap_or(0));
            }
        }

        Ok(())
    }

    /// Align continuation lines
    fn align_line_continuations(
        &mut self,
        line: &str,
        is_decl: bool,
        is_use: bool,
        indent_size: usize,
    ) {
        // Skip fypp directive lines - they don't affect bracket alignment
        let trimmed = line.trim_start();
        if trimmed.starts_with("#:")
            || trimmed.starts_with("$:")
            || trimmed.starts_with("@:")
            || trimmed.starts_with("#!")
        {
            // Don't process this line, but alignment state is preserved for next line
            return;
        }

        let mut equals_position = 0;
        let mut left_delim_positions: Vec<usize> = Vec::new();
        let mut left_delimiters: Vec<&str> = Vec::new();

        // Get current relative indent from stack
        let relative_indent = self.bracket_indent_stack.last().copied().unwrap_or(0);

        let mut end_of_delim = None;

        // Process each character using CharFilter
        let char_filter = CharFilter::new(line, true, true, true);

        for (pos, ch) in char_filter {
            // Check for delimiters
            let (what_del_open, what_del_close) = if Some(pos) > end_of_delim {
                get_curr_delim(line, pos)
            } else {
                (None, None)
            };

            // Handle opening delimiters
            if let Some(delim) = what_del_open {
                end_of_delim = Some(pos + delim.len() - 1);
                self.level += 1;
                self.bracket_indent_stack
                    .push(pos + delim.len() + relative_indent);
                left_delim_positions.push(pos);
                left_delimiters.push(delim);
            }

            // Handle closing delimiters
            if let Some(delim) = what_del_close {
                end_of_delim = Some(pos + delim.len() - 1);

                if self.level > 0 {
                    self.level -= 1;
                    self.bracket_indent_stack.pop();
                }

                if !left_delim_positions.is_empty() {
                    left_delim_positions.pop();
                    let open_delim = left_delimiters.pop();

                    // Validate matching pairs
                    let valid = matches!(
                        (open_delim, delim),
                        (Some("("), ")") | (Some("(/"), "/)") | (Some("["), "]")
                    );

                    if !valid {
                        // Unpaired delimiters - silently continue
                        // (bracket matching is best-effort for alignment)
                    }
                }
            }

            // Handle top-level comma (removes assignment alignment)
            if ch == ',' && self.level == 0 && equals_position > 0 {
                equals_position = 0;
                self.bracket_indent_stack.pop();
            }

            // Handle assignment operator (not in brackets, not relational)
            if self.level == 0 && !is_decl && ch == '=' {
                // Check if it's a relational operator (==, /=, <=, >=)
                let is_relational = if pos > 0 && pos + 1 < line.len() {
                    let context = &line[pos.saturating_sub(1)..=(pos + 1).min(line.len() - 1)];
                    REL_OP_RE.is_match(context)
                } else {
                    false
                };

                if !is_relational {
                    let is_pointer = pos + 1 < line.len() && line.chars().nth(pos + 1) == Some('>');
                    equals_position = pos + 1;

                    // Don't align if assignment operator directly before line break
                    // Check whole line for "= + whitespace + &" pattern
                    let assign_before_break = if is_pointer {
                        PTR_ASSIGN_BEFORE_BREAK_RE.is_match(line)
                    } else {
                        ASSIGN_BEFORE_BREAK_RE.is_match(line)
                    };
                    if !assign_before_break {
                        let indent =
                            equals_position + 1 + usize::from(is_pointer) + relative_indent;
                        self.bracket_indent_stack.push(indent);
                    }
                }
            }

            // Handle declaration operator ::
            if is_decl && pos + 1 < line.len() && &line[pos..pos + 2] == "::" {
                // Don't align if :: directly before line break
                // Check whole line for ":: + whitespace + &" pattern (not just from pos)
                if !DECL_BEFORE_BREAK_RE.is_match(line) {
                    self.bracket_indent_stack.push(pos + 3 + relative_indent);
                }
            }

            // Handle USE statement :
            if is_use && ch == ':' && pos + 1 < line.len() && line.chars().nth(pos + 1) != Some(':')
            {
                // Don't align if : directly before line break
                // Check whole line for ": + whitespace + &" pattern
                if !USE_BEFORE_BREAK_RE.is_match(line) {
                    self.bracket_indent_stack.push(pos + 2 + relative_indent);
                }
            }
        }

        // Don't align if delimiter opening directly before line break
        // Pattern: DEL_OPEN + whitespace + LINEBREAK (i.e., "( &" not "(x, y, &")
        if self.level > 0 {
            // Check for pattern: opening delimiter followed directly by whitespace and &
            if DEL_BEFORE_BREAK_RE.is_match(line) {
                if self.bracket_indent_stack.len() > 1 {
                    let prev = self.bracket_indent_stack[self.bracket_indent_stack.len() - 2];
                    if let Some(last) = self.bracket_indent_stack.last_mut() {
                        *last = prev;
                    }
                } else if !self.bracket_indent_stack.is_empty() {
                    self.bracket_indent_stack[0] = 0;
                }
            }
        }

        // Use default indent if no alignment found
        if self.bracket_indent_stack.last().copied().unwrap_or(0) == 0 {
            if let Some(last) = self.bracket_indent_stack.last_mut() {
                *last = indent_size;
            }
        }
    }

    /// Get the calculated line indents
    #[must_use]
    pub fn get_lines_indent(&self) -> &[usize] {
        &self.line_indents
    }
}

/// Get delimiter token in line starting at pos, if it exists
///
/// Returns (`opening_delimiter`, `closing_delimiter`) as Option<&str>
fn get_curr_delim(line: &str, pos: usize) -> (Option<&str>, Option<&str>) {
    if pos >= line.len() {
        return (None, None);
    }

    let substr = &line[pos..];

    // Check for opening delimiters: (, (/, [
    let open = if substr.starts_with("(/") {
        Some("(/")
    } else if substr.starts_with('(') {
        Some("(")
    } else if substr.starts_with('[') {
        Some("[")
    } else {
        None
    };

    // Check for closing delimiters: ), /), ]
    let close = if substr.starts_with("/)") {
        Some("/)")
    } else if substr.starts_with(')') {
        Some(")")
    } else if substr.starts_with(']') {
        Some("]")
    } else {
        None
    };

    (open, close)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aligner_default_indent() {
        let mut aligner = F90Aligner::new();
        let lines = vec!["x = 1".to_string()];

        aligner.process_logical_line("x = 1", &lines, 3).unwrap();

        let indents = aligner.get_lines_indent();
        assert_eq!(indents.len(), 1);
        assert_eq!(indents[0], 0); // First line always gets 0
    }

    #[test]
    fn test_aligner_bracket_nesting() {
        let mut aligner = F90Aligner::new();
        let lines = vec!["call foo(a,".to_string(), "         b)".to_string()];

        aligner
            .process_logical_line("call foo(a, b)", &lines, 3)
            .unwrap();

        let indents = aligner.get_lines_indent();
        assert_eq!(indents.len(), 2);
        // Second line should align after the opening (
        assert!(indents[1] > 0);
    }

    #[test]
    fn test_aligner_assignment() {
        let mut aligner = F90Aligner::new();
        let lines = vec!["x = a +".to_string(), "    b".to_string()];

        aligner
            .process_logical_line("x = a + b", &lines, 3)
            .unwrap();

        let indents = aligner.get_lines_indent();
        assert_eq!(indents.len(), 2);
        // Should align after the = sign
        assert!(indents[1] > 0);
    }

    #[test]
    fn test_aligner_declaration() {
        let mut aligner = F90Aligner::new();
        let lines = vec!["integer :: x,".to_string(), "           y".to_string()];

        aligner
            .process_logical_line("integer :: x, y", &lines, 3)
            .unwrap();

        let indents = aligner.get_lines_indent();
        assert_eq!(indents.len(), 2);
        // Should align after the ::
        assert!(indents[1] > 0);
    }

    #[test]
    fn test_get_curr_delim() {
        assert_eq!(get_curr_delim("(test)", 0), (Some("("), None));
        assert_eq!(get_curr_delim("(test)", 5), (None, Some(")")));
        assert_eq!(get_curr_delim("(/1,2/)", 0), (Some("(/"), None));
        assert_eq!(get_curr_delim("(/1,2/)", 5), (None, Some("/)")));
        assert_eq!(get_curr_delim("[array]", 0), (Some("["), None));
        assert_eq!(get_curr_delim("[array]", 6), (None, Some("]")));
        assert_eq!(get_curr_delim("test", 0), (None, None));
    }

    #[test]
    fn test_aligner_public_declaration() {
        let mut aligner = F90Aligner::new();
        let lines = vec![
            "public :: dp, test_routine, &".to_string(),
            "test_function, test_type".to_string(),
        ];

        let logical_line = "public :: dp, test_routine, test_function, test_type";

        aligner
            .process_logical_line(logical_line, &lines, 5)
            .unwrap();

        let indents = aligner.get_lines_indent();
        assert_eq!(indents.len(), 2);
        // :: is at position 7, so alignment should be 7 + 3 + 5 = 15
        // But we're adding to base indent, so offset should be ~10
        assert!(
            indents[1] >= 10,
            "Expected indent >= 10, got {}",
            indents[1]
        );
    }

    #[test]
    fn test_aligner_format_statement() {
        // Test alignment for FORMAT statement continuation
        // The trimmed line has "format(" where "(" is at position 6 (0-indexed)
        // Alignment should be at position 7 (after the "(")
        let mut aligner = F90Aligner::new();
        let lines = vec![
            "format(A10, I5, &".to_string(),
            "F10.2)".to_string(),
        ];

        let logical_line = "format(A10, I5, F10.2)";

        aligner
            .process_logical_line(logical_line, &lines, 3)
            .unwrap();

        let indents = aligner.get_lines_indent();
        assert_eq!(indents.len(), 2);
        assert_eq!(indents[0], 0, "First line indent should be 0");
        // "(" is at position 6, so alignment should be 6 + 1 = 7
        assert_eq!(
            indents[1], 7,
            "Second line indent should be 7 (position after opening paren)"
        );
    }
}
