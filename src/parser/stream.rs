/// `InputStream` - Converts physical lines to logical Fortran lines
///
/// This module handles:
/// - Joining line continuations (lines ending with &)
/// - Separating comments from code
/// - OMP conditional directives (!$)
/// - Multiline strings (tracking string state across lines)
use std::io::{BufRead, BufReader};

use anyhow::{bail, Result};

use super::char_filter::{comment_start, CharFilter, StringDelimiter};
use super::patterns::OMP_COND_RE;

/// Maximum number of physical lines allowed in a single logical line.
/// This prevents memory exhaustion from pathological inputs with many continuations.
const MAX_CONTINUATION_LINES: usize = 10_000;

/// A logical Fortran line with associated metadata
#[derive(Debug, Clone)]
pub struct FortranLine {
    /// The logical Fortran line (continuations joined, before semicolons split it)
    pub joined_line: String,
    /// Comments from each physical line
    pub comments: Vec<String>,
    /// The original physical lines
    pub lines: Vec<String>,
    /// OMP conditional prefix (e.g., "!$ ") that was stripped during parsing
    pub omp_prefix: String,
    /// Index of the line containing a semicolon (if any) within continuations
    /// Lines AFTER this index should use minimal indent (indent=1)
    pub semicolon_line_index: Option<usize>,
    /// For each physical line, whether it starts inside a string (multiline string continuation)
    pub lines_in_string: Vec<bool>,
}

/// `InputStream` reads logical Fortran lines from a reader
///
/// Handles line continuations and semicolon splitting
pub struct InputStream<R: BufRead> {
    reader: R,
    line_number: usize,
    /// Track string state across lines for multiline string support
    string_state: StringDelimiter,
}

impl<R: BufRead> InputStream<R> {
    /// Create a new `InputStream`
    ///
    /// # Arguments
    /// * `reader` - The underlying reader
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            line_number: 0,
            string_state: StringDelimiter::None,
        }
    }

    /// Read and normalize the next physical line.
    ///
    /// Converts tabs to spaces, strips the line ending, prepends `&` to lines
    /// that continue a multiline string, and splits off any OMP conditional
    /// prefix. Returns `(line, omp_prefix)`, or None at EOF.
    fn read_physical_line(&mut self) -> Result<Option<(String, String)>> {
        let mut raw_line = String::new();
        if self.reader.read_line(&mut raw_line)? == 0 {
            return Ok(None);
        }
        self.line_number += 1;

        // Convert tabs to 8 spaces
        raw_line = raw_line.replace('\t', "        ");
        // Remove trailing newline
        if raw_line.ends_with('\n') {
            raw_line.pop();
            if raw_line.ends_with('\r') {
                raw_line.pop();
            }
        }

        // Multiline string support: if we're inside a string and the line
        // doesn't start with &, prepend & to continue the string
        if self.string_state != StringDelimiter::None && !raw_line.trim_start().starts_with('&') {
            raw_line = format!("&{raw_line}");
        }

        // Check for OMP conditional prefix and strip it temporarily
        let omp_prefix = if let Some(caps) = OMP_COND_RE.captures(&raw_line) {
            let prefix = caps
                .get(1)
                .map(|m| m.as_str().to_string())
                .unwrap_or_default();
            // Remove the OMP prefix from the line
            raw_line = OMP_COND_RE.replace(&raw_line, "").to_string();
            prefix
        } else {
            String::new()
        };

        Ok(Some((raw_line, omp_prefix)))
    }

    /// Read the next logical Fortran line
    ///
    /// Returns None at EOF
    pub fn next_fortran_line(&mut self) -> Result<Option<FortranLine>> {
        // Read lines and handle continuations
        let mut lines = Vec::new();
        let mut comments = Vec::new();
        let mut line_parts = Vec::new();
        let mut first_omp_prefix = String::new();
        let mut lines_in_string = Vec::new(); // Track which lines start inside a string

        // Reset string state for each new logical line
        self.string_state = StringDelimiter::None;

        loop {
            let Some((mut line, what_omp)) = self.read_physical_line()? else {
                // EOF
                if lines.is_empty() {
                    return Ok(None);
                }
                break;
            };

            // Remember the OMP prefix from the first line
            if lines.is_empty() {
                first_omp_prefix.clone_from(&what_omp);
            }

            // Split off comment
            let (code, comment) = split_comment(&line);
            let code_str = code.to_string();
            comments.push(comment.to_string());
            line = code_str;

            // Track whether this line starts inside a string (before processing)
            // This is used to skip whitespace formatting for lines inside multiline strings
            let starts_in_string = self.string_state != StringDelimiter::None;
            lines_in_string.push(starts_in_string);

            // Track string state for multiline string support
            let mut state_tracker = CharFilter::with_string_state(
                &line,
                false, // don't filter comments (already split off)
                false, // don't filter strings (we want to track state)
                true,  // filter fypp
                self.string_state,
            );
            // Consume all characters to update string state
            for _ in state_tracker.by_ref() {}
            self.string_state = state_tracker.get_string_state();

            // Note: We do NOT split semicolons here because:
            // 1. Whitespace formatting already handles semicolon spacing (whitespace_flags[0])
            // 2. Splitting would produce multiple lines instead of one formatted line

            lines.push(what_omp.clone() + &line);

            // Guard against memory exhaustion from pathological inputs
            if lines.len() > MAX_CONTINUATION_LINES {
                bail!(
                    "line {} exceeds maximum continuation lines ({})",
                    self.line_number,
                    MAX_CONTINUATION_LINES
                );
            }

            // Check for continuation
            // Use explicit & at end of line - even if inside a string, & at EOL is a continuation
            let has_explicit_continuation = line.trim_end().ends_with('&');
            let is_comment_only = line.trim().is_empty(); // Code part is empty (comment was split)

            // If we're in a continuation context and this line is comment-only,
            // keep reading to find the actual continuation content
            let in_continuation = !line_parts.is_empty();

            if is_comment_only && in_continuation {
                // Comment-only line in the middle of a continuation
                // Don't add to line_parts (it has no code), just keep the comment
                // and continue reading for the actual continuation content
                continue;
            }

            // Check if this line is a fypp directive (starts with #:, $:, @:, or #!)
            // Fypp directives within Fortran continuations should:
            // 1. Be preserved in output (already added to `lines`)
            // 2. NOT break the Fortran continuation (continue reading)
            // 3. NOT contribute to the joined Fortran logical line
            let trimmed_line = line.trim_start();
            let is_fypp_directive = trimmed_line.starts_with("#:")
                || trimmed_line.starts_with("$:")
                || trimmed_line.starts_with("@:")
                || trimmed_line.starts_with("#!");

            if is_fypp_directive && in_continuation {
                // Fypp directive in the middle of a Fortran continuation
                // Don't add to line_parts (not Fortran code), but continue reading
                // The line is already in `lines` for output preservation
                continue;
            }

            if !has_explicit_continuation {
                // No explicit continuation - done reading this logical line
                if !is_comment_only {
                    line_parts.push(line.clone());
                }
                break;
            }

            // Has explicit continuation - remove the & and trailing whitespace
            let trimmed = line.trim_end();
            line_parts.push(trimmed.trim_end_matches('&').trim_end().to_string());
        }

        // Join the logical line (trim continuation lines)
        let joined_line = if line_parts.is_empty() {
            // Empty/comment-only line - no code content
            String::new()
        } else if line_parts.len() == 1 {
            // Single line - don't trim
            line_parts[0].clone()
        } else {
            // Multiple lines - trim continuation parts
            let mut result = line_parts[0].clone();
            for part in &line_parts[1..] {
                result.push(' ');
                result.push_str(part.trim());
            }
            result
        };

        // Find if and where a semicolon appears in the continuation lines.
        // This is used by the indenter to apply minimal indent only to lines AFTER the semicolon.
        let semicolon_line_index = if line_parts.len() > 1 {
            // Find the first line (except the last) that contains a semicolon
            line_parts[..line_parts.len() - 1]
                .iter()
                .position(|part| has_semicolon_outside_strings(part))
        } else {
            None
        };

        Ok(Some(FortranLine {
            joined_line,
            comments,
            lines,
            omp_prefix: first_omp_prefix,
            semicolon_line_index,
            lines_in_string,
        }))
    }
}

/// Split code and comment parts
fn split_comment(line: &str) -> (&str, &str) {
    if let Some(pos) = comment_start(line) {
        (&line[..pos], &line[pos..])
    } else {
        (line, "")
    }
}

/// Check if line contains a semicolon outside of strings
fn has_semicolon_outside_strings(line: &str) -> bool {
    for (_, c) in CharFilter::new(line, false, true, true) {
        if c == ';' {
            return true;
        }
    }
    false
}

/// Helper to create `InputStream` from a string (for testing)
impl<'a> InputStream<BufReader<&'a [u8]>> {
    #[must_use]
    pub fn from_string(s: &'a str) -> Self {
        let reader = BufReader::new(s.as_bytes());
        Self::new(reader)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_line() {
        let input = "x = 5\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5");
        assert_eq!(fortran_line.lines.len(), 1);

        assert!(stream.next_fortran_line().unwrap().is_none());
    }

    #[test]
    fn test_continuation() {
        let input = "x = &\n5\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5");
        assert_eq!(fortran_line.lines.len(), 2);
    }

    #[test]
    fn test_multiple_continuations() {
        let input = "x = &\n  1 + &\n  2\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 1 + 2");
        assert_eq!(fortran_line.lines.len(), 3);
    }

    #[test]
    fn test_comment() {
        let input = "x = 5 ! this is a comment\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5 ");
        assert_eq!(fortran_line.comments[0], "! this is a comment");
    }

    #[test]
    fn test_semicolon() {
        // Semicolon-separated statements stay on one line (not split in InputStream)
        let input = "x = 5; y = 10\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5; y = 10");

        // No second line - semicolons don't split in InputStream
        assert!(stream.next_fortran_line().unwrap().is_none());
    }

    #[test]
    fn test_semicolon_in_string() {
        let input = "x = \"a;b\"\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, r#"x = "a;b""#);

        // Should be only one line (semicolon in string shouldn't split)
        assert!(stream.next_fortran_line().unwrap().is_none());
    }

    #[test]
    fn test_continuation_with_comment() {
        let input = "x = & ! comment\n5\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5");
        assert_eq!(fortran_line.comments[0], "! comment");
    }

    #[test]
    fn test_empty_file() {
        let input = "";
        let mut stream = InputStream::from_string(input);
        assert!(stream.next_fortran_line().unwrap().is_none());
    }

    #[test]
    fn test_omp_conditional() {
        // OMP conditional: "!$ " prefix should be stripped and stored
        let input = "!$ x = 5\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5");
        assert_eq!(fortran_line.omp_prefix, "!$ ");
    }

    #[test]
    fn test_omp_conditional_with_leading_space() {
        let input = "  !$ x = 5\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5");
        assert_eq!(fortran_line.omp_prefix, "!$ ");
    }

    #[test]
    fn test_no_omp_prefix() {
        let input = "x = 5\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5");
        assert_eq!(fortran_line.omp_prefix, "");
    }

    #[test]
    fn test_omp_conditional_continuation() {
        // OMP conditional with line continuation
        let input = "!$ x = &\n!$ 5\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5");
        assert_eq!(fortran_line.omp_prefix, "!$ ");
        // Original lines should include the OMP prefix
        assert!(fortran_line.lines[0].starts_with("!$ "));
    }

    #[test]
    fn test_omp_semicolon_no_split() {
        // OMP conditional with semicolon - stays together on one line
        let input = "!$ x = 5; y = 10\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line, "x = 5; y = 10");
        assert_eq!(fortran_line.omp_prefix, "!$ ");

        // No second line - semicolons don't split
        assert!(stream.next_fortran_line().unwrap().is_none());
    }

    #[test]
    fn test_multiline_string_no_continuation() {
        // Without explicit &, lines are separate
        let input = "x = \"hello\nworld\"\n";
        let mut stream = InputStream::from_string(input);

        // First line is just "x = \"hello"
        let fortran_line1 = stream.next_fortran_line().unwrap().unwrap();
        assert!(fortran_line1.joined_line.contains("hello"));
        assert!(!fortran_line1.joined_line.contains("world")); // world is on separate line
        assert_eq!(fortran_line1.lines.len(), 1);

        // Second line is separate
        let fortran_line2 = stream.next_fortran_line().unwrap().unwrap();
        assert!(fortran_line2.joined_line.contains("world"));
    }

    #[test]
    fn test_multiline_string_with_continuation() {
        // With explicit continuation, lines ARE joined
        let input = "x = \"hello &\nworld\"\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        // Should work with explicit continuation
        assert!(fortran_line.joined_line.contains("hello"));
        assert!(fortran_line.joined_line.contains("world"));
    }

    #[test]
    fn test_string_no_false_continuation() {
        // Complete strings should not cause continuation
        let input = "x = 'test'\ny = 5\n";
        let mut stream = InputStream::from_string(input);

        // First line - complete string
        let fortran_line1 = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line1.joined_line, "x = 'test'");

        // Second line - should be separate
        let fortran_line2 = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line2.joined_line, "y = 5");
    }
}

#[cfg(test)]
mod continuation_tests {
    use super::*;

    #[test]
    fn test_continuation_with_comment_in_middle() {
        // Test that comment-only lines in the middle of continuations are handled correctly
        let input = "end &\n! comment\ndo\n";
        let mut stream = InputStream::from_string(input);

        let fortran_line = stream.next_fortran_line().unwrap().unwrap();
        assert_eq!(fortran_line.joined_line.trim(), "end do");
        assert_eq!(fortran_line.lines.len(), 3);
        assert_eq!(fortran_line.comments[1], "! comment");
    }
}

#[cfg(test)]
mod multiline_string_tests {
    use super::*;

    /// Verify that `InputStream` prepends exactly one & to lines inside multiline strings.
    /// This is important for Fortran continuation inside strings.
    #[test]
    fn test_multiline_string_single_ampersand() {
        let input = "write  ( * ,  * )  \"a&\nstring\"\n";

        let mut stream = InputStream::from_string(input);
        let fortran_line = stream.next_fortran_line().unwrap().unwrap();

        // Line 2 should have exactly ONE leading & (prepended by InputStream)
        assert!(
            fortran_line.lines[1].starts_with('&'),
            "Line 2 should start with &"
        );
        assert!(
            !fortran_line.lines[1].starts_with("&&"),
            "Line 2 should NOT start with &&"
        );

        // Verify lines_in_string is properly set
        assert_eq!(fortran_line.lines_in_string, vec![false, true]);
    }
}
