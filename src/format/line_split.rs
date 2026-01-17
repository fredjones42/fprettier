//! Line splitting for respecting line length limits
//!
//! Implements automatic line breaking for long Fortran lines,
//! inserting continuation markers (&) at appropriate positions.

use crate::parser::CharFilter;

/// Check if a line has a comment (!) outside of strings.
/// This is a simple state machine that tracks string delimiters.
fn has_comment_outside_string(text: &str) -> bool {
    let mut in_single_quote = false;
    let mut in_double_quote = false;

    for c in text.chars() {
        match c {
            '\'' if !in_double_quote => in_single_quote = !in_single_quote,
            '"' if !in_single_quote => in_double_quote = !in_double_quote,
            '!' if !in_single_quote && !in_double_quote => return true,
            _ => {}
        }
    }
    false
}

/// Split a line into (code, comment) parts if it contains a detachable inline comment.
/// Returns None if there's no inline comment or if the split isn't valid.
///
/// # Arguments
/// * `line` - The line to split
///
/// # Returns
/// * `Some((code_line, comment_line))` if the line can be split
/// * `None` if there's no inline comment or the split isn't valid
#[must_use]
pub fn split_inline_comment(line: &str) -> Option<(String, String)> {
    if !line.contains('!') {
        return None;
    }

    let has_newline = line.ends_with('\n');
    let body = if has_newline {
        &line[..line.len() - 1]
    } else {
        line
    };

    // Find the position of '!' outside of strings
    let mut comment_pos = None;
    let mut in_single_quote = false;
    let mut in_double_quote = false;

    for (i, c) in body.char_indices() {
        match c {
            '\'' if !in_double_quote => in_single_quote = !in_single_quote,
            '"' if !in_single_quote => in_double_quote = !in_double_quote,
            '!' if !in_single_quote && !in_double_quote => {
                comment_pos = Some(i);
                break;
            }
            _ => {}
        }
    }

    let comment_pos = comment_pos?;

    let code = body[..comment_pos].trim_end();
    let comment = body[comment_pos..].trim_start();

    if code.is_empty() || comment.is_empty() {
        return None;
    }

    let (code_line, comment_line) = if has_newline {
        (format!("{code}\n"), format!("{comment}\n"))
    } else {
        (code.to_string(), comment.to_string())
    };

    Some((code_line, comment_line))
}

/// Find a suitable breakpoint (prefer whitespace or comma) within `max_width`.
/// Returns None if no such breakpoint exists.
///
/// Uses `CharFilter` to avoid splitting inside strings or comments.
#[must_use]
pub fn find_split_position(text: &str, max_width: usize) -> Option<usize> {
    if max_width < 1 {
        return None;
    }

    let search_limit = std::cmp::min(text.len().saturating_sub(1), max_width);
    if search_limit == 0 && text.len() > 1 {
        return None;
    }

    let mut spaces: Vec<usize> = Vec::new();
    let mut commas: Vec<usize> = Vec::new();

    // Use CharFilter to skip strings and comments
    for (pos, char) in CharFilter::new(text, true, true, true) {
        if pos > search_limit {
            break;
        }
        if char == ' ' {
            spaces.push(pos);
        } else if char == ',' {
            commas.push(pos);
        }
    }

    // Prefer whitespace breakpoints (search from right to left)
    for candidate in spaces.iter().rev() {
        // Ensure remaining text is long enough (at least 12 chars)
        if text.len() - candidate >= 12 {
            return Some(*candidate);
        }
    }

    // Fall back to comma breakpoints
    for candidate in commas.iter().rev() {
        // Ensure remaining text is long enough (at least 4 chars)
        if text.len() - candidate > 4 {
            return Some(candidate + 1); // Break after the comma
        }
    }

    None
}

/// Attempt to split a long logical line into continuation lines that
/// respect the configured line-length limit.
///
/// Returns a list of new line fragments when successful, otherwise None.
///
/// # Arguments
/// * `line` - The line to split
/// * `initial_indent` - Current indentation level in characters
/// * `max_line_length` - Maximum line length
/// * `indent_size` - Size of indent for continuation lines
#[must_use]
pub fn auto_split_line(
    line: &str,
    initial_indent: usize,
    max_line_length: usize,
    indent_size: usize,
) -> Option<Vec<String>> {
    // Don't try to split very short lines
    if max_line_length < 40 {
        return None;
    }

    let stripped = line.trim_start();
    if stripped.is_empty() {
        return None;
    }

    // Skip if line already starts with continuation
    if stripped.starts_with('&') {
        return None;
    }

    let line_has_newline = stripped.ends_with('\n');
    let stripped = if line_has_newline {
        &stripped[..stripped.len() - 1]
    } else {
        stripped
    };

    // Check for inline comments - don't split lines with comments
    // We need to find '!' that's not inside a string
    let has_comment = has_comment_outside_string(stripped);
    if has_comment {
        return None;
    }

    // Calculate available space for first chunk
    // Reserve 2 chars for trailing " &"
    let max_first = if max_line_length > initial_indent + 2 {
        max_line_length - initial_indent - 2
    } else {
        return None;
    };

    if max_first == 0 {
        return None;
    }

    // Find split position
    let break_pos = find_split_position(stripped, max_first)?;
    if break_pos >= stripped.len() {
        return None;
    }

    let remainder = stripped[break_pos..].trim_start();
    if remainder.is_empty() {
        return None;
    }

    let first_chunk = stripped[..break_pos].trim_end();
    let mut new_lines = vec![format!("{} &", first_chunk)];

    let current_indent = initial_indent + indent_size;
    let mut current = remainder;

    while !current.is_empty() {
        let available = if max_line_length > current_indent {
            max_line_length - current_indent
        } else {
            return None;
        };

        if available == 0 {
            return None;
        }

        // Final chunk (fits without ampersand)
        if current.len() + 2 <= available {
            new_lines.push(current.to_string());
            break;
        }

        // Need to split further
        let split_limit = if available > 2 {
            available - 2
        } else {
            return None;
        };

        let cont_break = find_split_position(current, split_limit)?;
        // Prevent infinite loop: if we can't make progress, give up
        if cont_break == 0 || cont_break >= current.len() {
            return None;
        }

        let chunk = current[..cont_break].trim_end();
        if chunk.is_empty() {
            return None;
        }

        new_lines.push(format!("{chunk} &"));
        current = current[cont_break..].trim_start();
    }

    // Restore newlines if original had them
    if line_has_newline {
        new_lines = new_lines
            .into_iter()
            .map(|s| {
                let s = s.trim_end_matches('\n');
                format!("{s}\n")
            })
            .collect();
    }

    Some(new_lines)
}

/// Apply line splitting to an array of lines, replacing long lines with split versions.
///
/// Returns a tuple of:
/// - The new lines
/// - Their indentation levels
/// - Original line index for each result line (for proper comment placement)
#[must_use]
pub fn split_long_lines(
    lines: &[String],
    indents: &[usize],
    max_line_length: usize,
    indent_size: usize,
) -> (Vec<String>, Vec<usize>, Vec<usize>) {
    let mut result_lines = Vec::new();
    let mut result_indents = Vec::new();
    let mut result_origins = Vec::new(); // Track which original line each result came from

    for (i, line) in lines.iter().enumerate() {
        let indent = if i < indents.len() { indents[i] } else { 0 };

        // Calculate actual visual line length (including strings)
        // Don't filter strings - they contribute to visual line length
        let line_length = line.trim_end_matches('\n').len();

        // Check if line needs splitting
        if line_length > max_line_length {
            // First, try to detach inline comment if present
            // This may make the code short enough, or allow it to be split
            if let Some((code_line, comment_line)) = split_inline_comment(line) {
                let code_length = code_line.trim_end_matches('\n').len();

                // Check if code part still needs splitting
                if code_length > max_line_length {
                    if let Some(split_lines) =
                        auto_split_line(&code_line, indent, max_line_length, indent_size)
                    {
                        // Add first line with original indent
                        result_lines.push(split_lines[0].clone());
                        result_indents.push(indent);
                        result_origins.push(i);

                        // Add continuation lines with follow indent
                        let follow_indent = indent + indent_size;
                        for split_line in split_lines.iter().skip(1) {
                            result_lines.push(split_line.clone());
                            result_indents.push(follow_indent);
                            result_origins.push(i);
                        }

                        // Add detached comment with original indent
                        result_lines.push(comment_line);
                        result_indents.push(indent);
                        result_origins.push(i);
                        continue;
                    }
                }

                // Code fits without splitting, just detach the comment
                result_lines.push(code_line);
                result_indents.push(indent);
                result_origins.push(i);
                result_lines.push(comment_line);
                result_indents.push(indent);
                result_origins.push(i);
                continue;
            }

            // No comment to detach, try regular splitting
            if let Some(split_lines) = auto_split_line(line, indent, max_line_length, indent_size) {
                // Add first line with original indent
                result_lines.push(split_lines[0].clone());
                result_indents.push(indent);
                result_origins.push(i);

                // Add continuation lines with follow indent
                let follow_indent = indent + indent_size;
                for split_line in split_lines.iter().skip(1) {
                    result_lines.push(split_line.clone());
                    result_indents.push(follow_indent);
                    result_origins.push(i);
                }
                continue;
            }
        }

        // Line doesn't need splitting or couldn't be split
        result_lines.push(line.clone());
        result_indents.push(indent);
        result_origins.push(i);
    }

    (result_lines, result_indents, result_origins)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_split_position_whitespace() {
        // Should find the last space before limit
        let text = "call foo(a, b, c, d, e, f, g, h, i, j)";
        let pos = find_split_position(text, 20);
        assert!(pos.is_some());
        let pos = pos.unwrap();
        assert!(pos <= 20);
        assert_eq!(&text[pos..=pos], " ");
    }

    #[test]
    fn test_find_split_position_comma() {
        // Should find comma when no suitable space
        let text = "call foo(aaaaaaaaaaaaaaaa,bbbbbbbbbbbbbbbb)";
        let pos = find_split_position(text, 30);
        // Should break after the comma
        assert!(pos.is_some());
    }

    #[test]
    fn test_find_split_position_none() {
        // No suitable split point
        let text = "abcdefghijklmnopqrstuvwxyz";
        let pos = find_split_position(text, 10);
        assert!(pos.is_none());
    }

    #[test]
    fn test_auto_split_line_basic() {
        let line = "call very_long_subroutine_name(arg1, arg2, arg3, arg4, arg5, arg6, arg7)";
        let result = auto_split_line(line, 0, 50, 3);
        assert!(result.is_some());
        let lines = result.unwrap();
        assert!(lines.len() >= 2);
        assert!(lines[0].ends_with(" &"));
    }

    #[test]
    fn test_auto_split_line_with_indent() {
        let line = "   x = a + b + c + d + e + f + g + h + i + j + k + l + m + n + o";
        let result = auto_split_line(line, 3, 40, 3);
        assert!(result.is_some());
    }

    #[test]
    fn test_auto_split_line_too_short() {
        let line = "x = 1";
        let result = auto_split_line(line, 0, 132, 3);
        assert!(result.is_none());
    }

    #[test]
    fn test_auto_split_line_with_comment() {
        // Lines with comments should not be split
        let line = "x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 ! comment";
        let result = auto_split_line(line, 0, 40, 3);
        assert!(result.is_none());
    }

    #[test]
    fn test_has_comment_outside_string() {
        // Basic comment detection
        assert!(has_comment_outside_string("x = 1 ! comment"));
        assert!(has_comment_outside_string("call foo() ! end of line"));

        // No comment
        assert!(!has_comment_outside_string("x = 1"));
        assert!(!has_comment_outside_string("call foo()"));

        // Comment marker inside string should be ignored
        assert!(!has_comment_outside_string("x = '!' "));
        assert!(!has_comment_outside_string(r#"x = "!" "#));
        assert!(!has_comment_outside_string("print *, 'Hello! World'"));
    }

    #[test]
    fn test_split_long_lines() {
        let lines = vec![
            "short line".to_string(),
            "this is a very very very very very very very very long line that needs splitting"
                .to_string(),
        ];
        let indents = vec![0, 0];

        let (result_lines, result_indents, result_origins) =
            split_long_lines(&lines, &indents, 50, 3);

        // First line should be unchanged
        assert_eq!(result_lines[0], "short line");
        assert_eq!(result_indents[0], 0);
        assert_eq!(result_origins[0], 0);

        // Long line should be split
        assert!(result_lines.len() > 2);
        // All split lines should have origin 1
        for origin in &result_origins[1..] {
            assert_eq!(*origin, 1);
        }
    }

    #[test]
    fn test_split_long_lines_comment_detach_and_split() {
        // Line with comment that also needs splitting after comment removal
        let lines = vec![
            "            if (foo_bar_identifier .and. bar_baz_identifier) print *, long_identifier, another_long_identifier ! note\n".to_string(),
        ];
        let indents = vec![12];

        let (result_lines, result_indents, result_origins) =
            split_long_lines(&lines, &indents, 72, 4);

        // Should have 3 lines: split code (2) + comment (1)
        assert_eq!(result_lines.len(), 3, "Expected 3 lines after split");

        // First line: code with & continuation
        assert!(
            result_lines[0].contains(" &"),
            "First line should end with &"
        );
        assert_eq!(result_indents[0], 12, "First line indent should be 12");
        assert_eq!(result_origins[0], 0, "First line origin should be 0");

        // Second line: continuation
        assert_eq!(result_indents[1], 16, "Continuation indent should be 16");
        assert_eq!(result_origins[1], 0, "Second line origin should be 0");

        // Third line: comment
        assert!(result_lines[2].starts_with('!') || result_lines[2].trim_start().starts_with('!'));
        assert_eq!(result_indents[2], 12, "Comment indent should be 12");
        assert_eq!(result_origins[2], 0, "Comment origin should be 0");
    }
}
