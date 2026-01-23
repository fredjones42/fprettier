//! Continuation line alignment helpers
//!
//! Handles manual alignment for lines with leading `&` continuation markers.

use crate::parser::patterns::{NO_ALIGN_RE, PRE_AMPERSAND_RE, TRAILING_AMPERSAND_RE};

/// Result of removing pre-ampersands from continuation lines
#[derive(Debug)]
pub struct PreAmpersandResult {
    /// Lines with leading & and spaces stripped
    pub lines: Vec<String>,
    /// The extracted pre-ampersand portion for each line (e.g., "& " or "&  ")
    /// Empty string if line doesn't start with &
    pub pre_ampersand: Vec<String>,
    /// Number of whitespace characters before & on the previous line
    /// Used to preserve original spacing when reformatting
    pub ampersand_sep: Vec<usize>,
}

/// Check if auto-alignment should be disabled for these lines
///
/// Returns false if any line starts with & followed by content.
/// This triggers manual alignment mode where original indents are preserved.
#[must_use]
pub fn should_auto_align(lines: &[String]) -> bool {
    !lines.iter().any(|line| NO_ALIGN_RE.is_match(line))
}

/// Extract manual indents for line continuations
///
/// Returns the relative indent for each line based on the position of
/// content after stripping leading spaces and &.
///
/// For simple continuations (where all continuation lines have the same indent),
/// normalizes to use a standard continuation offset instead of preserving the
/// original file's indentation.
#[must_use]
#[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
pub fn get_manual_alignment(lines: &[String], continuation_indent: usize) -> Vec<usize> {
    // Calculate indent as: length - length after stripping spaces and &
    let manual_lines_indent: Vec<isize> = lines
        .iter()
        .map(|l| {
            let stripped = l.trim_start_matches(' ').trim_start_matches('&');
            (l.len() - stripped.len()) as isize
        })
        .collect();

    // Make relative to first line
    let first_indent = manual_lines_indent.first().copied().unwrap_or(0);
    let mut result: Vec<usize> = manual_lines_indent
        .into_iter()
        .map(|ind| (ind - first_indent).max(0) as usize)
        .collect();

    // Normalize continuation indents ONLY for simple continuations
    // If the first continuation line (index 1) has a leading &, it's likely
    // array-style alignment that should be preserved
    // Only normalize if line[1] doesn't start with & (after trimming spaces)
    if result.len() >= 2 && result[1] > 0 && lines.len() >= 2 {
        let first_cont_has_ampersand = lines[1].trim_start_matches(' ').starts_with('&');
        if !first_cont_has_ampersand {
            // Simple continuation - normalize to continuation_indent
            let original_first_cont = result[1];
            let adjustment = continuation_indent as isize - original_first_cont as isize;
            for indent in result.iter_mut().skip(1) {
                if *indent > 0 {
                    *indent = (*indent as isize + adjustment).max(0) as usize;
                }
            }
        }
    }

    result
}

/// Remove leading ampersands from continuation lines
///
/// Extracts the leading & and trailing whitespace from each line,
/// and also captures how many spaces were before the & on the previous line.
pub fn remove_pre_ampersands(
    lines: &[String],
    is_special: &[bool],
) -> Result<PreAmpersandResult, String> {
    let mut result_lines = Vec::with_capacity(lines.len());
    let mut pre_ampersand = Vec::with_capacity(lines.len());
    let mut ampersand_sep = Vec::with_capacity(lines.len());

    for (pos, line) in lines.iter().enumerate() {
        // Check if this line is special (fypp or inside multiline string)
        // Special lines should NOT have their leading & captured or stripped,
        // because they are preserved as-is and we don't want to re-add the &
        let is_line_special = is_special.get(pos).copied().unwrap_or(false);

        if is_line_special {
            // Special line - don't capture or strip the leading &
            pre_ampersand.push(String::new());
            if pos > 0 {
                ampersand_sep.push(1); // default
            }
        } else if let Some(caps) = PRE_AMPERSAND_RE.captures(line) {
            // Line starts with & (possibly with leading spaces)
            // Capture the "& " or "&  " etc.
            let amp_match = caps.get(1).unwrap();
            pre_ampersand.push(amp_match.as_str().to_string());

            // Get amount of whitespace before & on previous line
            if pos > 0 {
                if let Some(prev_caps) = TRAILING_AMPERSAND_RE.captures(&lines[pos - 1]) {
                    let sep = prev_caps.get(1).map_or(0, |m| m.as_str().len());
                    ampersand_sep.push(sep);
                } else {
                    // Previous line should have trailing &, but doesn't match
                    // This could be a parse error, but we'll be lenient
                    ampersand_sep.push(1); // default to 1 space
                }
            }
        } else {
            pre_ampersand.push(String::new());
            if pos > 0 {
                // No leading & on this line, use default 1 space before & on previous line
                ampersand_sep.push(1);
            }
        }

        // Strip leading spaces and & from line (unless it's a special line)
        if is_line_special {
            result_lines.push(line.clone());
        } else {
            // Strip leading spaces, then strip leading &
            let stripped = line.trim_start_matches(' ').trim_start_matches('&');
            result_lines.push(stripped.to_string());
        }
    }

    Ok(PreAmpersandResult {
        lines: result_lines,
        pre_ampersand,
        ampersand_sep,
    })
}

/// Prepend ampersands back to continuation lines and adjust indent
pub fn prepend_ampersands(
    lines: &[String],
    indents: &mut [usize],
    pre_ampersand: &[String],
) -> Vec<String> {
    let mut result = Vec::with_capacity(lines.len());

    for (pos, line) in lines.iter().enumerate() {
        let amp_insert = pre_ampersand.get(pos).map_or("", String::as_str);
        if amp_insert.is_empty() {
            result.push(line.clone());
        } else {
            // Adjust indent by -1 (& takes up one column)
            if pos < indents.len() && indents[pos] > 0 {
                indents[pos] -= 1;
            }
            // Prepend the ampersand portion to the trimmed line
            result.push(format!("{}{}", amp_insert, line.trim_start()));
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_should_auto_align_no_leading_amp() {
        let lines = vec!["   x = a + &".to_string(), "       b + c".to_string()];
        assert!(should_auto_align(&lines));
    }

    #[test]
    fn test_should_auto_align_with_leading_amp() {
        let lines = vec!["   x = [1, 2, &".to_string(), "        & 3, 4]".to_string()];
        // Has leading & followed by content, so auto_align = false
        assert!(!should_auto_align(&lines));
    }

    #[test]
    fn test_get_manual_alignment() {
        let lines = vec![
            "   big_arr = [1, 2, 3, 4, 5,&".to_string(),
            "           &  6, 7, 8, 9, 10, &".to_string(),
            "           & 11, 12, 13, 14, 15,&".to_string(),
            "            &16, 17, 18, 19, 20]".to_string(),
        ];
        let manual_indent = get_manual_alignment(&lines, 4);
        // First line starts at 3 spaces (after stripping nothing for &)
        // Second line: 11 spaces + & = 12 chars stripped, relative to first (3) = 9
        // Third line: 11 spaces + & = 12 chars stripped, relative to first (3) = 9
        // Fourth line: 12 spaces + & = 13 chars stripped, relative to first (3) = 10
        // Array alignment is preserved (first continuation has leading &)
        assert_eq!(manual_indent, vec![0, 9, 9, 10]);
    }

    #[test]
    fn test_remove_pre_ampersands() {
        let lines = vec![
            "   big_arr = [1, 2, &".to_string(),
            "           &  3, 4]".to_string(),
        ];
        let is_special = vec![false, false];
        let result = remove_pre_ampersands(&lines, &is_special).unwrap();

        // First line has no leading &
        assert_eq!(result.pre_ampersand[0], "");
        // Second line has leading "&  " (& followed by 2 spaces based on regex capture)
        assert_eq!(result.pre_ampersand[1], "&  ");
        // Second line content after stripping leading spaces and &, keeps spaces after &
        // Input: "           &  3, 4]"
        // After trim_start_matches(' '): "&  3, 4]"
        // After trim_start_matches('&'): "  3, 4]"
        assert_eq!(result.lines[1], "  3, 4]");
    }

    #[test]
    fn test_prepend_ampersands() {
        let lines = vec!["big_arr = [1, 2, &".to_string(), " 3, 4]".to_string()];
        let mut indents = vec![3, 11];
        let pre_ampersand = vec![String::new(), "&  ".to_string()];

        let result = prepend_ampersands(&lines, &mut indents, &pre_ampersand);

        // First line unchanged
        assert_eq!(result[0], "big_arr = [1, 2, &");
        // Second line gets ampersand prepended
        assert_eq!(result[1], "&  3, 4]");
        // Second line indent reduced by 1
        assert_eq!(indents[1], 10);
    }
}
