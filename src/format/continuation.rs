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
#[must_use]
pub fn remove_pre_ampersands(lines: &[String], is_special: &[bool]) -> PreAmpersandResult {
    let mut result_lines = Vec::with_capacity(lines.len());
    let mut pre_ampersand = Vec::with_capacity(lines.len());
    let mut ampersand_sep = Vec::with_capacity(lines.len());

    for (pos, line) in lines.iter().enumerate() {
        // A special line (fypp, or inside a multiline string) is reproduced
        // verbatim, so a leading & on it is content, not a continuation
        // marker: it is neither captured nor stripped.
        let is_line_special = is_special.get(pos).copied().unwrap_or(false);
        let leading_amp = if is_line_special {
            None
        } else {
            PRE_AMPERSAND_RE.captures(line)
        };

        // The "& " or "&  " to put back in front of this line
        pre_ampersand.push(
            leading_amp
                .as_ref()
                .map_or_else(String::new, |caps| caps[1].to_string()),
        );

        if pos > 0 {
            // How much space the previous line left before its trailing &, so
            // that spacing survives the round trip. Only meaningful when this
            // line has a leading & to pair with it; one space otherwise, and
            // one too if the previous line has no trailing & to measure.
            ampersand_sep.push(
                leading_amp
                    .and_then(|_| TRAILING_AMPERSAND_RE.captures(&lines[pos - 1]))
                    .and_then(|prev| prev.get(1))
                    .map_or(1, |m| m.as_str().len()),
            );
        }

        result_lines.push(if is_line_special {
            line.clone()
        } else {
            line.trim_start_matches(' ')
                .trim_start_matches('&')
                .to_string()
        });
    }

    PreAmpersandResult {
        lines: result_lines,
        pre_ampersand,
        ampersand_sep,
    }
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
        let result = remove_pre_ampersands(&lines, &is_special);

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
