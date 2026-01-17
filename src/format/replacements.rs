//! Relational operator replacement for Fortran code
//!
//! Converts between Fortran-style (.lt., .le., .gt., .ge., .eq., .ne.)
//! and C-style (<, <=, >, >=, ==, /=) relational operators.

use crate::parser::CharFilter;

/// Replace relational operators in a line
///
/// # Arguments
/// * `line` - The line to process
/// * `use_c_style` - If true, convert to C-style operators; if false, convert to Fortran-style
///
/// # Returns
/// The line with operators replaced
#[must_use]
pub fn replace_relational_operators(line: &str, use_c_style: bool) -> String {
    if use_c_style {
        // Convert Fortran-style to C-style
        replace_fortran_to_c(line)
    } else {
        // Convert C-style to Fortran-style
        replace_c_to_fortran(line)
    }
}

/// Convert Fortran-style operators to C-style
fn replace_fortran_to_c(line: &str) -> String {
    // Get positions that are outside strings and comments (safe to modify)
    let safe_positions = get_safe_positions(line);

    // Process character by character, looking for Fortran-style operators
    let mut result = String::with_capacity(line.len());
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        // Check if this position is safe to modify
        let byte_pos = line.char_indices().nth(i).map_or(0, |(p, _)| p);
        let is_safe = byte_pos < safe_positions.len() && safe_positions[byte_pos];

        if is_safe && chars[i] == '.' {
            // Try to match Fortran operators (case-insensitive)
            // Need at least 4 characters for .xx.
            if i + 3 < chars.len() {
                let four_chars: String = chars[i..i + 4].iter().collect();
                let four_lower = four_chars.to_lowercase();

                match four_lower.as_str() {
                    ".lt." => {
                        result.push('<');
                        i += 4;
                        continue;
                    }
                    ".gt." => {
                        result.push('>');
                        i += 4;
                        continue;
                    }
                    ".eq." => {
                        result.push_str("==");
                        i += 4;
                        continue;
                    }
                    ".ne." => {
                        result.push_str("/=");
                        i += 4;
                        continue;
                    }
                    ".le." => {
                        result.push_str("<=");
                        i += 4;
                        continue;
                    }
                    ".ge." => {
                        result.push_str(">=");
                        i += 4;
                        continue;
                    }
                    _ => {}
                }
            }
        }

        // Copy character as-is
        result.push(chars[i]);
        i += 1;
    }

    result
}

/// Convert C-style operators to Fortran-style
fn replace_c_to_fortran(line: &str) -> String {
    // Get positions that are outside strings and comments (safe to modify)
    let safe_positions = get_safe_positions(line);

    // Process character by character, looking for C-style operators
    let mut result = String::with_capacity(line.len() + 20);
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        // Check if this position is safe to modify
        let byte_pos = line.char_indices().nth(i).map_or(0, |(p, _)| p);
        let is_safe = byte_pos < safe_positions.len() && safe_positions[byte_pos];

        if is_safe {
            // Check for two-character operators first
            if i + 1 < chars.len() {
                let two_chars: String = chars[i..=i + 1].iter().collect();
                match two_chars.as_str() {
                    "<=" => {
                        result.push_str(".le.");
                        i += 2;
                        continue;
                    }
                    ">=" => {
                        result.push_str(".ge.");
                        i += 2;
                        continue;
                    }
                    "==" => {
                        result.push_str(".eq.");
                        i += 2;
                        continue;
                    }
                    "/=" => {
                        result.push_str(".ne.");
                        i += 2;
                        continue;
                    }
                    "=>" => {
                        // Pointer assignment - don't convert
                        result.push_str("=>");
                        i += 2;
                        continue;
                    }
                    _ => {}
                }
            }

            // Check for single-character operators
            match chars[i] {
                '<' => {
                    // Check it's not part of <= (already handled above)
                    result.push_str(".lt.");
                    i += 1;
                    continue;
                }
                '>' => {
                    // Check it's not part of >= or => (already handled above)
                    result.push_str(".gt.");
                    i += 1;
                    continue;
                }
                _ => {}
            }
        }

        // Copy character as-is
        result.push(chars[i]);
        i += 1;
    }

    result
}

/// Get a vector indicating which byte positions are safe to modify
/// (i.e., outside strings and comments)
fn get_safe_positions(line: &str) -> Vec<bool> {
    let mut safe = vec![false; line.len()];

    // Use CharFilter to find positions outside strings and comments
    for (pos, _) in CharFilter::new(line, false, true, true) {
        if pos < safe.len() {
            safe[pos] = true;
        }
    }

    safe
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fortran_to_c_simple() {
        let input = "if (a .lt. b) then";
        let result = replace_relational_operators(input, true);
        assert_eq!(result, "if (a < b) then");
    }

    #[test]
    fn test_fortran_to_c_all_operators() {
        let input = "a .lt. b .le. c .gt. d .ge. e .eq. f .ne. g";
        let result = replace_relational_operators(input, true);
        assert_eq!(result, "a < b <= c > d >= e == f /= g");
    }

    #[test]
    fn test_c_to_fortran_simple() {
        let input = "if (a < b) then";
        let result = replace_relational_operators(input, false);
        assert_eq!(result, "if (a .lt. b) then");
    }

    #[test]
    fn test_c_to_fortran_all_operators() {
        let input = "a < b <= c > d >= e == f /= g";
        let result = replace_relational_operators(input, false);
        assert_eq!(result, "a .lt. b .le. c .gt. d .ge. e .eq. f .ne. g");
    }

    #[test]
    fn test_case_insensitive() {
        let input = "a .LT. b .Le. c .gT. d";
        let result = replace_relational_operators(input, true);
        assert_eq!(result, "a < b <= c > d");
    }

    #[test]
    fn test_pointer_not_affected() {
        // => should not be converted to Fortran style
        let input = "ptr => target";
        let result = replace_relational_operators(input, false);
        assert_eq!(result, "ptr => target");
    }

    #[test]
    fn test_no_change_when_already_target_style() {
        // C-style to C-style should not change much
        // (Fortran operators shouldn't be present)
        let input = "if (a < b) then";
        let result = replace_relational_operators(input, true);
        assert_eq!(result, "if (a < b) then");
    }

    #[test]
    fn test_mixed_operators() {
        // Some Fortran, some C-style - convert all to C
        let input = "a .lt. b .and. c > d";
        let result = replace_relational_operators(input, true);
        assert!(result.contains('<'), "Should have <: {result}");
        assert!(result.contains('>'), "Should have >: {result}");
    }
}
