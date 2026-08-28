//! Relational operator replacement for Fortran code
//!
//! Converts between Fortran-style (.lt., .le., .gt., .ge., .eq., .ne.)
//! and C-style (<, <=, >, >=, ==, /=) relational operators.

use crate::parser::char_filter::CharFilter;

/// The six relational operators, as Fortran-style and C-style spellings.
///
/// Ordered so that a longer C form is tried before a shorter one that
/// prefixes it: `<=` must match before `<`, or `a <= b` becomes `a .lt.= b`.
const OPERATORS: [(&str, &str); 6] = [
    (".le.", "<="),
    (".ge.", ">="),
    (".eq.", "=="),
    (".ne.", "/="),
    (".lt.", "<"),
    (".gt.", ">"),
];

/// Pointer assignment, which is not a relational operator. Without skipping it
/// the `>` rule would rewrite `p => t` to `p =.gt. t`.
const POINTER_ASSIGNMENT: &str = "=>";

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
    // Byte positions outside string literals, the only ones safe to rewrite.
    // Comments are deliberately not excluded here: this reproduces what
    // get_safe_positions did, which rewrites operators inside comments too.
    let mut safe = vec![false; line.len()];
    for (pos, _) in CharFilter::code_and_comments(line) {
        safe[pos] = true;
    }

    let mut result = String::with_capacity(line.len() + 16);
    let mut i = 0;
    'next: while i < line.len() {
        let rest = &line.as_bytes()[i..];
        if safe[i] {
            if !use_c_style && rest.starts_with(POINTER_ASSIGNMENT.as_bytes()) {
                result.push_str(POINTER_ASSIGNMENT);
                i += POINTER_ASSIGNMENT.len();
                continue;
            }
            for (fortran, c_style) in OPERATORS {
                let (from, to) = if use_c_style {
                    (fortran, c_style)
                } else {
                    (c_style, fortran)
                };
                // Fortran spellings are case-insensitive; the C ones have no case
                if rest.len() >= from.len()
                    && rest[..from.len()].eq_ignore_ascii_case(from.as_bytes())
                {
                    result.push_str(to);
                    i += from.len();
                    continue 'next;
                }
            }
        }

        // Not an operator, or not in code: copy it across untouched
        let ch = line[i..].chars().next().unwrap_or_default();
        result.push(ch);
        i += ch.len_utf8();
    }

    result
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
    fn test_round_trip_through_both_directions() {
        // The two directions are one table read in either order, so they have
        // to be inverses for every operator, in code but not in strings
        let fortran = "if (a .lt. b .and. c(1) .ge. 'x .gt. y') p => q";
        let c_style = replace_relational_operators(fortran, true);
        assert_eq!(c_style, "if (a < b .and. c(1) >= 'x .gt. y') p => q");
        assert_eq!(replace_relational_operators(&c_style, false), fortran);
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
