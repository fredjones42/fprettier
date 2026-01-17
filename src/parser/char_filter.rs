/// `CharFilter` - Iterator that filters out strings and comments
///
/// This is a critical component that wraps a string iterator and maintains
/// state about whether we're inside strings, comments, or fypp preprocessor
/// directives. It's used throughout the codebase to ensure we only parse
/// actual Fortran code, not string contents or comments.
use std::sync::LazyLock;

use regex::Regex;

static FYPP_OPEN_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(#\{|\$\{|@\{)").unwrap());
static FYPP_CLOSE_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\}#|\}\$|\}@)").unwrap());

/// Type of string delimiter we're currently inside
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum StringDelimiter {
    #[default]
    None,
    Single,     // '...'
    Double,     // "..."
    FyppHash,   // #{...}#
    FyppDollar, // ${...}$
    FyppAt,     // @{...}@
}

/// Iterator adapter that filters out strings and comments
///
/// Yields (position, character) pairs for only the actual Fortran code,
/// skipping over string contents and comments.
pub struct CharFilter<'a> {
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    state: FilterState,
    filter_comments: bool,
    filter_strings: bool,
    filter_fypp: bool,
}

#[derive(Debug)]
struct FilterState {
    instring: StringDelimiter,
    infypp: bool,
    incomment: bool,
}

impl Default for FilterState {
    fn default() -> Self {
        Self {
            instring: StringDelimiter::None,
            infypp: false,
            incomment: false,
        }
    }
}

impl<'a> CharFilter<'a> {
    /// Create a new `CharFilter`
    ///
    /// # Arguments
    /// * `content` - The string to iterate over
    /// * `filter_comments` - Whether to filter out comments (starting with !)
    /// * `filter_strings` - Whether to filter out string contents
    /// * `filter_fypp` - Whether to treat fypp inline blocks as strings
    #[must_use]
    pub fn new(
        content: &'a str,
        filter_comments: bool,
        filter_strings: bool,
        filter_fypp: bool,
    ) -> Self {
        Self {
            chars: content.char_indices().peekable(),
            state: FilterState::default(),
            filter_comments,
            filter_strings,
            filter_fypp,
        }
    }

    /// Check if we're currently inside a string
    #[must_use]
    pub fn instring(&self) -> bool {
        self.state.instring != StringDelimiter::None
    }

    /// Create a `CharFilter` with initial string state (for multiline strings)
    ///
    /// This is used when a string spans multiple lines and we need to start
    /// the new line already in a string context.
    #[must_use]
    pub fn with_string_state(
        content: &'a str,
        filter_comments: bool,
        filter_strings: bool,
        filter_fypp: bool,
        string_state: StringDelimiter,
    ) -> Self {
        let infypp = matches!(
            string_state,
            StringDelimiter::FyppHash | StringDelimiter::FyppDollar | StringDelimiter::FyppAt
        );
        Self {
            chars: content.char_indices().peekable(),
            state: FilterState {
                instring: string_state,
                infypp,
                incomment: false,
            },
            filter_comments,
            filter_strings,
            filter_fypp,
        }
    }

    /// Get the current string delimiter state
    ///
    /// Returns the delimiter we're currently inside, or None if not in a string.
    /// This can be used to track multiline string state across lines.
    #[must_use]
    pub fn get_string_state(&self) -> StringDelimiter {
        self.state.instring
    }

    /// Get the filtered content as a string
    ///
    /// Pre-allocates the result string based on the input size for efficiency.
    pub fn filter_all(&mut self) -> String {
        // Pre-allocate based on remaining chars (filtering only reduces size)
        let size_hint = self.chars.size_hint().0;
        let mut result = String::with_capacity(size_hint);
        for (_, c) in self.by_ref() {
            result.push(c);
        }
        result
    }

    /// Peek at the next character without consuming
    fn peek_next_char(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, c)| c)
    }
}

impl Iterator for CharFilter<'_> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        let (pos, c) = self.chars.next()?;

        // Check for comment start (only if not in string)
        if self.state.instring == StringDelimiter::None && c == '!' {
            self.state.incomment = true;
            if self.filter_comments {
                return self.next(); // Skip the ! itself
            }
        }

        // If filtering and we're in a comment, skip
        if self.filter_comments && self.state.incomment {
            return self.next();
        }

        // Track string state (always, regardless of filter_strings)
        // This is necessary for multiline string tracking
        let mut just_closed_string = false;
        if self.state.instring != StringDelimiter::None {
            // Check for string close (single or double quote)
            let is_closing_quote = !self.state.infypp
                && ((c == '\'' && self.state.instring == StringDelimiter::Single)
                    || (c == '"' && self.state.instring == StringDelimiter::Double));

            if is_closing_quote {
                self.state.instring = StringDelimiter::None;
                just_closed_string = true;
                if self.filter_strings {
                    return self.next(); // Skip the closing quote
                }
            } else if self.state.infypp {
                // Check for fypp close (need to check current + next char)
                if let Some(next_char) = self.peek_next_char() {
                    let check_str = format!("{c}{next_char}");
                    if FYPP_CLOSE_RE.is_match(&check_str) {
                        match self.state.instring {
                            StringDelimiter::FyppHash if check_str == "}#" => {
                                self.state.instring = StringDelimiter::None;
                                self.state.infypp = false;
                                just_closed_string = true;
                                self.chars.next(); // consume second char
                                if self.filter_strings {
                                    return self.next(); // Skip both closing chars
                                }
                            }
                            StringDelimiter::FyppDollar if check_str == "}$" => {
                                self.state.instring = StringDelimiter::None;
                                self.state.infypp = false;
                                just_closed_string = true;
                                self.chars.next(); // consume second char
                                if self.filter_strings {
                                    return self.next(); // Skip both closing chars
                                }
                            }
                            StringDelimiter::FyppAt if check_str == "}@" => {
                                self.state.instring = StringDelimiter::None;
                                self.state.infypp = false;
                                just_closed_string = true;
                                self.chars.next(); // consume second char
                                if self.filter_strings {
                                    return self.next(); // Skip both closing chars
                                }
                            }
                            _ => {}
                        }
                    }
                }
            } else if self.filter_strings {
                // We're inside a string and filtering, skip this character
                return self.next();
            }

            // If we're still in a string (state wasn't closed) and filtering, skip
            if self.filter_strings && self.state.instring != StringDelimiter::None {
                return self.next();
            }
        }

        // Check for string open (only if not already in string and didn't just close one)
        if self.state.instring == StringDelimiter::None && !just_closed_string {
            if c == '\'' {
                self.state.instring = StringDelimiter::Single;
                if self.filter_strings {
                    return self.next(); // Skip the opening quote
                }
            } else if c == '"' {
                self.state.instring = StringDelimiter::Double;
                if self.filter_strings {
                    return self.next(); // Skip the opening quote
                }
            } else if self.filter_fypp {
                // Check for fypp inline block open (need current + next char)
                if let Some(next_char) = self.peek_next_char() {
                    let two_chars = format!("{c}{next_char}");
                    if FYPP_OPEN_RE.is_match(&two_chars) {
                        match two_chars.as_str() {
                            "#{" => {
                                self.state.instring = StringDelimiter::FyppHash;
                                self.state.infypp = true;
                                self.chars.next(); // consume second char
                                if self.filter_strings {
                                    return self.next(); // Skip both opening chars
                                }
                            }
                            "${" => {
                                self.state.instring = StringDelimiter::FyppDollar;
                                self.state.infypp = true;
                                self.chars.next(); // consume second char
                                if self.filter_strings {
                                    return self.next(); // Skip both opening chars
                                }
                            }
                            "@{" => {
                                self.state.instring = StringDelimiter::FyppAt;
                                self.state.infypp = true;
                                self.chars.next(); // consume second char
                                if self.filter_strings {
                                    return self.next(); // Skip both opening chars
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        Some((pos, c))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_filtering() {
        let input = r#"x = "hello" + 5"#;
        let filter = CharFilter::new(input, false, false, false);
        let result: String = filter.map(|(_, c)| c).collect();
        assert_eq!(result, input);
    }

    #[test]
    fn test_filter_strings() {
        let input = r#"x = "hello" + 5"#;
        let filter = CharFilter::new(input, false, true, false);
        let result: String = filter.map(|(_, c)| c).collect();
        assert_eq!(result, r"x =  + 5");
    }

    #[test]
    fn test_filter_single_quotes() {
        let input = "x = 'hello' + 5";
        let filter = CharFilter::new(input, false, true, false);
        let result: String = filter.map(|(_, c)| c).collect();
        assert_eq!(result, "x =  + 5");
    }

    #[test]
    fn test_filter_comments() {
        let input = "x = 5 ! this is a comment";
        let filter = CharFilter::new(input, true, false, false);
        let result: String = filter.map(|(_, c)| c).collect();
        assert_eq!(result, "x = 5 ");
    }

    #[test]
    fn test_filter_both() {
        let input = r#"x = "hello" ! comment"#;
        let filter = CharFilter::new(input, true, true, false);
        let result: String = filter.map(|(_, c)| c).collect();
        assert_eq!(result, "x =  ");
    }

    #[test]
    fn test_fypp_inline() {
        let input = "x = #{expr}# + 5";
        let filter = CharFilter::new(input, false, true, true);
        let result: String = filter.map(|(_, c)| c).collect();
        assert_eq!(result, "x =  + 5");
    }

    #[test]
    fn test_fypp_dollar() {
        let input = "x = ${expr}$ + 5";
        let filter = CharFilter::new(input, false, true, true);
        let result: String = filter.map(|(_, c)| c).collect();
        assert_eq!(result, "x =  + 5");
    }

    #[test]
    fn test_fypp_at() {
        let input = "x = @{expr}@ + 5";
        let filter = CharFilter::new(input, false, true, true);
        let result: String = filter.map(|(_, c)| c).collect();
        assert_eq!(result, "x =  + 5");
    }

    #[test]
    fn test_instring_check() {
        let input = r#"x = "hello""#;
        let mut filter = CharFilter::new(input, false, false, false);

        // Before any string
        assert!(!filter.instring());

        // Consume until we're in the string
        while let Some((_, c)) = filter.next() {
            if c == 'h' {
                assert!(filter.instring());
                break;
            }
        }
    }

    #[test]
    fn test_position_tracking() {
        let input = "x = 5";
        let filter = CharFilter::new(input, false, false, false);
        let positions: Vec<usize> = filter.map(|(pos, _)| pos).collect();
        assert_eq!(positions, vec![0, 1, 2, 3, 4]);
    }
}
