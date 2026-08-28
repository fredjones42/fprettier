//! `CharFilter` - Iterator that filters out strings and comments
//!
//! This is a critical component that wraps a string iterator and maintains
//! state about whether we're inside strings, comments, or fypp preprocessor
//! directives. It's used throughout the codebase to ensure we only parse
//! actual Fortran code, not string contents or comments.

/// Find the byte position of the first `!` that starts a comment,
/// i.e. outside strings and fypp expressions.
#[must_use]
pub fn comment_start(line: &str) -> Option<usize> {
    CharFilter::code_and_comments(line).find_map(|(pos, c)| (c == '!').then_some(pos))
}

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

/// Split `line` into alternating runs of code and masked regions.
///
/// `filter` decides what is masked: string literals always, comments and fypp
/// expressions depending on how it was built. Even indices hold the code the
/// filter yielded, odd indices the masked text verbatim, so concatenating the
/// result reproduces `line`. A code run may be empty, when two masked regions
/// sit next to each other.
///
/// Returns `None` when the filter yields nothing at all - the whole line is
/// one masked region, and there is no code in it to rewrite.
#[must_use]
pub fn split_masked_regions(line: &str, filter: CharFilter<'_>) -> Option<Vec<String>> {
    let mut parts = vec![String::new()];
    // The byte just past the last code character, which is where a masked
    // region begins. Tracked as (position, char) so multi-byte characters
    // advance it by their real width.
    let mut prev: Option<(usize, char)> = None;

    for (pos, ch) in filter {
        let resume = prev.map_or(0, |(prev_pos, prev_char): (usize, char)| {
            prev_pos + prev_char.len_utf8()
        });
        if pos > resume {
            parts.push(line[resume..pos].to_string());
            parts.push(String::new());
        }
        // `parts` always has at least one element
        if let Some(last) = parts.last_mut() {
            last.push(ch);
        }
        prev = Some((pos, ch));
    }

    let (last_pos, last_char) = prev?;
    let resume = last_pos + last_char.len_utf8();
    if resume < line.len() {
        parts.push(line[resume..].to_string());
    }
    Some(parts)
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

    /// Iterate the line's code: string literals, comments and fypp inline
    /// blocks are all masked out
    #[must_use]
    pub fn code(content: &'a str) -> Self {
        Self::new(content, true, true, true)
    }

    /// Iterate the line's code and its comment, masking only string literals
    /// and fypp inline blocks
    #[must_use]
    pub fn code_and_comments(content: &'a str) -> Self {
        Self::new(content, false, true, true)
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
            state: FilterState {
                instring: string_state,
                infypp,
                incomment: false,
            },
            ..Self::new(content, filter_comments, filter_strings, filter_fypp)
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

    /// Peek at the next character without consuming
    fn peek_next_char(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, c)| c)
    }
}

impl Iterator for CharFilter<'_> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        // Loops rather than recursing: a filtered run is as long as the string
        // or comment it covers, and one stack frame per skipped character
        // overflows the stack on a megabyte-long literal.
        loop {
            let (pos, c) = self.chars.next()?;

            // Check for comment start (only if not in string)
            if self.state.instring == StringDelimiter::None && c == '!' {
                self.state.incomment = true;
                if self.filter_comments {
                    continue; // Skip the ! itself
                }
            }

            // If filtering and we're in a comment, skip
            if self.filter_comments && self.state.incomment {
                continue;
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
                        continue; // Skip the closing quote
                    }
                } else if self.state.infypp {
                    // Check for fypp close: `}` followed by the marker char matching
                    // the open delimiter (#{...}#, ${...}$, @{...}@)
                    let close_char = match self.state.instring {
                        StringDelimiter::FyppHash => '#',
                        StringDelimiter::FyppDollar => '$',
                        StringDelimiter::FyppAt => '@',
                        _ => '\0',
                    };
                    if c == '}' && self.peek_next_char() == Some(close_char) {
                        self.state.instring = StringDelimiter::None;
                        self.state.infypp = false;
                        just_closed_string = true;
                        self.chars.next(); // consume second char
                        if self.filter_strings {
                            continue; // Skip both closing chars
                        }
                    }
                } else if self.filter_strings {
                    // We're inside a string and filtering, skip this character
                    continue;
                }

                // If we're still in a string (state wasn't closed) and filtering, skip
                if self.filter_strings && self.state.instring != StringDelimiter::None {
                    continue;
                }
            }

            // Check for string open (only if not already in string and didn't just close one)
            if self.state.instring == StringDelimiter::None && !just_closed_string {
                if c == '\'' {
                    self.state.instring = StringDelimiter::Single;
                    if self.filter_strings {
                        continue; // Skip the opening quote
                    }
                } else if c == '"' {
                    self.state.instring = StringDelimiter::Double;
                    if self.filter_strings {
                        continue; // Skip the opening quote
                    }
                } else if self.filter_fypp && self.peek_next_char() == Some('{') {
                    // Check for fypp inline block open: #{, ${, @{
                    let delim = match c {
                        '#' => Some(StringDelimiter::FyppHash),
                        '$' => Some(StringDelimiter::FyppDollar),
                        '@' => Some(StringDelimiter::FyppAt),
                        _ => None,
                    };
                    if let Some(delim) = delim {
                        self.state.instring = delim;
                        self.state.infypp = true;
                        self.chars.next(); // consume second char
                        if self.filter_strings {
                            continue; // Skip both opening chars
                        }
                    }
                }
            }

            return Some((pos, c));
        }
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
