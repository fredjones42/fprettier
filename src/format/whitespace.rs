//! Whitespace formatting for Fortran code
//!
//! Implements 3-stage whitespace formatting:
//! 1. `rm_extra_whitespace`: Remove unnecessary whitespace
//! 2. `add_whitespace_charwise`: Add whitespace character-by-character
//! 3. `add_whitespace_context`: Context-aware whitespace additions

use std::sync::LazyLock;

use regex::Regex;

use crate::parser::CharFilter;

// Print/read statement formatting
static PRINT_READ_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)\b(print|read)\s*(\*,?)\s*").unwrap());

// Namelist formatting
static NML_STMT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)^\s*namelist\s*/.*/").unwrap());
static NML_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(/\w+/)").unwrap());

// END keyword separation (e.g., ENDIF -> END IF)
static END_WITH_EQ_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)^\s*end\s*(if|do|select|associate|block|subroutine|function|module|submodule|type|program|interface|enum|where|forall)\s*=").unwrap()
});
static END_AFTER_SEMI_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)(;\s*)(end)(if|do|select|associate|block|subroutine|function|module|submodule|type|program|interface|enum|where|forall)\b").unwrap()
});

// USE only formatting
static ONLY_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?i)(only)\s*:\s*").unwrap());

// Label formatting (label: keyword)
static LABEL_STMT_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)(\w+):(\s*)(do|if|select|where|forall|block|critical|associate)\b").unwrap()
});

// Plus/minus spacing
static PLUSMINUS_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"([\w\)\]])(\s*)(\+|-)(\s*)").unwrap());
// Pattern for scientific notation: number followed by e/d at end
// Must NOT be preceded by a letter (which would make it a variable name like val_1d)
static SCI_NOTATION_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)(?:^|[^a-z_])(\d+\.?\d*|\d*\.?\d+)[ed]$").unwrap());

/// Format a single logical Fortran line with whitespace formatting
///
/// Applies the 3-stage whitespace formatting process:
/// 1. Remove extra whitespace
/// 2. Add whitespace character-wise
/// 3. Add whitespace context-aware
#[must_use]
pub fn format_line(logical_line: &str, whitespace_flags: &[bool; 11], format_decl: bool) -> String {
    format_line_with_level(logical_line, whitespace_flags, format_decl, 0, None).0
}

/// Format a single Fortran line with bracket level tracking
///
/// Returns (`formatted_line`, `ending_bracket_level`, `last_significant_char`) for continuation support.
/// `prev_line_last_char` should be the last significant character from the previous line (before `&`),
/// used to determine if a leading +/- on a continuation line is binary or unary.
#[must_use]
pub fn format_line_with_level(
    logical_line: &str,
    whitespace_flags: &[bool; 11],
    format_decl: bool,
    initial_level: usize,
    prev_line_last_char: Option<char>,
) -> (String, usize, Option<char>) {
    let mut line = logical_line.to_string();

    // Stage 1: Remove extra whitespace
    line = rm_extra_whitespace(&line, format_decl);

    // Stage 2: Add whitespace character-wise (with bracket level tracking)
    let (formatted, final_level, last_char) = add_whitespace_charwise_with_level(
        &line,
        whitespace_flags,
        format_decl,
        initial_level,
        prev_line_last_char,
    );
    line = formatted;

    // Stage 3: Add whitespace context-aware
    line = add_whitespace_context(&line, whitespace_flags);

    (line, final_level, last_char)
}

/// Remove all unnecessary whitespace from the line
///
/// Removes double spaces while preserving single spaces between words
fn rm_extra_whitespace(line: &str, format_decl: bool) -> String {
    let mut formatted_line = String::new();
    // Store (byte_position, char) to correctly compute next byte position for multi-byte chars
    let mut prev_info: Option<(usize, char)> = None;

    // Preserve leading whitespace - important for alignment computation
    let leading_spaces = line.len() - line.trim_start().len();
    formatted_line.push_str(&" ".repeat(leading_spaces));

    let char_filter = CharFilter::new(line, true, true, true);

    for (pos, ch) in char_filter {
        // Skip characters that are part of leading whitespace (already handled)
        if pos < leading_spaces {
            prev_info = Some((pos, ch));
            continue;
        }

        // Add any skipped content (strings, comments, fypp expressions)
        // This handles content skipped by CharFilter that needs to be preserved verbatim
        if let Some((prev_pos, prev_char)) = prev_info {
            // Next byte position after previous char (accounts for multi-byte Unicode)
            let next_byte = prev_pos + prev_char.len_utf8();
            if pos > next_byte {
                // Only add skipped content if it's not part of leading whitespace
                let start = if next_byte < leading_spaces {
                    leading_spaces
                } else {
                    next_byte
                };
                if start < pos {
                    formatted_line.push_str(&line[start..pos]);
                }
            }
        } else if pos > leading_spaces {
            // First character in the loop, but there's skipped content before it
            // This handles fypp/string content at the start of the line (after leading whitespace)
            formatted_line.push_str(&line[leading_spaces..pos]);
        }

        // Check if we're in a declaration
        let is_decl = if format_decl {
            false
        } else {
            line[pos..].trim_start().starts_with("::") || line[..pos].trim_end().ends_with("::")
        };

        if ch == ' ' {
            // Remove double spaces, but keep single spaces between words
            // Also keep spaces after fypp close markers (}#, }$, }@)
            // And keep spaces before fypp open markers (#{, ${, @{) by checking original line
            // And keep spaces before string literals (CharFilter skips quotes, so check original)
            if !formatted_line.is_empty() {
                // Safe: we just checked !formatted_line.is_empty()
                let last_char = formatted_line.chars().last().unwrap_or(' ');
                let ends_with_fypp_close = formatted_line.ends_with("}#")
                    || formatted_line.ends_with("}$")
                    || formatted_line.ends_with("}@");
                // Check if this space is followed by fypp open in the original line
                // Use byte slicing from current position to look ahead
                let after_space = &line[pos + ch.len_utf8()..];
                let space_before_fypp = after_space.starts_with("#{")
                    || after_space.starts_with("${")
                    || after_space.starts_with("@{");
                // Check if this space is followed by a string literal (quote)
                // CharFilter skips quotes, so we must check original line
                let space_before_string =
                    after_space.starts_with('"') || after_space.starts_with('\'');
                if last_char.is_alphanumeric()
                    || is_decl
                    || ends_with_fypp_close
                    || space_before_fypp
                    || space_before_string
                {
                    formatted_line.push(ch);
                }
            }
        } else {
            // Remove trailing space before non-word characters (except in declarations)
            // But keep space before fypp open markers (#{, ${, @{)
            // And keep space after fypp close markers (}#, }$, }@)
            // And keep space before string literals (quotes)
            if !formatted_line.is_empty() && formatted_line.ends_with(' ') {
                // Check if current char starts a fypp open marker
                let after_ch = &line[pos + ch.len_utf8()..];
                let is_fypp_open =
                    (ch == '#' || ch == '$' || ch == '@') && after_ch.starts_with('{');
                // Check if the content before the space ends with fypp close
                let len = formatted_line.len();
                let before_space = if len >= 3 {
                    &formatted_line[..len - 1]
                } else {
                    ""
                };
                let space_after_fypp_close = before_space.ends_with("}#")
                    || before_space.ends_with("}$")
                    || before_space.ends_with("}@");
                // Keep space before string literals (opening quotes)
                let is_string_start = ch == '"' || ch == '\'';
                if !ch.is_alphanumeric()
                    && !is_decl
                    && !is_fypp_open
                    && !space_after_fypp_close
                    && !is_string_start
                {
                    formatted_line.pop(); // Remove the trailing space
                }
            }
            formatted_line.push(ch);
        }

        prev_info = Some((pos, ch));
    }

    // Add any remaining content after the last filtered character
    if let Some((prev_pos, prev_char)) = prev_info {
        let next_byte = prev_pos + prev_char.len_utf8();
        if next_byte < line.len() {
            formatted_line.push_str(&line[next_byte..]);
        }
    } else {
        // No filtered characters, return whole line
        return line.to_string();
    }

    formatted_line
}

/// Add whitespace character-wise with bracket level tracking
///
/// Returns (`formatted_line`, `ending_bracket_level`, `last_significant_char`) for continuation support.
/// `prev_line_last_char` is the last significant character from the previous line (if any),
/// used to determine if leading +/- on a continuation is binary or unary.
fn add_whitespace_charwise_with_level(
    line: &str,
    whitespace_flags: &[bool; 11],
    format_decl: bool,
    initial_level: usize,
    prev_line_last_char: Option<char>,
) -> (String, usize, Option<char>) {
    use crate::parser::patterns::{
        DEL_CLOSE_RE, DEL_OPEN_RE, INTR_STMTS_PAR_RE, KEYWORD_PAREN_RE, REL_OP_RE,
    };

    let mut formatted_line = String::with_capacity(line.len() * 2);
    let mut i = 0;
    let chars: Vec<char> = line.chars().collect();
    let mut level = initial_level; // Bracket nesting level (can start > 0 for continuations)
    let mut in_string: Option<char> = None; // None or Some(quote_char) when in string
    let mut in_fypp: Option<char> = None; // None or Some(close_char) when in fypp expr

    while i < chars.len() {
        let ch = chars[i];

        // Track fypp expression state - copy fypp content verbatim without formatting
        // Fypp expressions: #{...}#, ${...}$, @{...}@
        if let Some(close_char) = in_fypp {
            // We're inside a fypp expression - copy verbatim
            formatted_line.push(ch);
            i += 1;
            // Check for closing sequence: }# or }$ or }@
            if ch == '}' && i < chars.len() && chars[i] == close_char {
                // Found closing - copy the close char and exit fypp
                formatted_line.push(chars[i]);
                i += 1;
                in_fypp = None;
            }
            continue;
        }

        // Check for fypp expression start: #{ or ${ or @{
        if (ch == '#' || ch == '$' || ch == '@') && i + 1 < chars.len() && chars[i + 1] == '{' {
            in_fypp = Some(ch); // Store the open char - same char is used for close
            formatted_line.push(ch);
            formatted_line.push(chars[i + 1]);
            i += 2;
            continue;
        }

        // Track string state - copy string content verbatim without formatting
        if let Some(quote) = in_string {
            // We're inside a string - copy verbatim
            formatted_line.push(ch);
            i += 1;
            // Check for closing quote (handle escaped quotes by checking for double quote)
            if ch == quote {
                // Check if it's an escaped quote (doubled)
                if i < chars.len() && chars[i] == quote {
                    // Escaped quote - copy it and continue
                    formatted_line.push(chars[i]);
                    i += 1;
                } else {
                    // Closing quote - exit string
                    in_string = None;
                }
            }
            continue;
        }

        // Check for string start
        if ch == '"' || ch == '\'' {
            in_string = Some(ch);
            formatted_line.push(ch);
            i += 1;
            continue;
        }

        // Handle opening delimiters: (, (/, [
        if ch == '(' || ch == '[' {
            // Check if this is a delimiter token
            let remaining: String = chars[i..].iter().take(2).collect();
            if let Some(m) = DEL_OPEN_RE.find(&remaining) {
                let delim = m.as_str();
                level += 1;

                // Check if we need space before opening delimiter
                let lhs: String = chars[..i].iter().collect();
                let mut sep1 = false;

                // Check for keywords that need space before (
                if whitespace_flags[8] {
                    // Check if lhs ends with %word pattern (e.g., obj%method, hdf5%open)
                    // This prevents adding space in type component member access
                    let ends_with_percent_word = if let Some(percent_pos) = lhs.rfind('%') {
                        // Check if everything after % is alphanumeric/underscore (no spaces)
                        lhs[percent_pos + 1..].chars().all(|c| c.is_alphanumeric() || c == '_')
                    } else {
                        false
                    };

                    // Check for intrinsic statements (ALLOCATE, WRITE, etc.)
                    // or IF, DO WHILE, CASE, etc.
                    // But NOT if preceded by % (e.g., obj%open should not add space)
                    if !ends_with_percent_word && (INTR_STMTS_PAR_RE.is_match(&lhs) || KEYWORD_PAREN_RE.is_match(&lhs)) {
                        sep1 = true;
                    }
                    // Also check after semicolon (e.g., "do i=1,10; if(x)")
                    // Take portion after last semicolon and check
                    else if let Some(semi_pos) = lhs.rfind(';') {
                        let after_semi = &lhs[semi_pos + 1..];
                        if KEYWORD_PAREN_RE.is_match(after_semi) {
                            sep1 = true;
                        }
                    }
                }

                // Also need space after closing delimiter before opening
                // e.g., "write (*, *)(merge" -> "write (*, *) (merge"
                let prev_non_space = lhs.chars().rev().find(|&c| c != ' ');
                if prev_non_space == Some(')') || prev_non_space == Some(']') {
                    sep1 = true;
                }

                // Also need space after assignment before opening bracket
                // e.g., "big_arr =[1" -> "big_arr = [1"
                // But NOT for named parameters in function calls (level > 1)
                // Note: level was already incremented, so level==1 means top-level assignment
                if level == 1 && (prev_non_space == Some('=') || prev_non_space == Some('>')) {
                    sep1 = true;
                }

                // Also need space after comma before opening bracket
                // e.g., "[1,(/3" -> "[1, (/3"
                if prev_non_space == Some(',') {
                    sep1 = true;
                }

                // Need space before [ after certain operators like + for array constructors
                // But NOT before ( after operators (function call style: 3*(x), -(x))
                if delim == "[" {
                    if let Some(pc) = prev_non_space {
                        if pc == '+' || pc == '-' {
                            sep1 = true;
                        }
                    }
                }

                // General case: add space before ( unless the previous char is a special char
                // This handles cases like "function name(", "subroutine name(", etc.
                // Add space UNLESS the previous char is one of: ( [ (/ alphanumeric _ * / = + - :
                if whitespace_flags[8] && !sep1 {
                    if let Some(pc) = prev_non_space {
                        // Skip space if ending with delimiter, operator, alphanumeric, or underscore
                        let skip_space = pc == '('
                            || pc == '['
                            || pc == '/'  // part of (/
                            || pc.is_alphanumeric()
                            || pc == '_'  // underscore is valid in variable names
                            || pc == '*'
                            || pc == '='
                            || pc == '+'
                            || pc == '-'
                            || pc == ':';
                        if !skip_space {
                            sep1 = true;
                        }
                    }
                }

                // Apply formatting
                // Remove trailing spaces (but preserve if sep1 is true, we'll add it back)
                // Also preserve space after binary operators (+, -, *, /)
                // Check the last non-space character in formatted_line
                let last_non_space_in_ftd = formatted_line.chars().rev().find(|&c| c != ' ');
                let preserve_space = matches!(last_non_space_in_ftd, Some('+' | '-' | '*' | '/'));

                if !preserve_space {
                    while formatted_line.ends_with(' ') {
                        formatted_line.pop();
                    }
                }

                // Add space before if needed (but not if we already preserved one)
                if sep1 && !formatted_line.ends_with(' ') {
                    formatted_line.push(' ');
                }

                // Add the delimiter
                formatted_line.push_str(delim);
                i += delim.len();

                // Remove leading spaces from what follows
                while i < chars.len() && chars[i] == ' ' {
                    i += 1;
                }
                continue;
            }
        }

        // Handle closing delimiters: ), /), ]
        if ch == ')' || ch == ']' || (ch == '/' && i + 1 < chars.len() && chars[i + 1] == ')') {
            let remaining: String = chars[i..].iter().take(2).collect();
            if let Some(m) = DEL_CLOSE_RE.find(&remaining) {
                let delim = m.as_str();
                level = level.saturating_sub(1);

                // Remove trailing spaces before delimiter
                while formatted_line.ends_with(' ') {
                    formatted_line.pop();
                }

                // Add the delimiter
                formatted_line.push_str(delim);
                i += delim.len();

                // Count existing spaces after delimiter (before stripping)
                // This is needed to preserve spacing before :: when format_decl=false
                let mut space_count = 0;
                let mut j = i;
                while j < chars.len() && chars[j] == ' ' {
                    space_count += 1;
                    j += 1;
                }

                // Check if followed by :: (declaration operator)
                let followed_by_double_colon =
                    j + 1 < chars.len() && chars[j] == ':' && chars[j + 1] == ':';

                // Skip existing spaces
                while i < chars.len() && chars[i] == ' ' {
                    i += 1;
                }

                // Check what follows and determine spacing
                if i < chars.len() {
                    let next_ch = chars[i];

                    // Special handling for :: after closing delimiter
                    // When format_decl=false, preserve the original spacing
                    // When format_decl=true, use single space
                    if followed_by_double_colon {
                        if !format_decl && space_count > 0 {
                            // Preserve original spacing
                            for _ in 0..space_count {
                                formatted_line.push(' ');
                            }
                        } else if format_decl {
                            // format_decl=true uses single space (handled by :: processing)
                            // Don't add space here; let the :: handler add it
                        }
                        // If format_decl=false and space_count=0, no spaces added
                        continue;
                    }

                    // Don't add space before these characters
                    let skip_space = next_ch == ')'
                        || next_ch == ']'
                        || next_ch == ','
                        || next_ch == '%'
                        || next_ch == ':'
                        || next_ch == '/'
                        || next_ch == '*'
                        || next_ch == ';'
                        || (next_ch == '/' && i + 1 < chars.len() && chars[i + 1] == ')');

                    if !skip_space {
                        formatted_line.push(' ');
                    }
                }
                continue;
            }
        }

        // Handle commas and semicolons
        if (ch == ',' || ch == ';') && whitespace_flags[0] {
            // Remove trailing space before
            while formatted_line.ends_with(' ') {
                formatted_line.pop();
            }
            formatted_line.push(ch);
            // Add space after
            formatted_line.push(' ');
            i += 1;
            // Skip any existing spaces after
            while i < chars.len() && chars[i] == ' ' {
                i += 1;
            }
            continue;
        }

        // Handle type component % (whitespace_flags[7] - remove spaces around %)
        if ch == '%' {
            // Remove trailing space before
            while formatted_line.ends_with(' ') {
                formatted_line.pop();
            }

            // Add % with optional spacing (whitespace_flags[7] = 0 means no space)
            if whitespace_flags[7] {
                formatted_line.push(' ');
            }
            formatted_line.push('%');
            if whitespace_flags[7] {
                formatted_line.push(' ');
            }
            i += 1;

            // Skip any existing spaces after
            while i < chars.len() && chars[i] == ' ' {
                i += 1;
            }
            continue;
        }

        // Handle assignment operators (= and =>)
        // Only add space when NOT inside brackets (level == 0), EXCEPT for pointer assignment
        if ch == '=' && whitespace_flags[1] {
            // Check if it's => (pointer assignment) first
            let is_pointer = i + 1 < chars.len() && chars[i + 1] == '>';

            // Check if it's part of a relational operator (==, /=, <=, >=)
            // But NOT if it's => (pointer assignment)
            let is_relational = if is_pointer {
                false // => is not a relational operator
            } else {
                let context_start = i.saturating_sub(1);
                let context_end = (i + 2).min(chars.len());
                let context: String = chars[context_start..context_end].iter().collect();

                REL_OP_RE.is_match(&context)
            };

            // Only add spacing at top level (level == 0) or for pointer assignment
            let should_space = (level == 0 || is_pointer) && !is_relational;

            if should_space {
                // Remove trailing space before
                while formatted_line.ends_with(' ') {
                    formatted_line.pop();
                }

                // Add space before
                formatted_line.push(' ');
                formatted_line.push('=');

                if is_pointer {
                    formatted_line.push('>');
                    i += 1;
                }

                // Add space after
                formatted_line.push(' ');
                i += 1;

                // Skip any existing spaces after
                while i < chars.len() && chars[i] == ' ' {
                    i += 1;
                }
                continue;
            }
        }

        // Handle declaration operator :: (whitespace_flags[9])
        // Only reformat when format_decl is enabled; otherwise preserve existing spacing
        if ch == ':'
            && i + 1 < chars.len()
            && chars[i + 1] == ':'
            && format_decl
            && whitespace_flags[9]
        {
            // Remove trailing space before
            while formatted_line.ends_with(' ') {
                formatted_line.pop();
            }

            // Add space before ::
            formatted_line.push(' ');
            formatted_line.push(':');
            formatted_line.push(':');

            // Add space after ::
            formatted_line.push(' ');
            i += 2;

            // Skip any existing spaces after
            while i < chars.len() && chars[i] == ' ' {
                i += 1;
            }
            continue;
        }

        // Handle string concatenation operator // (whitespace_flags[10])
        // Must distinguish from array constructor delimiters (/ and /)
        if ch == '/' && i + 1 < chars.len() && chars[i + 1] == '/' {
            // When whitespace_flags[10] is false, normalize by removing surrounding spaces
            if !whitespace_flags[10] {
                // Remove trailing spaces before //
                while formatted_line.ends_with(' ') {
                    formatted_line.pop();
                }

                // Add the concat operator without spaces
                formatted_line.push('/');
                formatted_line.push('/');
                i += 2;

                // Skip any existing spaces after //
                while i < chars.len() && chars[i] == ' ' {
                    i += 1;
                }
                continue;
            }

            // whitespace_flags[10] is true - add spacing around //
            let prev_char = formatted_line.chars().last();
            let prev_non_space = formatted_line.chars().rev().find(|&c| c != ' ');
            let skip_space_before = prev_non_space == Some('(') || prev_non_space == Some('[');
            let next_after_concat = if i + 2 < chars.len() {
                Some(chars[i + 2])
            } else {
                None
            };
            let skip_space_after = next_after_concat == Some(')') || next_after_concat == Some(']');

            // Remove trailing spaces
            while formatted_line.ends_with(' ') {
                formatted_line.pop();
            }

            // Add space before unless after open delimiter
            if !skip_space_before && prev_char.is_some() {
                formatted_line.push(' ');
            }

            // Add the concat operator
            formatted_line.push('/');
            formatted_line.push('/');
            i += 2;

            // Skip existing spaces after
            while i < chars.len() && chars[i] == ' ' {
                i += 1;
            }

            // Add space after unless before close delimiter
            if !skip_space_after && i < chars.len() {
                formatted_line.push(' ');
            }

            continue;
        }

        // Handle plus/minus operators
        if (ch == '+' || ch == '-') && whitespace_flags[4] {
            // Only add spacing if it's a binary operator, not unary
            // Check for scientific notation exponent (1.0d-3, 1.0e+5)

            // Look at the last non-space character (we may have added a space after delimiter)
            let prev_char = formatted_line.chars().rev().find(|&c| c != ' ');

            // Check if this is a scientific notation exponent sign
            // Must verify that what precedes is actually a number, not a variable name
            // e.g., "1.0d" is exponential, but "val_1d" is a variable name
            let is_exponent = match prev_char {
                Some(c) if c == 'd' || c == 'D' || c == 'e' || c == 'E' => {
                    // Check if the content before the d/e forms a valid number
                    let trimmed = formatted_line.trim_end();
                    SCI_NOTATION_RE.is_match(trimmed)
                }
                _ => false,
            };

            // Simple heuristic: add spacing if preceded by alphanumeric, ), or ]
            // But NOT if it's part of scientific notation.
            // For continuation lines, check the previous line's last character.
            let is_binary = match prev_char {
                Some(c) => (c.is_alphanumeric() || c == ')' || c == ']') && !is_exponent,
                None => {
                    // At start of line: binary only if this is a continuation
                    // from a line that ended with an operand (alphanumeric, ), or ])
                    // If prev line ended with comma, delimiter, or operator, it's unary.
                    if formatted_line.trim().is_empty() {
                        // Check prev_line_last_char to determine if +/- is binary
                        match prev_line_last_char {
                            Some(c) => c.is_alphanumeric() || c == ')' || c == ']',
                            None => false,
                        }
                    } else {
                        false
                    }
                }
            };

            if is_binary {
                // Remove trailing space before
                while formatted_line.ends_with(' ') {
                    formatted_line.pop();
                }

                // Add space before
                formatted_line.push(' ');
                formatted_line.push(ch);

                // Add space after
                formatted_line.push(' ');
                i += 1;

                // Skip any existing spaces after
                while i < chars.len() && chars[i] == ' ' {
                    i += 1;
                }
                continue;
            }
        }

        // Handle multiply/divide operators (* and /) (whitespace_flags[5])
        // Must NOT match ** (exponentiation) or // (concatenation) or (/ /) (array constructor)
        if (ch == '*' || ch == '/') && whitespace_flags[5] {
            // Check for ** (exponentiation) - skip
            if ch == '*' && i + 1 < chars.len() && chars[i + 1] == '*' {
                formatted_line.push(ch);
                i += 1;
                continue;
            }

            // Check for // (concatenation) - already handled above if whitespace_flags[10]
            // But if whitespace_flags[10] is false and we see //, just skip both
            if ch == '/' && i + 1 < chars.len() && chars[i + 1] == '/' {
                formatted_line.push(ch);
                i += 1;
                continue;
            }

            // Check for (/ or /) - array constructor delimiters, skip
            let prev_non_space = formatted_line.chars().rev().find(|&c| c != ' ');
            let next_char = if i + 1 < chars.len() {
                Some(chars[i + 1])
            } else {
                None
            };

            // Skip if this is part of array constructor: (/ or /)
            if ch == '/' && (prev_non_space == Some('(') || next_char == Some(')')) {
                formatted_line.push(ch);
                i += 1;
                continue;
            }

            // This is a binary mult/div operator - add spacing
            // But only if preceded by alphanumeric, ), or ]
            let is_binary = match prev_non_space {
                Some(c) => c.is_alphanumeric() || c == ')' || c == ']',
                None => false,
            };

            if is_binary {
                // Remove trailing spaces
                while formatted_line.ends_with(' ') {
                    formatted_line.pop();
                }

                // Add space before
                formatted_line.push(' ');
                formatted_line.push(ch);

                // Add space after
                formatted_line.push(' ');
                i += 1;

                // Skip existing spaces after
                while i < chars.len() && chars[i] == ' ' {
                    i += 1;
                }
                continue;
            }
        }

        formatted_line.push(ch);
        i += 1;
    }

    // Compute last significant character from final formatted output
    // Use CharFilter to skip strings and comments, so we get the last CODE char
    // This is used to determine if leading +/- on next line is binary or unary
    let last_significant_char = {
        let mut last_char = None;
        for (_, c) in CharFilter::new(&formatted_line, true, true, true) {
            if !c.is_whitespace() {
                last_char = Some(c);
            }
        }
        last_char
    };

    (formatted_line, level, last_significant_char)
}

/// Add whitespace in a context-aware manner
///
/// Uses regex-based splitting for operators that need context:
/// - Relational operators (`whitespace_flags[2]`): ==, /=, <, >, <=, >=, .eq., .ne., etc.
/// - Logical operators (`whitespace_flags[3]`): .and., .or., .not., .eqv., .neqv.
/// - Multiply/divide (`whitespace_flags[5]`): *, / (but not ** or //)
/// - Print statements (`whitespace_flags[6]`): print, read
///
/// This preserves strings and comments by splitting the line into parts.
fn add_whitespace_context(line: &str, whitespace_flags: &[bool; 11]) -> String {
    use crate::parser::patterns::{END_RE, LOG_OP_RE, REL_OP_RE, USE_RE};

    // Placeholder to protect pointer assignment (=>) from relational processing
    const PLACEHOLDER: &str = "\x00POINTER_ASSIGN\x00";

    // Split line into code parts and non-code parts (strings/comments)
    let mut line_parts: Vec<String> = vec![String::new()];
    // Store (byte_position, char) to correctly compute next byte position for multi-byte chars
    let mut prev_info: Option<(usize, char)> = None;

    let char_filter = CharFilter::new(line, true, true, true);

    for (pos, ch) in char_filter {
        // Add skipped content (strings, comments, fypp expressions) as separate parts
        if let Some((prev_pos, prev_char)) = prev_info {
            // Next byte position after previous char (accounts for multi-byte Unicode)
            let next_byte = prev_pos + prev_char.len_utf8();
            if pos > next_byte {
                let skipped = &line[next_byte..pos];
                if !skipped.trim().is_empty() {
                    line_parts.push(skipped.to_string());
                    line_parts.push(String::new());
                }
            }
        } else if pos > 0 {
            // First character in the loop, but there's skipped content before it
            // This handles fypp/string content at the start of the line
            let skipped = &line[0..pos];
            if !skipped.trim().is_empty() {
                line_parts.push(skipped.to_string());
                line_parts.push(String::new());
            }
        }

        // Add character to current part (line_parts always has at least one element)
        if let Some(last_part) = line_parts.last_mut() {
            last_part.push(ch);
        }
        prev_info = Some((pos, ch));
    }

    // Add any remaining content after last filtered character
    if let Some((prev_pos, prev_char)) = prev_info {
        let next_byte = prev_pos + prev_char.len_utf8();
        if next_byte < line.len() {
            line_parts.push(line[next_byte..].to_string());
        }
    } else {
        // No filtered characters - line is entirely in a string/comment/fypp
        // Return the original line unchanged
        return line.to_string();
    }

    // Process each code part (not strings/comments) with operator regexes
    // Operators array maps to whitespace_flags indices:
    // [2]=relational, [3]=logical, [4]=plusminus, [5]=multdiv, [6]=print

    for part in &mut line_parts {
        // Skip strings, comments, and fypp expressions
        // They start with quotes, !, or fypp markers #{, ${, @{
        if part.starts_with('"')
            || part.starts_with('\'')
            || part.starts_with('!')
            || part.starts_with("#{")
            || part.starts_with("${")
            || part.starts_with("@{")
        {
            continue;
        }

        // Protect pointer assignment operators (=>) from relational operator processing
        // Replace with placeholder, process, then restore
        let has_pointer_assign = part.contains(" => ");
        if has_pointer_assign {
            *part = part.replace(" => ", PLACEHOLDER);
        }

        // Relational operators (whitespace_flags[2])
        if whitespace_flags[2] {
            *part = add_spacing_around_operator(part, &REL_OP_RE);
        }

        // Restore pointer assignment operators
        if has_pointer_assign {
            *part = part.replace(PLACEHOLDER, " => ");
        }

        // Logical operators (whitespace_flags[3])
        if whitespace_flags[3] {
            *part = add_spacing_around_operator(part, &LOG_OP_RE);
        }

        // Plus/minus (whitespace_flags[4])
        // Pattern must match binary +/- only (preceded by word char, ), or ])
        // and exclude scientific notation (e.g., 1.0e+10)
        if whitespace_flags[4] {
            *part = add_spacing_around_plusminus(part);
        }

        // Note: Multiply/divide ([5]) handled in add_whitespace_charwise_with_level

        // Print statements (whitespace_flags[6]): add space between print/read and *
        // Pattern: print* -> print * and print*, -> print *,
        if whitespace_flags[6] {
            *part = PRINT_READ_RE.replace_all(part, "$1 $2 ").to_string();
        }
    }

    // Join all parts back together
    let mut result = line_parts.join("");

    // Format namelists with spaces around / delimiters
    // Pattern: namelist /name/ variables -> namelist /name/ variables (with spaces)
    if NML_STMT_RE.is_match(&result) {
        // Add spaces around /word/ patterns
        result = NML_RE.replace_all(&result, " $1 ").to_string();
        // Clean up any double spaces that might have been introduced
        while result.contains("  ") {
            result = result.replace("  ", " ");
        }
    }

    // Separate compound END keywords (e.g., ENDIF -> END IF) if whitespace_flags[8] is true
    // BUT: Don't split if followed by assignment (e.g., "endif = 3" is a variable name)
    if whitespace_flags[8] && END_RE.is_match(&result) {
        // Check if this is really an END statement (not followed by =)
        if !END_WITH_EQ_RE.is_match(&result) {
            result = END_RE.replace_all(&result, "$1 $2").to_string();
        }
    }

    // Also handle END keywords after semicolon (e.g., "end do; enddo" -> "end do; end do")
    if whitespace_flags[8] {
        result = END_AFTER_SEMI_RE
            .replace_all(&result, "$1$2 $3")
            .to_string();
    }

    // Format ':' for USE only statements (use module, only: ...)
    if whitespace_flags[0] && USE_RE.is_match(&result) {
        // Add space after 'only:' -> 'only: '
        result = ONLY_RE.replace_all(&result, "$1: ").to_string();
    }

    // Format labeled statements (label: statement)
    // Pattern: identifier followed by : (not ::) followed by keyword
    if whitespace_flags[8] {
        // Match label followed by : and a keyword (DO, IF, etc.) without intervening space
        result = LABEL_STMT_RE.replace_all(&result, "$1: $3").to_string();
    }

    result
}

/// Add spacing around binary +/- operators
///
/// Special handling for plus/minus:
/// 1. Only matches binary operators (preceded by word char, ), or ])
/// 2. Excludes scientific notation (e.g., 1.0e+10, 2.5d-3)
fn add_spacing_around_plusminus(text: &str) -> String {
    let mut result = String::new();
    let mut last_end = 0;

    for captures in PLUSMINUS_RE.captures_iter(text) {
        let full_match = captures.get(0).unwrap();
        let match_start = full_match.start();
        let match_end = full_match.end();

        // Add text before this match
        result.push_str(&text[last_end..match_start]);

        let before_char = captures.get(1).unwrap().as_str();
        let operator = captures.get(3).unwrap().as_str();

        // Check if this is scientific notation by looking at what precedes
        let prefix = &text[..match_start + before_char.len()];
        if SCI_NOTATION_RE.is_match(prefix) {
            // This is scientific notation, keep original
            result.push_str(full_match.as_str());
        } else {
            // Binary operator - add spaces
            result.push_str(before_char);
            result.push(' ');
            result.push_str(operator);
            result.push(' ');
        }

        last_end = match_end;
    }

    // Add remaining text
    result.push_str(&text[last_end..]);

    result
}

/// Add spacing around operators matched by a regex
///
/// Splits the string by the regex and joins with spaces
fn add_spacing_around_operator(text: &str, operator_re: &regex::Regex) -> String {
    // Split by the operator, preserving the delimiters
    let parts: Vec<&str> = operator_re.split(text).collect();
    let operators: Vec<&str> = operator_re.find_iter(text).map(|m| m.as_str()).collect();

    if operators.is_empty() {
        return text.to_string();
    }

    let mut result = String::new();
    for (i, part) in parts.iter().enumerate() {
        // Only trim leading whitespace (from after the previous operator).
        // Preserve trailing whitespace (may be significant, e.g., before string).
        // For the first part, also trim trailing since we'll add space before operator.
        let trimmed = if i == 0 {
            part.trim()
        } else if i == parts.len() - 1 {
            // Last part: only trim leading (preserve trailing for content after last operator)
            part.trim_start()
        } else {
            // Middle parts: trim both since we add spaces on both sides of operators
            part.trim()
        };
        result.push_str(trimmed);
        if i < operators.len() {
            // Don't add space before operator if:
            // - previous part ends with ( or [, e.g., "(.not." should stay as "(.not."
            // - result already ends with a space (e.g., adjacent operators like ".and..not.")
            let ends_with_open = trimmed.ends_with('(') || trimmed.ends_with('[');
            let already_has_space = result.ends_with(' ');
            if !ends_with_open && !already_has_space {
                result.push(' ');
            }
            result.push_str(operators[i]);
            result.push(' ');
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rm_extra_whitespace() {
        let line = "x  =  5";
        let result = rm_extra_whitespace(line, false);
        // Should remove double spaces
        assert!(result.len() < line.len());
    }

    #[test]
    fn test_rm_extra_whitespace_preserves_leading() {
        // Leading whitespace should be preserved (important for label handling)
        let line = "      FORMAT(A)"; // 6 leading spaces
        let result = rm_extra_whitespace(line, false);
        let leading = result.len() - result.trim_start().len();
        assert_eq!(
            leading, 6,
            "Should preserve 6 leading spaces, got {leading}"
        );
    }

    #[test]
    fn test_format_line_preserves_leading() {
        // format_line should preserve leading whitespace
        let line = "      FORMAT(A)"; // 6 leading spaces
        let whitespace_flags = [true; 11];
        let result = format_line(line, &whitespace_flags, true);
        let leading = result.len() - result.trim_start().len();
        assert_eq!(
            leading, 6,
            "format_line should preserve 6 leading spaces, got {leading}. Result: '{result}'"
        );
    }

    #[test]
    fn test_add_whitespace_commas() {
        let whitespace_flags = [
            true, false, false, false, false, false, false, false, false, false, false,
        ];
        let line = "call foo(a,b,c)";
        let result = add_whitespace_charwise_with_level(line, &whitespace_flags, false, 0, None).0;
        assert_eq!(result, "call foo(a, b, c)");
    }

    #[test]
    fn test_add_whitespace_assignment() {
        let whitespace_flags = [
            false, true, false, false, false, false, false, false, false, false, false,
        ];
        let line = "x=5";
        let result = add_whitespace_charwise_with_level(line, &whitespace_flags, false, 0, None).0;
        assert_eq!(result, "x = 5");
    }

    #[test]
    fn test_add_whitespace_pointer_assignment() {
        let whitespace_flags = [
            false, true, false, false, false, false, false, false, false, false, false,
        ];
        let line = "ptr=>target";
        let result = add_whitespace_charwise_with_level(line, &whitespace_flags, false, 0, None).0;
        assert_eq!(result, "ptr => target");
    }

    #[test]
    fn test_add_whitespace_plusminus_binary() {
        let whitespace_flags = [
            false, false, false, false, true, false, false, false, false, false, false,
        ];
        let line = "x=a+b-c";
        let result = add_whitespace_charwise_with_level(line, &whitespace_flags, false, 0, None).0;
        // Should add spaces around binary + and -
        assert!(result.contains(" + ") && result.contains(" - "));
    }

    #[test]
    fn test_add_whitespace_plusminus_unary() {
        // Test that unary +/- don't get spacing when prev_line_last_char is comma
        let whitespace_flags = [
            false, false, false, false, true, false, false, false, false, false, false,
        ];
        // Simulating continuation line where previous line ended with comma
        let line = "-2";
        let result =
            add_whitespace_charwise_with_level(line, &whitespace_flags, false, 0, Some(',')).0;
        // Should NOT add space after unary -
        assert_eq!(
            result, "-2",
            "Unary minus after comma should not have space"
        );

        let line2 = "+3";
        let result2 =
            add_whitespace_charwise_with_level(line2, &whitespace_flags, false, 0, Some(',')).0;
        // Should NOT add space after unary +
        assert_eq!(
            result2, "+3",
            "Unary plus after comma should not have space"
        );
    }

    #[test]
    fn test_add_whitespace_plusminus_binary_continuation() {
        // Test that +/- are treated as binary when prev_line_last_char is ) or alphanumeric
        let whitespace_flags = [
            false, false, false, false, true, false, false, false, false, false, false,
        ];
        // Previous line ended with )
        let line = "+c";
        let result =
            add_whitespace_charwise_with_level(line, &whitespace_flags, false, 0, Some(')')).0;
        // Should add space before and after binary +
        assert!(
            result.contains(" + "),
            "Binary plus after ) should have spaces: {result}"
        );

        // Previous line ended with alphanumeric
        let line2 = "-d";
        let result2 =
            add_whitespace_charwise_with_level(line2, &whitespace_flags, false, 0, Some('b')).0;
        // Should add space before and after binary -
        assert!(
            result2.contains(" - "),
            "Binary minus after alphanumeric should have spaces: {result2}"
        );
    }

    #[test]
    fn test_format_line_comprehensive() {
        let whitespace_flags = [
            true, true, false, false, true, false, false, false, false, false, false,
        ];
        let line = "x=a+b,y=c-d";
        let result = format_line(line, &whitespace_flags, false);
        // Should have spacing around =, +, -, and ,
        assert!(result.contains(" = "));
        assert!(result.contains(", "));
    }

    #[test]
    fn test_relational_operators() {
        let whitespace_flags = [
            false, false, true, false, false, false, false, false, false, false, false,
        ];
        let line = "if(x>0)then";
        let result = format_line(line, &whitespace_flags, false);
        // Should have spaces around >
        assert!(result.contains(" > "));
    }

    #[test]
    fn test_relational_operators_eq() {
        let whitespace_flags = [
            false, false, true, false, false, false, false, false, false, false, false,
        ];
        let line = "if(x==y)then";
        let result = format_line(line, &whitespace_flags, false);
        // Should have spaces around ==
        assert!(result.contains(" == "));
    }

    #[test]
    fn test_relational_operators_dotted() {
        let whitespace_flags = [
            false, false, true, false, false, false, false, false, false, false, false,
        ];
        let line = "if(x.gt.y)then";
        let result = format_line(line, &whitespace_flags, false);
        // Should have spaces around .gt.
        assert!(result.contains(" .gt. ") || result.contains(" .GT. "));
    }

    #[test]
    fn test_logical_operators() {
        let whitespace_flags = [
            false, false, false, true, false, false, false, false, false, false, false,
        ];
        let line = "if(a.and.b)then";
        let result = format_line(line, &whitespace_flags, false);
        // Should have spaces around .and.
        assert!(result.contains(" .and. ") || result.contains(" .AND. "));
    }

    #[test]
    fn test_logical_operators_or() {
        let whitespace_flags = [
            false, false, false, true, false, false, false, false, false, false, false,
        ];
        let line = "if(a.or.b.or.c)then";
        let result = format_line(line, &whitespace_flags, false);
        // Should have spaces around .or.
        assert!(result.contains(" .or. ") || result.contains(" .OR. "));
    }

    #[test]
    fn test_adjacent_logical_operators() {
        // Test that adjacent operators like ".and..not." get single space between them
        let whitespace_flags = [
            false, false, false, true, false, false, false, false, false, false, false,
        ];

        // .and. followed directly by .not.
        let line = "if(a.and..not.b)then";
        let result = format_line(line, &whitespace_flags, false);
        // Should have single space between operators, not double
        assert!(
            result.contains(".and. .not.") || result.contains(".AND. .NOT."),
            "Expected single space between adjacent operators, got: '{result}'"
        );
        assert!(
            !result.contains(".and.  .not.") && !result.contains(".AND.  .NOT."),
            "Should not have double space between adjacent operators, got: '{result}'"
        );

        // .or. followed directly by .not.
        let line = "if(a.or..not.b)then";
        let result = format_line(line, &whitespace_flags, false);
        assert!(
            result.contains(".or. .not.") || result.contains(".OR. .NOT."),
            "Expected single space between adjacent operators, got: '{result}'"
        );
    }

    #[test]
    fn test_combined_operators() {
        let whitespace_flags = [
            true, true, true, true, true, false, false, false, false, false, false,
        ];
        let line = "if(x>0.and.y==5)x=x+1";
        let result = format_line(line, &whitespace_flags, false);
        // Should have spaces around all operators
        assert!(result.contains(" > "));
        assert!(result.contains(".and.") || result.contains(".AND."));
        assert!(result.contains(" == "));
        assert!(result.contains(" = "));
        assert!(result.contains(" + "));
    }

    #[test]
    fn test_operators_in_strings_preserved() {
        let whitespace_flags = [
            false, false, true, true, false, false, false, false, false, false, false,
        ];
        let line = r#"print *,"x>0.and.y==5""#;
        let result = format_line(line, &whitespace_flags, false);
        // Operators inside strings should NOT get spaces
        assert!(result.contains(r#""x>0.and.y==5""#));
    }

    #[test]
    fn test_fypp_inline_preservation() {
        let line = "#{if 1 > 2}#Some code#{endif}#";
        let whitespace_flags = [true; 11];
        let result = format_line(line, &whitespace_flags, false);

        // Fypp expressions should be preserved
        assert!(
            result.contains("#{"),
            "Fypp expression not preserved: '{result}'"
        );
    }

    #[test]
    fn test_fypp_inline_with_spaces() {
        // Spaces around .true. between fypp expressions should be preserved
        let line = "#{if  defined('MPI')}# .true. #{else}# .false. #{endif}#";
        let whitespace_flags = [true; 11];
        let result = format_line(line, &whitespace_flags, false);

        assert!(
            result.contains("}# .true."),
            "Space before .true. not preserved: '{result}'"
        );
    }

    #[test]
    fn test_concat_operator() {
        // Test string concatenation operator //
        let whitespace_flags = [
            false, false, false, false, false, false, false, false, false, false, true,
        ];
        let line = r"str1//str2";
        let result = format_line(line, &whitespace_flags, false);
        assert_eq!(result, "str1 // str2");
    }

    #[test]
    fn test_concat_operator_with_strings() {
        // Test concat with string literals
        let whitespace_flags = [
            false, false, false, false, false, false, false, false, false, false, true,
        ];
        let line = r#""hello"//"world""#;
        let result = format_line(line, &whitespace_flags, false);
        assert_eq!(result, r#""hello" // "world""#);
    }

    #[test]
    fn test_concat_operator_disabled() {
        // Test that concat spacing is disabled when whitespace_flags[10] is false
        let whitespace_flags = [
            false, false, false, false, false, false, false, false, false, false, false,
        ];
        let line = r"str1//str2";
        let result = format_line(line, &whitespace_flags, false);
        // Should NOT add spaces when disabled
        assert_eq!(result, "str1//str2");
    }

    #[test]
    fn test_plus_before_paren() {
        // Binary + before ( should have space: r3+(r5 -> r3 + (r5
        let line = "r3+(r5";
        let whitespace_flags = [true; 11];
        let result = format_line(line, &whitespace_flags, false);

        assert!(
            result.contains(" + ("),
            "Space between + and ( not present: '{result}'"
        );
    }

    #[test]
    fn test_declaration_spacing_preserved() {
        // When format_decl=false, preserve existing spacing before ::
        let whitespace_flags = [
            true, true, true, true, true, true, true, true, true, true, true,
        ];

        // Test that large spacing is preserved
        let line = "integer, intent(in)                                :: r";
        let result = format_line(line, &whitespace_flags, false); // format_decl=false
        assert!(
            result.contains(")                                ::"),
            "Declaration spacing not preserved: '{result}'"
        );
    }

    #[test]
    fn test_declaration_spacing_normalized() {
        // When format_decl=true, normalize spacing around ::
        let whitespace_flags = [
            true, true, true, true, true, true, true, true, true, true, true,
        ];

        // Test that spacing is normalized
        let line = "integer, intent(in)                                :: r";
        let result = format_line(line, &whitespace_flags, true); // format_decl=true
        assert_eq!(
            result, "integer, intent(in) :: r",
            "Declaration spacing not normalized"
        );
    }

    #[test]
    fn test_declaration_no_spacing_preserved() {
        // When format_decl=false and there's no spacing, keep it that way
        let whitespace_flags = [
            true, true, true, true, true, true, true, true, true, true, true,
        ];

        let line = "integer, intent(in):: r";
        let result = format_line(line, &whitespace_flags, false);
        assert!(
            result.contains(")::"),
            "No-space declaration spacing not preserved: '{result}'"
        );
    }
}
