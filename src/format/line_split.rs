//! Line splitting for respecting line length limits
//!
//! Implements automatic line breaking for long Fortran lines,
//! inserting continuation markers (&) at appropriate positions.

use crate::format::aligner::F90Aligner;
use crate::parser::char_filter::{CharFilter, comment_start};

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
    let has_newline = line.ends_with('\n');
    let body = if has_newline {
        &line[..line.len() - 1]
    } else {
        line
    };

    let comment_pos = comment_start(body)?;

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
    for (pos, char) in CharFilter::code(text) {
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

/// Where to break what is left of a statement so the fragment before the break
/// fits within `max_line_length`, or `None` when it neither needs nor admits a
/// break: it already fits as the last line, or holds no break point.
fn next_chunk_end(current: &str, max_line_length: usize, indent: usize) -> Option<usize> {
    let available = max_line_length.checked_sub(indent)?;
    // A final fragment carries no trailing ` &`, hence the 2
    if current.len() + 2 <= available {
        return None;
    }
    let cont_break = find_split_position(current, available.checked_sub(2)?)?;
    // A break at either end makes no progress, and would loop forever
    if cont_break == 0 || cont_break >= current.len() || current[..cont_break].trim().is_empty() {
        return None;
    }
    Some(cont_break)
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
/// * `continuation_indent` - Column the fragments after the break start in
#[must_use]
pub fn auto_split_line(
    line: &str,
    initial_indent: usize,
    max_line_length: usize,
    continuation_indent: usize,
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
    if comment_start(stripped).is_some() {
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

    let current_indent = continuation_indent;
    let mut current = remainder;

    while !current.is_empty() {
        // What is left either fits, or has no break point in it at all — a
        // long string literal, say, or a long `a%b%c` path. Emit it as the
        // final fragment, over-length if it must be, rather than discarding
        // the breaks already found and leaving the whole statement on one
        // line.
        let Some(cont_break) = next_chunk_end(current, max_line_length, current_indent) else {
            new_lines.push(current.to_string());
            break;
        };

        new_lines.push(format!("{} &", current[..cont_break].trim_end()));
        current = current[cont_break..].trim_start();
    }

    let widest = new_lines
        .iter()
        .enumerate()
        .map(|(i, fragment)| {
            let indent = if i == 0 {
                initial_indent
            } else {
                current_indent
            };
            indent + fragment.len()
        })
        .max()
        .unwrap_or(0);
    // Bringing every line within the limit is always worth it. When that is out
    // of reach — an unbreakable string literal or `a%b%c` path is wider than
    // the budget by itself — the split is only worth making if it saves more
    // than the continuation indent it costs. Otherwise `call &` in front of a
    // 200-character call buys a line and saves one character.
    if widest > max_line_length && widest + current_indent >= initial_indent + stripped.len() {
        return None;
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

/// The offsets the aligner gives the physical `lines` of one statement,
/// relative to the statement's base indent. The entry for a line depends only
/// on the lines before it.
fn aligner_offsets(lines: &[String], indent_size: usize) -> Vec<usize> {
    let trimmed: Vec<String> = lines.iter().map(|l| l.trim().to_string()).collect();
    // Only the statement's opening words matter to the aligner here (is it a
    // declaration, is it a USE), so a rough join is enough
    let joined = trimmed
        .iter()
        .map(|l| l.strip_suffix('&').unwrap_or(l).trim_end())
        .collect::<Vec<_>>()
        .join(" ");
    let mut aligner = F90Aligner::new();
    aligner.process_logical_line(&joined, &trimmed, indent_size);
    aligner.get_lines_indent().to_vec()
}

/// The column the next pass will put a line in when it follows `emitted`, the
/// lines of the statement so far.
///
/// Asking the aligner the same question the indentation pass asks keeps a
/// freshly split line in the column a hand-written continuation would get,
/// so the split does not move on a second run.
fn next_line_offset(emitted: &[String], indent_size: usize) -> usize {
    let mut lines = emitted.to_vec();
    lines.push(String::new());
    aligner_offsets(&lines, indent_size)
        .last()
        .copied()
        .unwrap_or(indent_size)
}

/// A preprocessor directive between the lines of a statement: it keeps its
/// own column and has no say in the statement's
fn is_directive(line: &str) -> bool {
    line.trim_start().starts_with('#')
}

fn is_code(line: &str) -> bool {
    let line = line.trim();
    !line.is_empty() && !line.starts_with('!') && !is_directive(line)
}

/// The indent the statement's continuation columns are measured from, if its
/// layout is the aligner's: every continuation line sits where the aligner
/// puts it. `None` for a statement laid out some other way (by hand with
/// leading `&`, or not indented yet), whose columns are not ours to compute.
fn aligner_base(lines: &[String], indents: &[usize], indent_size: usize) -> Option<usize> {
    if lines.iter().any(|l| l.trim_start().starts_with('&')) {
        return None;
    }
    let offsets = aligner_offsets(lines, indent_size);
    let mut base = None;
    for k in (1..lines.len()).filter(|&k| is_code(&lines[k])) {
        let this = indents.get(k)?.checked_sub(*offsets.get(k)?)?;
        if *base.get_or_insert(this) != this {
            return None;
        }
    }
    base.or_else(|| indents.first().copied())
}

/// Apply line splitting to an array of lines, replacing long lines with split versions.
///
/// `lines` are the physical lines of one statement. When their layout is the
/// aligner's, each fragment broken off goes in the column the aligner gives
/// it, and the lines after a break are re-aligned, since a new break can move
/// them: the result is what the next run would make of it. Otherwise the
/// fragments of a line go in the column of the line that follows it.
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
    let mut result_lines = Vec::with_capacity(lines.len());
    let mut result_indents = Vec::with_capacity(lines.len());
    let mut result_origins = Vec::with_capacity(lines.len());

    // Strings count toward a line's visual width, so measure the raw text
    let too_long = |s: &str, indent: usize| {
        indent + s.trim_start().trim_end_matches('\n').len() > max_line_length
    };

    let base = aligner_base(lines, indents, indent_size);
    let mut split_any = false;

    for (i, line) in lines.iter().enumerate() {
        let mut indent = indents.get(i).copied().unwrap_or(0);
        // A blank line stands for a comment, which moves with the code
        if let (Some(base), true) = (base, split_any && !is_directive(line)) {
            indent = base + next_line_offset(&result_lines, indent_size);
        }

        // Detaching an inline comment comes first: it may bring the code
        // within the limit on its own, and otherwise hands the splitter a
        // shorter line to work with.
        let detached = too_long(line, indent)
            .then(|| split_inline_comment(line))
            .flatten();
        let (mut code, comment) = match detached {
            Some((code, comment)) => (code, Some(comment)),
            None => (line.clone(), None),
        };
        let first_indent = indent;
        // Every line is passed on, a blank one included: it holds the place
        // of a comment the caller writes back
        let mut emitted_whole = false;

        // Break one fragment off at a time; over-length and unsplittable
        // lines stand
        while !is_directive(line) && too_long(&code, indent) {
            let follow = if let Some(base) = base {
                // Where the break lands does not depend on the column of
                // what follows it, so find it first
                let Some(probe) =
                    auto_split_line(&code, indent, max_line_length, indent + indent_size)
                else {
                    break;
                };
                let mut emitted = result_lines.clone();
                emitted.push(probe[0].clone());
                base + next_line_offset(&emitted, indent_size)
            } else {
                // The column of the line after this one, or this line's own
                // if it is the last
                let next_is_code = lines.get(i + 1).is_some_and(|next| is_code(next));
                match indents.get(i + 1) {
                    Some(&next) if next_is_code => next,
                    _ => indent,
                }
            };
            let Some(fragments) = auto_split_line(&code, indent, max_line_length, follow) else {
                break;
            };
            split_any = true;
            if base.is_none() {
                for (n, fragment) in fragments.into_iter().enumerate() {
                    result_lines.push(fragment);
                    result_indents.push(if n == 0 { indent } else { follow });
                    result_origins.push(i);
                }
                emitted_whole = true;
                break;
            }
            // Keep the first fragment and look at the rest afresh: it may open
            // a bracket of its own, which moves what follows it again
            let stripped = code.trim_start();
            let head_len = fragments[0].trim_end().len() - 2;
            let rest = stripped[head_len..].trim_start().to_string();
            result_lines.push(fragments[0].clone());
            result_indents.push(indent);
            result_origins.push(i);
            code = rest;
            indent = follow;
        }
        if !emitted_whole {
            result_lines.push(code);
            result_indents.push(indent);
            result_origins.push(i);
        }
        if let Some(comment) = comment {
            result_lines.push(comment);
            result_indents.push(first_indent);
            result_origins.push(i);
        }
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
    fn test_unbreakable_tail_keeps_the_breaks_before_it() {
        // The tail is one 48-character token: it cannot be broken, but that is
        // no reason to abandon the break found before it and leave the whole
        // statement on one 82-character line.
        let line =
            "call sub(aaaaaaaaaa, bbbbbbbbbb, cccccccccccccccccccccccccccccccccccccccccccccccc)";
        let split = auto_split_line(line, 4, 40, 13).expect("the head is breakable");
        assert_eq!(split[0], "call sub(aaaaaaaaaa, bbbbbbbbbb, &");
        assert_eq!(split.len(), 2, "one break, then the unbreakable tail");
        assert!(
            4 + split[0].len() < 4 + line.len(),
            "the widest line must actually get narrower"
        );
    }

    #[test]
    fn test_split_not_worth_its_indent_is_declined() {
        // Breaking after `y =` leaves a 65-character path that cannot be
        // broken further, so the split costs a line and saves nothing.
        let line = "y = obj%component%subcomponent%field%another_field_here%final_value_x";
        assert!(auto_split_line(line, 4, 40, 8).is_none());
    }

    #[test]
    fn test_split_continuations_share_one_column() {
        // A statement broken into three lines must not alternate columns:
        // every fragment goes where the aligner puts a continuation.
        let line = "      real, intent(in) :: alpha, beta, gamma, delta, epsilon, zeta, eta, \
                    theta, iota, kappa, lambda, mu, nu, xi, omicron";
        let (lines, indents, _) = split_long_lines(&[line.to_string()], &[6], 60, 3);
        assert!(lines.len() > 2);
        assert!(indents[1..].iter().all(|&i| i == indents[1]));
        // Aligned on the `::`, not on a plain extra indent
        assert_eq!(indents[1], 6 + "real, intent(in) :: ".len());
        // And every line fits
        for (line, indent) in lines.iter().zip(&indents) {
            assert!(indent + line.trim_start().len() <= 60);
        }
    }

    #[test]
    fn test_split_keeps_the_column_of_existing_continuations() {
        // Re-splitting one line of an already aligned statement keeps the
        // column the other continuation lines use
        let lines = [
            "call sub(alpha, beta, gamma, delta, epsilon, zeta, eta, theta, iota, &".to_string(),
            "         kappa)".to_string(),
        ];
        let (out, indents, _) = split_long_lines(&lines, &[6, 15], 60, 3);
        assert_eq!(out.len(), 3);
        assert_eq!(indents, vec![6, 15, 15]);
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
    fn test_comment_start() {
        // Basic comment detection
        assert!(comment_start("x = 1 ! comment").is_some());
        assert!(comment_start("call foo() ! end of line").is_some());

        // No comment
        // fypp's `#!` after code is the comment, `#` included; on a line of
        // its own the `#` marks the directive line and stays
        assert_eq!(comment_start("x = 1 #! note"), Some(6));
        assert_eq!(comment_start("  #! note"), Some(3));
        assert!(comment_start("x = 1").is_none());
        assert!(comment_start("call foo()").is_none());

        // Comment marker inside string should be ignored
        assert!(comment_start("x = '!' ").is_none());
        assert!(comment_start(r#"x = "!" "#).is_none());
        assert!(comment_start("print *, 'Hello! World'").is_none());
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
