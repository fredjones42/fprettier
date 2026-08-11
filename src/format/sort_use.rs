//! Alphabetical reordering of `use` statements and their `only:` lists.
//!
//! This runs as a whole-buffer pass before the rest of the pipeline, on raw
//! text. It works on physical lines, so reordering is just moving line blocks
//! around: indentation, line endings and trailing comments travel with the
//! statement they belong to. Anything left over-length by a collapsed `only:`
//! list is re-broken later by the normal line-splitting pass.

use crate::parser::char_filter::comment_start;
use crate::parser::patterns::{CPP_LINE_RE, USE_NAME_RE, USE_RE};
use crate::parser::stream::has_semicolon_outside_strings;

/// One logical statement: its physical lines plus what we need to sort it.
struct Unit {
    lines: Vec<String>,
    is_use: bool,
    /// Never moved and never rewritten (deactivation markers, preprocessor
    /// directives inside the statement, semicolons, unparseable module name).
    frozen: bool,
    /// Lowercased module name, empty unless `is_use`.
    key: String,
}

impl Unit {
    fn sortable(&self) -> bool {
        self.is_use && !self.frozen
    }
}

/// Reorder `use` statements and/or the names in their `only:` lists.
///
/// `sort_stmts` sorts statements alphabetically within a group; a group is a
/// run of consecutive `use` statements, ended by anything else at all — a blank
/// line, a comment, a preprocessor directive or another statement.
/// `sort_only` sorts the names in every `use ... only:` list.
#[must_use]
pub fn sort_use_statements(src: &str, sort_stmts: bool, sort_only: bool) -> String {
    // A line terminator belongs to a position, not to the statement sitting
    // there, so give every line one and take the last one back at the end.
    let unterminated = !src.is_empty() && !src.ends_with('\n');
    let terminated;
    let src = if unterminated {
        terminated = format!("{src}\n");
        terminated.as_str()
    } else {
        src
    };

    let mut units = split_into_units(src);

    if sort_only {
        for unit in &mut units {
            sort_only_list(unit);
        }
    }
    if sort_stmts {
        sort_groups(&mut units);
    }

    let mut out: String = units.into_iter().flat_map(|u| u.lines).collect();
    if unterminated {
        out.pop();
    }
    out
}

/// Split the source into logical statements, one `Unit` per statement.
fn split_into_units(src: &str) -> Vec<Unit> {
    let mut units = Vec::new();
    let mut current: Vec<String> = Vec::new();
    let mut frozen = false;
    let mut in_deactivated_block = false;
    let mut in_continuation = false;

    for line in src.split_inclusive('\n') {
        let (code, comment) = split_at_comment(body(line));

        // Deactivation markers, matching `detect_skip_format` in process.rs
        let marker = comment.trim();
        if marker.starts_with("!&<") {
            in_deactivated_block = true;
        } else if marker.starts_with("!&>") {
            frozen = true;
            in_deactivated_block = false;
        } else if marker.starts_with("!&") {
            frozen = true;
        }
        if in_deactivated_block || is_preprocessor(code) || has_semicolon_outside_strings(code) {
            frozen = true;
        }

        current.push(line.to_string());

        let trimmed = code.trim();
        if trimmed.ends_with('&') {
            in_continuation = true;
        } else if in_continuation && (trimmed.is_empty() || is_preprocessor(trimmed)) {
            // Comment-only or fypp line inside a continuation: keep reading
        } else {
            in_continuation = false;
            units.push(make_unit(std::mem::take(&mut current), frozen));
            frozen = false;
        }
    }
    if !current.is_empty() {
        units.push(make_unit(current, frozen));
    }
    units
}

fn make_unit(lines: Vec<String>, frozen: bool) -> Unit {
    let first_code = split_at_comment(body(&lines[0])).0;
    let is_use = USE_RE.is_match(first_code);
    let key = USE_NAME_RE
        .captures(first_code)
        .map(|caps| caps[1].to_ascii_lowercase());
    Unit {
        is_use,
        frozen: frozen || (is_use && key.is_none()),
        key: key.unwrap_or_default(),
        lines,
    }
}

/// Stable-sort each maximal run of consecutive sortable `use` statements.
fn sort_groups(units: &mut [Unit]) {
    let mut i = 0;
    while i < units.len() {
        if !units[i].sortable() {
            i += 1;
            continue;
        }
        let start = i;
        while i < units.len() && units[i].sortable() {
            i += 1;
        }
        units[start..i].sort_by(|a, b| a.key.cmp(&b.key));
    }
}

/// Sort the names in a `use ... only:` list, collapsing the statement onto one
/// line. Statements carrying a comment on any line but the last are left alone:
/// collapsing would have nowhere to put those comments.
fn sort_only_list(unit: &mut Unit) {
    if !unit.is_use || unit.frozen {
        return;
    }
    let Some((last_line, leading_lines)) = unit.lines.split_last() else {
        return;
    };
    if leading_lines
        .iter()
        .any(|line| !split_at_comment(body(line)).1.is_empty())
    {
        return;
    }

    let joined = join_code(&unit.lines);
    let Some((prefix, tail)) = split_at_only(&joined) else {
        return;
    };
    let mut items = split_top_level_commas(tail);
    if items.len() < 2 {
        return;
    }
    items.sort_by_key(|item| sort_key_of(item));

    let (last_code, comment) = split_at_comment(body(last_line));
    let mut out = format!(
        "{}{prefix} {}",
        leading_whitespace(&unit.lines[0]),
        items.join(", ")
    );
    if !comment.is_empty() {
        let gap_before_comment = &last_code[last_code.trim_end().len()..];
        out.push_str(gap_before_comment);
        out.push_str(comment);
    }
    out.push_str(line_ending(last_line));
    unit.lines = vec![out];
}

/// Split `use <module>, only: <tail>` into the part up to and including the
/// colon, and the item list. `only` is not a reserved word, so this walks the
/// grammar rather than searching for the keyword.
fn split_at_only(joined: &str) -> Option<(&str, &str)> {
    let name = USE_NAME_RE.find(joined)?;
    let rest = joined[name.end()..].trim_start();
    let rest = rest.strip_prefix(',')?.trim_start();
    if !rest.get(..4)?.eq_ignore_ascii_case("only") {
        return None;
    }
    let items = rest[4..].trim_start().strip_prefix(':')?;
    let prefix = &joined[..joined.len() - items.len()];
    Some((prefix.trim_end(), items))
}

/// Join the code parts of a statement's physical lines into one logical line,
/// dropping continuation ampersands. Mirrors `InputStream::next_fortran_line`.
fn join_code(lines: &[String]) -> String {
    let parts: Vec<&str> = lines
        .iter()
        .map(|line| {
            let code = split_at_comment(body(line)).0.trim();
            let code = code.strip_suffix('&').unwrap_or(code).trim_end();
            code.strip_prefix('&').unwrap_or(code).trim_start()
        })
        .filter(|code| !code.is_empty())
        .collect();
    parts.join(" ")
}

/// Split on commas that are not inside parentheses. `only:` lists contain no
/// string literals, and the parenthesized forms (`operator(+)`,
/// `assignment(=)`, `read(formatted)`) contain no commas, so depth is enough.
fn split_top_level_commas(list: &str) -> Vec<&str> {
    let mut items = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (pos, ch) in list.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => {
                items.push(list[start..pos].trim());
                start = pos + 1;
            }
            _ => {}
        }
    }
    items.push(list[start..].trim());
    items.retain(|item| !item.is_empty());
    items
}

/// Sort key for one `only:` item: a rename sorts on its local name, the one
/// that appears in the code.
fn sort_key_of(item: &str) -> String {
    let mut depth = 0i32;
    let mut chars = item.char_indices().peekable();
    let mut head = item;
    while let Some((pos, ch)) = chars.next() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            '=' if depth == 0 && chars.peek().is_some_and(|&(_, c)| c == '>') => {
                head = &item[..pos];
                break;
            }
            _ => {}
        }
    }
    head.to_ascii_lowercase()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn is_preprocessor(line: &str) -> bool {
    let trimmed = line.trim_start();
    trimmed.starts_with("#:")
        || trimmed.starts_with("$:")
        || trimmed.starts_with("@:")
        || trimmed.starts_with("#!")
        || CPP_LINE_RE.is_match(line)
}

/// The line without its terminator.
fn body(line: &str) -> &str {
    line.strip_suffix('\n')
        .map_or(line, |l| l.strip_suffix('\r').unwrap_or(l))
}

fn line_ending(line: &str) -> &str {
    &line[body(line).len()..]
}

fn leading_whitespace(line: &str) -> &str {
    &line[..line.len() - line.trim_start().len()]
}

fn split_at_comment(line: &str) -> (&str, &str) {
    comment_start(line).map_or((line, ""), |pos| (&line[..pos], &line[pos..]))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sort_stmts(src: &str) -> String {
        sort_use_statements(src, true, false)
    }

    fn sort_only(src: &str) -> String {
        sort_use_statements(src, false, true)
    }

    #[test]
    fn sorts_a_group_alphabetically() {
        assert_eq!(sort_stmts("use foo\nuse bar\n"), "use bar\nuse foo\n");
    }

    #[test]
    fn sorts_each_group_independently() {
        let src = "use foo\nuse bar\n\nuse efgh\nuse abcd\n";
        let expected = "use bar\nuse foo\n\nuse abcd\nuse efgh\n";
        assert_eq!(sort_stmts(src), expected);
    }

    #[test]
    fn both_flags_together() {
        let src = "use foo, only: baz, bar\nuse bar\n";
        let expected = "use bar\nuse foo, only: bar, baz\n";
        assert_eq!(sort_use_statements(src, true, true), expected);
    }

    #[test]
    fn disabled_by_default_leaves_source_untouched() {
        let src = "use foo, only: baz, bar\nuse bar\n";
        assert_eq!(sort_use_statements(src, false, false), src);
    }

    #[test]
    fn comment_line_breaks_a_group() {
        let src = "use foo\n! divider\nuse bar\n";
        assert_eq!(sort_stmts(src), src);
    }

    #[test]
    fn fypp_directive_breaks_a_group() {
        let src = "#:if defined('WITH_MPI')\nuse mpi\n#:else\nuse serial\n#:endif\n";
        assert_eq!(sort_stmts(src), src);
    }

    #[test]
    fn cpp_directive_breaks_a_group() {
        let src = "#ifdef WITH_MPI\nuse mpi\n#else\nuse abc\n#endif\n";
        assert_eq!(sort_stmts(src), src);
    }

    #[test]
    fn other_statement_breaks_a_group() {
        let src = "use foo\nimplicit none\nuse bar\n";
        assert_eq!(sort_stmts(src), src);
    }

    #[test]
    fn sorting_is_case_insensitive() {
        assert_eq!(sort_stmts("USE Foo\nuse bar\n"), "use bar\nUSE Foo\n");
    }

    #[test]
    fn indentation_and_comments_travel_with_the_statement() {
        let src = "   use foo  ! second\n   use bar  ! first\n";
        let expected = "   use bar  ! first\n   use foo  ! second\n";
        assert_eq!(sort_stmts(src), expected);
    }

    #[test]
    fn continuation_lines_travel_with_the_statement() {
        let src = "use foo, only: a, &\n               b\nuse bar\n";
        let expected = "use bar\nuse foo, only: a, &\n               b\n";
        assert_eq!(sort_stmts(src), expected);
    }

    #[test]
    fn module_nature_and_double_colon_parse() {
        let src = "use, intrinsic :: iso_c_binding\nuse :: alpha\n";
        let expected = "use :: alpha\nuse, intrinsic :: iso_c_binding\n";
        assert_eq!(sort_stmts(src), expected);
    }

    #[test]
    fn same_module_twice_keeps_its_order() {
        let src = "use m, only: z\nuse m, only: a\n";
        assert_eq!(sort_stmts(src), src);
    }

    #[test]
    fn deactivation_marker_freezes_a_statement() {
        let src = "use foo  !&\nuse bar\n";
        assert_eq!(sort_stmts(src), src);
    }

    #[test]
    fn deactivation_block_freezes_its_statements() {
        let src = "use foo  !&<\nuse bar\nuse abc  !&>\n";
        assert_eq!(sort_stmts(src), src);
    }

    #[test]
    fn sorts_an_only_list() {
        assert_eq!(
            sort_only("use foo, only: baz, bar\n"),
            "use foo, only: bar, baz\n"
        );
    }

    #[test]
    fn rename_sorts_on_its_local_name() {
        assert_eq!(
            sort_only("use m, only: c => x, b\n"),
            "use m, only: b, c => x\n"
        );
    }

    #[test]
    fn operator_items_keep_their_commas_intact() {
        assert_eq!(
            sort_only("use m, only: z, operator(+), assignment(=)\n"),
            "use m, only: assignment(=), operator(+), z\n"
        );
    }

    #[test]
    fn only_is_not_reserved() {
        assert_eq!(
            sort_only("use only, only: b, a\n"),
            "use only, only: a, b\n"
        );
    }

    #[test]
    fn continued_only_list_collapses_and_sorts() {
        let src = "use m, only: d, c, &\n             b, a\n";
        assert_eq!(sort_only(src), "use m, only: a, b, c, d\n");
    }

    #[test]
    fn trailing_comment_survives_the_collapse() {
        let src = "use m, only: d, &\n             c   ! why\n";
        assert_eq!(sort_only(src), "use m, only: c, d   ! why\n");
    }

    #[test]
    fn interior_comment_blocks_the_collapse() {
        let src = "use m, only: d, &   ! keep me\n             c\n";
        assert_eq!(sort_only(src), src);
    }

    #[test]
    fn statement_without_an_only_list_is_untouched() {
        let src = "use m\nuse, intrinsic :: iso_fortran_env\n";
        assert_eq!(sort_only(src), src);
    }

    #[test]
    fn sorting_an_only_list_is_idempotent() {
        let src = "use m, only: d, c, &\n             b, a\n";
        let once = sort_only(src);
        assert_eq!(sort_only(&once), once);
    }

    #[test]
    fn a_missing_final_newline_is_preserved() {
        assert_eq!(sort_stmts("use foo\nuse bar"), "use bar\nuse foo");
    }
}
