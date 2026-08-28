//! File processing and formatting pipeline.
//!
//! This module orchestrates the two-pass formatting process:
//!
//! **Pass 1 - Analysis:**
//! - Parse the input into logical lines (joining continuations)
//! - Track scope changes (IF/DO/MODULE/etc.) to determine indentation
//! - Identify manual alignment markers and deactivation directives
//!
//! **Pass 2 - Formatting:**
//! - Apply indentation based on scope depth
//! - Format whitespace around operators and punctuation
//! - Align continuation lines relative to opening delimiters
//! - Split long lines and convert case as configured
//!
//! The main entry point is [`format_file`] which processes a buffered reader
//! and writes formatted output to any `Write` implementation.

use std::borrow::Cow;
use std::collections::HashSet;
use std::io::{BufRead, BufReader, Cursor, Write};
use std::sync::LazyLock;

use anyhow::Result;
use regex::Regex;

use crate::config::{Config, MAX_LINE_LENGTH, MAX_STATEMENT_LENGTH};
use crate::format::case_convert::{convert_case, CaseSettings};
use crate::format::continuation::{
    get_manual_alignment, prepend_ampersands, remove_pre_ampersands, should_auto_align,
};
use crate::format::indenter::{F90Indenter, IndentParams};
use crate::format::line_split::split_long_lines;
use crate::format::replacements::replace_relational_operators;
use crate::format::sort_use::sort_use_statements;
use crate::format::whitespace::{format_line, format_line_with_level};
use crate::parser::char_filter::CharFilter;
use crate::parser::patterns::{
    ASSOCIATE_RE, BLK_RE, CHANGETEAM_RE, CPP_LINE_RE, CRITICAL_RE, DO_RE, ENUMTYPE_RE, ENUM_RE,
    FORALL_RE, IF_RE, INTERFACE_RE, MOD_RE, OMP_DIR_RE, PROG_RE, SELCASE_RE, STATEMENT_LABEL_RE,
    TYPE_RE, WHERE_RE,
};
use crate::parser::stream::{FortranLine, InputStream};
use crate::scope::build_scope_parser;

/// Maximum line length at which automatic line splitting is attempted.
///
/// Lines longer than this are assumed to be intentionally long (e.g., data
/// arrays, generated code) and are left unsplit to avoid breaking them.
/// This also prevents excessive processing time on pathologically long lines.
const LINE_SPLIT_THRESHOLD: usize = 1024;

/// Write `count` spaces to output.
fn write_spaces<W: Write>(output: &mut W, count: usize) -> std::io::Result<()> {
    output.write_all(" ".repeat(count).as_bytes())
}

/// The column a line's content starts at, i.e. its leading-space count.
fn indent_of(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

/// Re-base a fypp continuation line onto its directive's new indent, keeping
/// the offset it had relative to the directive's first line.
///
/// Using the continuation's own absolute indent instead would re-add
/// `base_indent` on every run, so the block would walk further right each
/// time the file was formatted.
fn rebase_fypp_continuation(line: &str, first_line: &str, base_indent: usize) -> String {
    let offset_from_first = indent_of(line).saturating_sub(indent_of(first_line));
    format!(
        "{}{}",
        " ".repeat(base_indent + offset_from_first),
        line.trim_start()
    )
}

/// Check whether a line ends with a continuation `&` that is real code,
/// i.e. not inside a string or comment (a `&` inside a string is content).
fn ends_with_continuation(line: &str) -> bool {
    let trimmed = line.trim_end();
    if !trimmed.ends_with('&') {
        return false;
    }
    // Check if the trailing & is visible to CharFilter (outside strings)
    let mut last_amp_outside_string = None;
    for (pos, c) in CharFilter::new(trimmed, true, true, true) {
        if c == '&' {
            last_amp_outside_string = Some(pos);
        }
    }
    last_amp_outside_string.is_some_and(|p| p == trimmed.len() - 1)
}

/// Check if a logical line is blank (no code, comments, or OMP prefix).
///
/// Both `inspect_file` and `format_pass` skip consecutive blank lines using
/// this predicate; they must agree or their line counts drift out of sync.
fn is_blank_line(fortran_line: &FortranLine) -> bool {
    fortran_line.joined_line.trim().is_empty()
        && fortran_line
            .comments
            .iter()
            .all(std::string::String::is_empty)
        && fortran_line.omp_prefix.is_empty()
}

/// Result of inspecting a Fortran file for indentation info
#[derive(Debug)]
struct InspectResult {
    /// Requested indents for each Fortran line
    required_indents: Vec<usize>,
    /// Indent level of first non-empty line
    first_indent: usize,
}

// =============================================================================
// Context structs to reduce function parameter counts
// =============================================================================

/// Pass-level context shared across all lines in a formatting pass
struct PassContext<'a> {
    /// Configuration settings
    config: &'a Config,
    /// Whitespace formatting flags (11 boolean options)
    whitespace_flags: [bool; 11],
    /// Whether to apply indentation in this pass
    impose_indent: bool,
    /// Whether to apply whitespace formatting in this pass
    impose_whitespace: bool,
    /// Pre-computed indentation info from inspection pass
    inspect_result: Option<&'a InspectResult>,
}

/// Line-level flags controlling formatting behavior
#[derive(Debug, Clone, Copy)]
struct FormattingFlags {
    /// Line is a fypp preprocessor directive
    is_fypp_line: bool,
    /// Line is a C preprocessor directive
    is_cpp_line: bool,
    /// Formatting is disabled for this line (via !& marker)
    skip_format: bool,
    /// Use automatic alignment (no leading & on continuation lines)
    auto_align: bool,
}

/// Label-related strings extracted from a Fortran line
struct LineLabels {
    /// The joined logical line with label removed
    joined_no_label: String,
    /// The first physical line with label removed
    first_no_label: String,
    /// The label extracted from first physical line
    first_label: String,
    /// The label extracted from joined line
    label: String,
    /// Indentation shift due to label normalization
    label_shift: usize,
}

/// Context for writing output lines
struct LineWriteContext<'a> {
    /// Computed indentation for each line
    computed_indents: &'a [usize],
    /// Whether each original line was indented
    lines_were_indented: &'a [bool],
    /// Output line indices that should have comments
    comment_line_indices: &'a HashSet<usize>,
    /// Origin indices that were split into multiple lines
    split_origins: &'a HashSet<usize>,
    /// Effective line length for wrapping decisions
    effective_line_length: usize,
}

/// Apply an adjustment to indent values, clamping to non-negative
///
/// Used for label continuation indent adjustments where we need signed arithmetic.
#[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
fn adjust_indent(indent: usize, adjustment: isize) -> usize {
    (indent as isize + adjustment).max(0) as usize
}

/// Inspect a Fortran file to compute requested indents
///
/// This pre-pass determines how much indentation each line should receive.
/// For IF/DO statements that are already correctly aligned, we preserve
/// their indentation (`required_indents` = 0). For other statements, we use
/// the full `indent_size`.
///
/// Note: The `required_indents` values are accessed with a 1-based offset in
/// `format_pass` (`fortran_line_number` starts at 1), which shifts all values by one position.
fn inspect_file<R: BufRead>(
    input: R,
    indent_size: usize,
    strict_indent: bool,
) -> Result<InspectResult> {
    let mut required_indents = Vec::new();
    let mut stream = InputStream::new(input);
    let mut prev_offset: usize = 0;
    let mut first_indent: Option<usize> = None;
    let mut skip_blank = false; // Track consecutive blank lines
    let mut prev_was_scope_opener = false; // Track if previous line was a non-IF/DO scope opener

    while let Some(fortran_line) = stream.next_fortran_line()? {
        // Skip empty lines
        if fortran_line.lines.is_empty() {
            continue;
        }

        let is_blank = is_blank_line(&fortran_line);

        // Skip consecutive blank lines (same as formatting pass)
        if is_blank && skip_blank {
            continue;
        }
        skip_blank = is_blank;

        // Blank lines are transparent: they get no entry (`format_pass` counts
        // only lines with content) and they leave the trackers below alone.
        // Otherwise a blank line would both displace every entry under it and
        // report an indent of 0, costing the next IF/DO its stacking.
        if is_blank {
            continue;
        }

        // Calculate offset (leading spaces) of first line
        let first_line = &fortran_line.lines[0];
        let offset = indent_of(first_line);

        // Determine first_indent from first non-empty Fortran line
        let joined_trimmed = fortran_line.joined_line.trim();
        if !joined_trimmed.is_empty() && first_indent.is_none() {
            // If first line is PROGRAM or MODULE, first_indent = 0
            if PROG_RE.is_match(joined_trimmed) || MOD_RE.is_match(joined_trimmed) {
                first_indent = Some(0);
            } else {
                first_indent = Some(offset);
            }
        }

        // Determine requested indent for this line
        // Default: offset - prev_offset (delta from previous line)
        let mut required_indent = offset.saturating_sub(prev_offset);

        // For IF/DO statements, preserve existing indentation if correctly aligned
        // Disallow stacking (delta=0) when the previous line was a scope-opener
        // (like ASSOCIATE, SELECT, etc.), so the body gets properly indented.
        if IF_RE.is_match(joined_trimmed) || DO_RE.is_match(joined_trimmed) {
            // The `indent_size > 0` guard is load-bearing: is_multiple_of(0)
            // is true only for 0, so dropping it would call an indent of 0
            // misaligned for every line that is not at column 0
            let indent_misaligned = indent_size > 0 && !offset.is_multiple_of(indent_size);
            if prev_offset != offset || strict_indent || indent_misaligned || prev_was_scope_opener
            {
                required_indent = indent_size;
            }
            // Otherwise keep required_indent as delta (usually 0 if same offset)
        } else {
            // For non-IF/DO statements, always use indent_size
            required_indent = indent_size;
        }

        // Track whether this line is a block scope opener (not IF/DO, not
        // module-level constructs like PROGRAM/MODULE/SUBROUTINE/FUNCTION).
        // Only includes constructs whose body is always indented.
        prev_was_scope_opener = opens_indented_scope(joined_trimmed);

        required_indents.push(required_indent);
        prev_offset = offset;
    }

    Ok(InspectResult {
        required_indents,
        first_indent: first_indent.unwrap_or(0),
    })
}

/// Whether a statement opens a scope whose body is indented.
///
/// `inspect_file` uses this to stop an IF or DO on the next line from being
/// read as stacked against it: written in the same column as the statement
/// that opened its scope, an IF is a new level, not a continuation of one.
///
/// The scope openers are the ones from [`crate::scope::SCOPES`], less IF and
/// DO — those two are what stacking is about — and less the program units
/// (PROGRAM, MODULE, SUBMODULE, SUBROUTINE, FUNCTION), where an IF written
/// flush with the header is deliberately left there; see
/// `test_end_to_end_formatting`. An END never opens anything, and some of
/// the patterns match one, so END is ruled out first.
fn opens_indented_scope(statement: &str) -> bool {
    let trimmed = statement.trim_start();
    if trimmed
        .get(..3)
        .is_some_and(|start| start.eq_ignore_ascii_case("end"))
    {
        return false;
    }

    [
        &*INTERFACE_RE,
        &*TYPE_RE,
        &*ENUM_RE,
        &*ENUMTYPE_RE,
        &*ASSOCIATE_RE,
        &*SELCASE_RE,
        &*BLK_RE,
        &*CRITICAL_RE,
        &*CHANGETEAM_RE,
        &*WHERE_RE,
        &*FORALL_RE,
    ]
    .iter()
    .any(|re| re.is_match(trimmed))
}

/// Fypp line directive pattern - matches lines starting with #!, #:, $:, or @:
static FYPP_LINE_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^\s*(#!|#:|\$:|@:)").unwrap());

/// Extract statement label from a line and return (label, `line_without_label`)
///
/// Statement labels are numeric prefixes like "100 " at the start of a line.
/// The label is extracted and replaced with spaces to preserve column positions.
fn extract_label(line: &str) -> (String, String) {
    if let Some(caps) = STATEMENT_LABEL_RE.captures(line) {
        if let Some(label_match) = caps.get(1) {
            let label = label_match.as_str().to_string();
            // Replace only the label part with spaces (preserve column positions)
            // The regex matches label+space+one_more_char, but we only want to replace label+space
            let prefix_len = label_match.start();
            let prefix = &line[..prefix_len];
            let rest = &line[label_match.end()..];
            let line_without_label = format!("{}{}{}", prefix, " ".repeat(label.len()), rest);
            return (label, line_without_label);
        }
    }
    (String::new(), line.to_string())
}

/// Format a Fortran file with two-pass processing
///
/// Pass 1: Whitespace formatting (if `impose_whitespace` is true)
/// Pass 2: Indentation (if `impose_indent` is true)
/// Case conversion is applied in whichever pass runs (or a dedicated pass if neither)
/// Diagnose input that already breaks the free source form limits on line
/// (6.3.2.1) and statement (6.3.2.6) length.
///
/// Such input is still formatted — fprettier is not a compiler — but a
/// formatter is the right place to notice, and no reformatting can bring a
/// single over-long line back under the limit.
fn over_limit_warnings(src: &str) -> Vec<String> {
    let mut warnings = Vec::new();
    let mut statement_start = 0;
    let mut statement_length = 0;
    let mut statement_reported = false;

    for (i, line) in src.lines().enumerate() {
        let line = line.trim_end_matches('\r');
        let length = line.chars().count();

        if length > MAX_LINE_LENGTH {
            warnings.push(format!(
                "line {} is {length} characters, over the free-form maximum of {MAX_LINE_LENGTH}",
                i + 1
            ));
        }

        if statement_length == 0 {
            statement_start = i + 1;
            statement_reported = false;
        }
        statement_length += length;
        if statement_length > MAX_STATEMENT_LENGTH && !statement_reported {
            warnings.push(format!(
                "the statement at line {statement_start} is over the maximum of \
                 {MAX_STATEMENT_LENGTH} characters"
            ));
            statement_reported = true;
        }

        // A trailing `&` continues the statement onto the next line
        if !line.trim_end().ends_with('&') {
            statement_length = 0;
        }
    }

    warnings
}

pub fn format_file<R: BufRead, W: Write>(input: R, output: &mut W, config: &Config) -> Result<()> {
    // Check if case conversion is enabled
    let case_settings = CaseSettings::from_dict(&config.case_dict);
    let case_enabled = case_settings.is_enabled();

    // Check if line splitting is needed
    let line_split_needed = config.line_length > 0 && config.line_length < LINE_SPLIT_THRESHOLD;

    // Check if comment spacing normalization is enabled
    let normalize_comment_spacing_needed = config.normalize_comment_spacing;

    // Read input into buffer so we can inspect and process it
    let mut input_buffer = Vec::new();
    let mut reader = input;
    reader.read_to_end(&mut input_buffer)?;
    let crlf_input = input_buffer.windows(2).any(|pair| pair == b"\r\n");

    // Only worth scanning when the input is long enough to break a limit
    if input_buffer.len() > MAX_LINE_LENGTH {
        for warning in over_limit_warnings(&String::from_utf8_lossy(&input_buffer)) {
            eprintln!("Warning: {warning}");
        }
    }

    // Reorder `use` statements before anything else, so the passes below see the
    // final line order and fix up whatever the reordering left over-length.
    if config.sort_use || config.sort_use_only {
        if let Ok(src) = std::str::from_utf8(&input_buffer) {
            let sorted = sort_use_statements(src, config.sort_use, config.sort_use_only);
            input_buffer = sorted.into_bytes();
        }
    }

    // Pass 1: Whitespace formatting
    let intermediate = if config.impose_whitespace {
        let cursor = Cursor::new(&input_buffer);
        let pass1_reader = BufReader::new(cursor);
        let mut pass1_output = Vec::new();
        format_pass(
            pass1_reader,
            &mut pass1_output,
            config,
            false, // Don't impose indent in pass 1
            true,  // Do impose whitespace in pass 1
            None,  // No required_indents for whitespace pass
        )?;
        pass1_output
    } else {
        // No whitespace pass, use input directly
        input_buffer
    };

    // Inspect the buffer the indentation pass is about to read, not the
    // original: pass 1 collapses runs of blank lines and detaches comments, so
    // inspecting the original would pair every line below such a change with
    // another line's requested indent.
    // Pass 1 preserves leading whitespace, which is all the inspection reads.
    let inspect_result = if config.impose_indent && !config.strict_indent {
        let cursor = Cursor::new(&intermediate);
        let inspect_reader = BufReader::new(cursor);
        Some(inspect_file(
            inspect_reader,
            config.indent,
            config.strict_indent,
        )?)
    } else {
        None
    };

    // Pass 2: Indentation
    let mut formatted = Vec::with_capacity(intermediate.len());
    if config.impose_indent {
        let cursor = Cursor::new(intermediate);
        let reader = BufReader::new(cursor);
        format_pass(
            reader,
            &mut formatted,
            config,
            true,  // Do impose indent in pass 2
            false, // Don't impose whitespace in pass 2
            inspect_result.as_ref(),
        )?;
    } else if !config.impose_whitespace
        && (case_enabled || line_split_needed || normalize_comment_spacing_needed)
    {
        // Neither whitespace nor indent, but case/line-split/comment-spacing enabled
        // Run a pass for these features
        let cursor = Cursor::new(intermediate);
        let reader = BufReader::new(cursor);
        format_pass(
            reader,
            &mut formatted,
            config,
            false, // No indent
            false, // No whitespace (case conversion and line splitting will still run)
            None,
        )?;
    } else {
        // No indentation pass, use the intermediate output directly
        formatted = intermediate;
    }

    write_lines(output, &formatted, crlf_input)?;

    Ok(())
}

/// Write the formatted bytes, restoring CRLF line endings if the input used
/// them.
///
/// The parser strips `\r` along with `\n`, so every formatted line ends with a
/// bare `\n`; without this, formatting a file would rewrite every one of its
/// line endings. A file with mixed endings is normalized to CRLF, the ending
/// it uses anywhere.
fn write_lines<W: Write>(output: &mut W, formatted: &[u8], crlf: bool) -> std::io::Result<()> {
    if !crlf {
        return output.write_all(formatted);
    }

    for line in formatted.split_inclusive(|&byte| byte == b'\n') {
        match line.strip_suffix(b"\n") {
            Some(body) => {
                output.write_all(body)?;
                output.write_all(b"\r\n")?;
            }
            // Trailing content with no line ending to restore
            None => output.write_all(line)?,
        }
    }

    Ok(())
}

/// Check for deactivation markers in comments
///
/// Returns whether to skip formatting for this line.
/// - "!&" at end of line: skip formatting for that line only
/// - "!&<": start block deactivation (skip formatting until !&>)
/// - "!&>": end block deactivation
fn detect_skip_format(comments: &[String], in_deactivation_block: &mut bool) -> bool {
    let mut skip_format = *in_deactivation_block;

    for comment in comments {
        let trimmed = comment.trim();
        if trimmed.starts_with("!&<") {
            // Start of deactivation block
            *in_deactivation_block = true;
            skip_format = true;
        } else if trimmed.starts_with("!&>") {
            // End of deactivation block - still skip this line
            skip_format = true;
            *in_deactivation_block = false;
        } else if trimmed.starts_with("!&") {
            // Single line deactivation
            skip_format = true;
        }
    }

    skip_format
}

/// Extract pre-ampersands and apply whitespace formatting to continuation lines
///
/// When lines have leading `&` (manual alignment), this extracts the ampersand prefixes,
/// applies whitespace formatting to continuation lines, and returns alignment info.
///
/// Returns `(pre_ampersand, ampersand_sep, manual_lines_indent)`.
fn extract_and_format_pre_ampersands(
    output_lines: &mut Vec<String>,
    fortran_line: &FortranLine,
    pass_ctx: &PassContext<'_>,
    flags: FormattingFlags,
) -> (Vec<String>, Vec<usize>, Option<Vec<usize>>) {
    // Only process in the indentation pass and when not skipped
    if flags.auto_align || output_lines.len() <= 1 || !pass_ctx.impose_indent || flags.skip_format {
        return (vec![], vec![], None);
    }

    // Get manual alignment before modifying lines
    // Pass continuation_indent to normalize simple continuations
    let manual_indent = get_manual_alignment(output_lines, pass_ctx.config.indent);

    // Extract is_special based on fypp lines AND lines inside multiline strings
    // Lines inside multiline strings must be preserved as-is
    // When the first line is a fypp directive, ALL continuation lines are special
    let is_special: Vec<bool> = output_lines
        .iter()
        .enumerate()
        .map(|(i, line)| {
            // If first line is fypp, all continuation lines are special
            let is_fypp_continuation = flags.is_fypp_line && i > 0;
            // Or if this specific line is a fypp directive
            let is_fypp = i > 0 && FYPP_LINE_RE.is_match(line.trim_start());
            is_fypp_continuation || is_fypp || fortran_line.starts_in_string(i)
        })
        .collect();

    // Remove pre-ampersands from lines
    let result = remove_pre_ampersands(output_lines, &is_special);

    *output_lines = result.lines;

    // Apply whitespace formatting to continuation lines (i > 0) now that
    // the leading & prefix has been stripped. These lines were skipped in
    // Pass 1 (whitespace pass) because remove_pre_ampersands only runs here.
    if pass_ctx.config.impose_whitespace && !flags.skip_format {
        let mut bracket_level: usize = 0;
        // Get the last significant character from line 0 (already formatted in Pass 1)
        // This determines if leading +/- on line 1 is binary or unary
        let mut prev_line_last_char: Option<char> = output_lines.first().and_then(|line0| {
            // Find last non-space char before trailing & (if any)
            let trimmed = line0.trim_end().trim_end_matches('&').trim_end();
            // Use CharFilter to skip strings and comments
            let mut last_char = None;
            for (_, c) in CharFilter::new(trimmed, true, true, true) {
                if !c.is_whitespace() {
                    last_char = Some(c);
                }
            }
            last_char
        });
        for (i, line) in output_lines.iter_mut().enumerate() {
            if i == 0 {
                continue; // First line was already formatted in Pass 1
            }
            if is_special.get(i).copied().unwrap_or(false) {
                continue; // Skip special (fypp) lines
            }
            if fortran_line.starts_in_string(i) {
                continue;
            }
            // Format the line content (& has been stripped)
            // Only treat trailing & as continuation if it's NOT inside a string
            let has_continuation = ends_with_continuation(line);
            let line_to_format: Cow<str> = if has_continuation {
                Cow::Owned(line.trim_end().trim_end_matches('&').to_string())
            } else {
                Cow::Borrowed(line)
            };
            let (formatted, new_level, last_char) = format_line_with_level(
                &line_to_format,
                &pass_ctx.whitespace_flags,
                pass_ctx.config.format_decl,
                bracket_level,
                prev_line_last_char,
            );
            bracket_level = new_level;
            // Only update prev_line_last_char if line had code content
            // (comment-only lines return None, preserve previous value)
            if last_char.is_some() {
                prev_line_last_char = last_char;
            }
            *line = if has_continuation {
                // Use ampersand_sep to preserve original spacing before trailing &
                // ampersand_sep[i] is the spacing for line i's trailing &
                // (captured when processing line i+1 which has leading &)
                let sep = result.ampersand_sep.get(i).copied().unwrap_or(1);
                format!("{}{}&", formatted.trim_end(), " ".repeat(sep))
            } else {
                formatted
            };
        }
    }

    (
        result.pre_ampersand,
        result.ampersand_sep,
        Some(manual_indent),
    )
}

/// Apply whitespace formatting to output lines
///
/// Handles both single-line and multi-line (continuation) formatting.
/// For multi-line cases, formats each physical line separately while
/// tracking bracket levels for proper keyword argument spacing.
#[allow(clippy::ptr_arg)]
fn apply_whitespace_to_lines(
    output_lines: &mut Vec<String>,
    fortran_line: &FortranLine,
    pass_ctx: &PassContext<'_>,
    labels: &LineLabels,
    flags: FormattingFlags,
    ampersand_sep: &[usize],
) {
    if pass_ctx.impose_whitespace && !flags.skip_format && !flags.is_fypp_line && !flags.is_cpp_line
    {
        // If there are multiple physical lines (continuations), format each line separately
        // to preserve the continuation structure
        // Track bracket level across continuation lines for proper keyword argument spacing
        if output_lines.len() > 1 {
            // Strip label from first line before formatting (preserves column positions)
            if !labels.first_label.is_empty() && !output_lines.is_empty() {
                output_lines[0].clone_from(&labels.first_no_label);
            }

            let mut bracket_level: usize = 0;
            let mut prev_line_last_char: Option<char> = None;
            for (i, line) in output_lines.iter_mut().enumerate() {
                // When lines have leading & (auto_align=false), skip formatting here.
                // The leading & hasn't been stripped yet (remove_pre_ampersands runs
                // in the indent pass), so formatting would break the prefix.
                // These lines get formatted in the indent pass instead.
                if !flags.auto_align && i > 0 {
                    continue;
                }

                // Check if this specific physical line is a fypp directive
                // If so, skip formatting and preserve original content
                let is_line_fypp = FYPP_LINE_RE.is_match(line.trim_start());
                if is_line_fypp {
                    // Preserve fypp directive lines as-is (no whitespace formatting)
                    // Note: OMP prefix is handled during indentation, not here
                    continue;
                }

                // Preserve lines inside multiline strings as-is
                if fortran_line.starts_in_string(i) {
                    continue;
                }

                // Remove continuation marker for formatting, then restore
                // Note: Only treat trailing & as continuation if it's OUTSIDE strings
                // (A & inside a string is part of string content, not a continuation marker)
                let has_continuation = ends_with_continuation(line);
                let line_content = if has_continuation {
                    line.trim_end().trim_end_matches('&').trim_end()
                } else {
                    line.trim_end()
                };

                // Format this physical line with bracket level tracking
                // Pass prev_line_last_char for continuation lines to determine +/- treatment
                let (formatted, ending_level, last_char) = format_line_with_level(
                    line_content,
                    &pass_ctx.whitespace_flags,
                    pass_ctx.config.format_decl,
                    bracket_level,
                    if i > 0 { prev_line_last_char } else { None },
                );
                bracket_level = ending_level;
                // Only update prev_line_last_char if line had code content
                // (comment-only lines return None, preserve previous value)
                if last_char.is_some() {
                    prev_line_last_char = last_char;
                }

                // Restore continuation marker if needed
                // When lines have leading & (auto_align=false), preserve original spacing
                // before trailing & using ampersand_sep or extracting from original line
                if has_continuation {
                    let spacing = if !ampersand_sep.is_empty() && i < ampersand_sep.len() {
                        // Use captured spacing from extraction
                        ampersand_sep[i]
                    } else if !flags.auto_align && i == 0 {
                        // For first line when auto_align=false, extract original spacing
                        // to preserve manual formatting
                        let original = if i < fortran_line.lines.len() {
                            &fortran_line.lines[i]
                        } else {
                            line.as_str()
                        };
                        // Count spaces before trailing &
                        let trimmed = original.trim_end();
                        if let Some(amp_pos) = trimmed.rfind('&') {
                            let before_amp = &trimmed[..amp_pos];
                            before_amp.len() - before_amp.trim_end().len()
                        } else {
                            1
                        }
                    } else {
                        // Default: 1 space before &
                        1
                    };
                    *line = format!("{}{}&", formatted.trim_end(), " ".repeat(spacing));
                } else {
                    *line = formatted;
                }
                // Note: OMP prefix is handled during indentation, not here
            }
        } else {
            // Single line - format the whole joined line
            let formatted = format_line(
                &labels.joined_no_label,
                &pass_ctx.whitespace_flags,
                pass_ctx.config.format_decl,
            );
            if !output_lines.is_empty() {
                // Note: OMP prefix is handled during indentation, not here
                output_lines[0] = formatted;
            }
        }
    } else if !labels.first_label.is_empty() && !output_lines.is_empty() {
        // Even without whitespace formatting, strip label for consistent handling
        output_lines[0].clone_from(&labels.first_no_label);
    }
}

/// Re-base the continuation indents of a labeled statement.
///
/// The indenter works on label-stripped lines, so it computes continuation
/// indents relative to a first line starting at column 0. In the output that
/// line is `label + padding + content`, so its content really starts at
/// `max(label.len(), base_indent)` and the continuations have to follow it
/// there.
///
/// Manual alignment (a leading `&`) strips the base indent instead: those
/// lines keep just their manual indent, and `prepend_ampersands` re-applies
/// the base along with `label_shift`.
fn shift_indents_for_label(
    computed_indents: &mut [usize],
    output_lines: &[String],
    label: &str,
    has_pre_amp: bool,
) {
    if label.is_empty() || computed_indents.is_empty() {
        return;
    }
    // Leading whitespace on the first line is what encodes the label's column
    if output_lines.first().map_or(0, |line| indent_of(line)) == 0 {
        return;
    }

    let base_indent = computed_indents[0];
    if has_pre_amp {
        for ind in computed_indents.iter_mut().skip(1) {
            *ind = ind.saturating_sub(base_indent);
        }
        return;
    }

    #[allow(clippy::cast_possible_wrap)] // indents are columns, far below isize::MAX
    let adjustment = label.len().max(base_indent) as isize - base_indent as isize;
    for ind in computed_indents.iter_mut().skip(1) {
        *ind = adjust_indent(*ind, adjustment);
    }
}

/// Split the statement label off a logical line.
///
/// `normalize` collapses the run of spaces after the label so the statement
/// starts exactly `label.len()` columns in, and records how far that moved it
/// as `label_shift` — a shift the continuation indents have to follow. It is
/// only ever done on the indent pass: pass 1's output is pass 2's input, so
/// normalizing earlier would erase the original spacing the shift is measured
/// from. The first line of the file keeps its spacing either way.
fn split_off_label(
    fortran_line: &FortranLine,
    output_lines: &[String],
    normalize: bool,
) -> LineLabels {
    let (label, joined_no_label) = extract_label(&fortran_line.joined_line);
    let (first_label, first_no_label) = output_lines.first().map_or_else(
        || (String::new(), String::new()),
        |line| extract_label(line),
    );

    if !normalize || label.is_empty() {
        return LineLabels {
            joined_no_label,
            first_no_label,
            first_label,
            label,
            label_shift: 0,
        };
    }

    // Measured against the ORIGINAL first line, not `output_lines`, which
    // pass 1 may already have reflowed.
    let original_leading = fortran_line
        .lines
        .first()
        .map_or(0, |line| indent_of(&extract_label(line).1));
    let target_spaces = label.len();
    let strip_excess = |line: String| match indent_of(&line) {
        leading if leading > target_spaces => line[(leading - target_spaces)..].to_string(),
        _ => line,
    };

    LineLabels {
        joined_no_label: strip_excess(joined_no_label),
        first_no_label: strip_excess(first_no_label),
        first_label,
        label_shift: original_leading.saturating_sub(target_spaces),
        label,
    }
}

/// Compute and apply indentation to output lines
///
/// Processes the logical line through the indenter, computes indentation levels,
/// and applies them to the output lines. Handles special cases for labels,
/// fypp directives, and OMP prefixes.
#[allow(clippy::too_many_arguments, clippy::ptr_arg)]
fn compute_and_apply_indentation(
    output_lines: &mut Vec<String>,
    computed_indents: &mut Vec<usize>,
    indenter: &mut F90Indenter,
    fortran_line: &FortranLine,
    pass_ctx: &PassContext<'_>,
    labels: &LineLabels,
    fortran_line_number: usize,
    pre_ampersand: &[String],
    manual_lines_indent: Option<&[usize]>,
    is_fypp_line: bool,
) {
    // Get requested indent for this Fortran line from inspection result
    // Falls back to config.indent if no inspection or index out of bounds
    let relative_indent = pass_ctx
        .inspect_result
        .and_then(|r| r.required_indents.get(fortran_line_number).copied())
        .unwrap_or(pass_ctx.config.indent);

    // Build indent params
    let indent_params = IndentParams {
        relative_indent,
        continuation_indent: pass_ctx.config.indent,
        indent_fypp: pass_ctx.config.indent_fypp,
        manual_lines_indent,
        semicolon_line_index: fortran_line.semicolon_line_index,
        label: &labels.label,
    };

    // Process the logical line for indentation (without label)
    // Use output_lines (which may have been formatted) for alignment computation
    indenter.process_logical_line(&labels.joined_no_label, output_lines, &indent_params);

    // Get computed indents and save for comment handling
    let indents = indenter.get_lines_indent();
    *computed_indents = indents.to_vec();

    shift_indents_for_label(
        computed_indents,
        output_lines,
        &labels.label,
        !pre_ampersand.is_empty(),
    );

    // Check if this is a multi-line fypp directive (first line is fypp + continuation)
    // If so, preserve original indent for ALL lines
    let is_multiline_fypp_directive = is_fypp_line && output_lines.len() > 1;

    // Apply indents to output lines
    // When we have pre_ampersand (lines with leading &), skip applying indent here
    // because prepend_ampersands needs the lines without indent applied
    let has_pre_ampersand = !pre_ampersand.is_empty();

    for (i, line) in output_lines.iter_mut().enumerate() {
        if i >= computed_indents.len() {
            continue;
        }

        // Skip continuation lines when we have pre_ampersand
        // (first line still gets indent, continuation lines are handled later)
        // For fypp lines with continuations, use per-line check since fypp
        // continuation lines are marked as special and have empty pre_ampersand
        let should_skip = if is_fypp_line {
            // For fypp: only skip if THIS line has pre_ampersand
            let line_has_pre_amp = pre_ampersand.get(i).is_some_and(|s| !s.is_empty());
            line_has_pre_amp && i > 0
        } else {
            // For non-fypp: skip ALL continuation lines if any has leading &
            has_pre_ampersand && i > 0
        };
        if should_skip {
            continue;
        }

        // Determine if this line should preserve original indent
        // Rules for fypp lines:
        // 1. When indent_fypp=True (default):
        //    - First line (i=0) of fypp directive: use scope-based indent
        //    - Continuation lines (i>0) of fypp directive: preserve original indent
        // 2. When indent_fypp=False:
        //    - All fypp directive lines preserve original indent
        let is_line_fypp = FYPP_LINE_RE.is_match(line.trim_start());

        let preserve_original_indent =
            // Continuation lines of multiline fypp directives preserve original
            (is_multiline_fypp_directive && i > 0)
            // Any fypp line preserves original when indent_fypp=False
            // Continuation fypp lines (i>0) also preserve original
            || (is_line_fypp && (!pass_ctx.config.indent_fypp || i > 0));

        if preserve_original_indent {
            // For fypp continuation lines when indent_fypp=True:
            // Add scope-based indent to original relative indent
            // Example: input "    & content" (4 spaces) + scope indent (6)
            //          -> output "          & content" (10 spaces)
            if pass_ctx.config.indent_fypp && is_multiline_fypp_directive && i > 0 {
                let first_line = fortran_line.lines.first().map_or("", String::as_str);
                let base_indent = computed_indents.first().copied().unwrap_or(0);
                *line = rebase_fypp_continuation(line, first_line, base_indent);
            }
            // Otherwise preserve original indentation as-is
            continue;
        }

        // For labeled lines (i == 0 with label), preserve leading whitespace
        // The label handling code at write time uses these spaces for padding
        if i == 0 && !labels.label.is_empty() {
            // Don't strip/re-apply indent for labeled first line
            // The leading spaces encode the column position after label
            continue;
        }

        let mut indent = computed_indents[i];
        // Remove existing leading whitespace
        let trimmed = line.trim_start();

        // For continuation lines starting with &, adjust indent by -1
        // The & is a continuation marker, so content should align after bracket
        if i > 0 && trimmed.starts_with('&') && indent > 0 {
            indent -= 1;
        }

        // Apply new indent - but don't add spaces to empty lines
        if trimmed.is_empty() {
            *line = String::new();
        } else {
            // Handle OMP conditional prefix specially
            // OMP conditional (!$ ) is written at column 0, with padding
            // to align the code with the expected indent
            // Note: ALL lines in an OMP conditional block have the !$ prefix
            if fortran_line.omp_prefix.is_empty() {
                *line = format!("{}{}", " ".repeat(indent), trimmed);
            } else {
                let omp_len = fortran_line.omp_prefix.len();
                let padding = indent.saturating_sub(omp_len);
                *line = format!(
                    "{}{}{}",
                    fortran_line.omp_prefix,
                    " ".repeat(padding),
                    trimmed
                );
            }
        }
    }
}

/// Write a single output line with labels, comments, and proper indentation
///
/// Handles all the complexity of writing a line including:
/// - Label prefixing for first lines
/// - FORD documentation comment preservation
/// - Comment attachment/detachment based on line length
/// - OMP directive handling
#[allow(clippy::too_many_arguments)]
fn write_output_line<W: Write>(
    output: &mut W,
    line: &str,
    line_index: usize,
    origin: usize,
    fortran_line: &FortranLine,
    pass_ctx: &PassContext<'_>,
    labels: &LineLabels,
    write_ctx: &LineWriteContext<'_>,
    indenter: Option<&F90Indenter>,
) -> std::io::Result<()> {
    let has_comment = origin < fortran_line.comments.len()
        && !fortran_line.comments[origin].is_empty()
        && write_ctx.comment_line_indices.contains(&line_index);

    // Determine what to write for the line portion
    // If there's a comment, trim trailing spaces from the line since the
    // comment provides the separation
    let line_to_write = if has_comment {
        let trimmed = line.trim_end();
        // If the line is now empty (comment-only) but was originally indented,
        // preserve one space as a marker so that Pass 2 of two-pass formatting
        // can detect the original indentation. Only when an indent pass is
        // actually coming: with indentation off nothing consumes the marker
        // and the comment gains a space on every run.
        if trimmed.is_empty()
            && pass_ctx.config.impose_indent
            && origin < write_ctx.lines_were_indented.len()
            && write_ctx.lines_were_indented[origin]
        {
            " "
        } else {
            trimmed
        }
    } else {
        line
    };

    // Check if the original line was indented (started with space)
    // Used for comment-only lines to decide indentation behavior
    // Use pre-computed lines_were_indented to handle two-pass formatting correctly
    // (in Pass 2, fortran_line.lines comes from Pass 1 output which may have lost whitespace)
    let original_line = if origin < fortran_line.lines.len() {
        &fortran_line.lines[origin]
    } else {
        ""
    };
    let was_indented = if origin < write_ctx.lines_were_indented.len() {
        write_ctx.lines_were_indented[origin]
    } else {
        original_line.starts_with(' ') || original_line.starts_with('\t')
    };

    // Check if we need to detach inline comment to its own line due to line length
    // Also detach if the original line was split (comment goes after all split lines)
    let should_detach_comment = if has_comment
        && write_ctx.effective_line_length < LINE_SPLIT_THRESHOLD
    {
        let comment = &fortran_line.comments[origin];
        let comment_trimmed = comment.trim();
        let is_comment_only = line_to_write.trim().is_empty();

        if !is_comment_only && !comment_trimmed.is_empty() {
            // Always detach if the line was split
            if write_ctx.split_origins.contains(&origin) {
                true
            } else {
                // Calculate total line length with code + spacing + comment
                let spacing = if pass_ctx.config.normalize_comment_spacing {
                    pass_ctx.config.comment_spacing
                } else {
                    let trailing_spaces = original_line.len() - original_line.trim_end().len();
                    trailing_spaces.max(1) // At least 1 space before comment
                };
                let total_length = line_to_write.trim_end().len() + spacing + comment_trimmed.len();
                total_length > write_ctx.effective_line_length
            }
        } else {
            false
        }
    } else {
        false
    };

    // Check if this is a FORD documentation comment line (!! at start of comment)
    // FORD comments should preserve their original indentation
    let is_ford_comment_line = if has_comment {
        let comment = &fortran_line.comments[origin];
        let comment_trimmed = comment.trim();
        line_to_write.trim().is_empty() && comment_trimmed.starts_with("!!")
    } else {
        false
    };

    // Prepend label to first line
    if line_index == 0 && !labels.label.is_empty() {
        // Compute padding to place the statement content at the target column.
        // The target column comes from computed_indents[0] (the target indentation).
        // Padding = target_indent - labels.label.len(), ensuring the statement starts
        // at the same column it would if there were no label.
        let trimmed = line_to_write.trim_start();
        let target_indent = if pass_ctx.impose_indent && !write_ctx.computed_indents.is_empty() {
            write_ctx.computed_indents[0]
        } else {
            // Without indentation, use the current line's spacing
            line_to_write.len() - trimmed.len()
        };
        let padding = target_indent.saturating_sub(labels.label.len());
        output.write_all(labels.label.as_bytes())?;
        write_spaces(output, padding)?;
        output.write_all(trimmed.as_bytes())?;
    } else if is_ford_comment_line {
        // FORD comment lines: write original indentation, not the processed line_to_write
        let original_indent = indent_of(original_line);
        write_spaces(output, original_indent)?;
    } else {
        // For comment-only lines that will be indented in the comment handling section,
        // don't write the line_to_write here (it may contain just a space marker from
        // the two-pass logic). The indent will be written later.
        let is_comment_only_indented = has_comment
            && line_to_write.trim().is_empty()
            && pass_ctx.impose_indent
            && (was_indented || line_index > 0);
        if !is_comment_only_indented {
            output.write_all(line_to_write.as_bytes())?;
        }
    }

    // Add comment if present
    if has_comment {
        let comment = &fortran_line.comments[origin];
        let comment_trimmed = comment.trim();
        let is_comment_only = line_to_write.trim().is_empty();

        if should_detach_comment {
            // Detach comment to its own line
            // Write newline after code, then comment on new line with same indent
            output.write_all(b"\n")?;

            // Get indent for the detached comment line (same as the code line)
            let comment_indent = if origin < write_ctx.computed_indents.len() {
                write_ctx.computed_indents[origin]
            } else if let Some(ind) = indenter {
                ind.get_scope_indent()
            } else {
                indent_of(line_to_write)
            };
            write_spaces(output, comment_indent)?;
            output.write_all(comment_trimmed.as_bytes())?;
        } else {
            // For comment-only lines with impose_indent, apply the appropriate indent
            // if EITHER: the original line was indented OR we're in a continuation (i > 0)
            // BUT within continuations, even non-indented comments get continuation indent
            // OMP directives (!$OMP) should stay at column 0, not indented
            // FORD documentation comments (!!) are handled earlier (preserve original position)
            let is_omp_directive = OMP_DIR_RE.is_match(comment_trimmed);
            let is_ford_comment = comment_trimmed.starts_with("!!");
            let in_continuation = line_index > 0;
            if is_comment_only
                && pass_ctx.impose_indent
                && (was_indented || in_continuation)
                && !is_omp_directive
                && !is_ford_comment
            {
                // Use computed continuation indent if available, otherwise use scope indent
                let indent = if line_index < write_ctx.computed_indents.len() {
                    write_ctx.computed_indents[line_index]
                } else if let Some(ind) = indenter {
                    ind.get_scope_indent()
                } else {
                    0
                };
                write_spaces(output, indent)?;
            }
            // Note: FORD comments (!!) are handled earlier - their original indent is preserved

            // Determine spacing before comment
            let spacing = if pass_ctx.config.normalize_comment_spacing {
                // When normalizing, use consistent spacing
                // If comment is on its own line (line is empty), no extra spacing
                if is_comment_only {
                    0
                } else {
                    pass_ctx.config.comment_spacing
                }
            } else if is_comment_only && is_ford_comment {
                // FORD comment-only lines: no extra spacing (indentation handled earlier)
                0
            } else if is_comment_only && pass_ctx.impose_indent && (was_indented || in_continuation)
            {
                // Comment-only line with indentation - no extra spacing
                0
            } else if is_comment_only && !was_indented && !in_continuation {
                // Comment-only line at column 1 (not in continuation) - no spacing
                0
            } else {
                // Preserve original spacing between code and comment
                // fortran_line.lines[i] contains the code part from InputStream, which preserves
                // trailing spaces before the comment (e.g., "then " vs "then")
                let trailing_spaces = original_line.len() - original_line.trim_end().len();
                trailing_spaces
            };

            // Write spacing and trimmed comment
            write_spaces(output, spacing)?;
            output.write_all(comment_trimmed.as_bytes())?;
        }
    }

    output.write_all(b"\n")?;
    Ok(())
}

/// Compute which output line indices should have comments written
///
/// Returns `(comment_line_indices, split_origins)` where:
/// - `comment_line_indices`: Set of output line indices that should have their comment written
/// - `split_origins`: Set of origin indices that were split into multiple lines
fn compute_comment_indices(line_origins: &[usize]) -> (HashSet<usize>, HashSet<usize>) {
    let mut first_for_origin: std::collections::HashMap<usize, usize> =
        std::collections::HashMap::new();
    let mut last_for_origin: std::collections::HashMap<usize, usize> =
        std::collections::HashMap::new();

    for (i, &origin) in line_origins.iter().enumerate() {
        first_for_origin.entry(origin).or_insert(i);
        last_for_origin.insert(origin, i);
    }

    // Origins where first != last are split origins
    let split: HashSet<usize> = first_for_origin
        .iter()
        .filter(|(origin, &first)| last_for_origin.get(*origin) != Some(&first))
        .map(|(origin, _)| *origin)
        .collect();

    (last_for_origin.values().copied().collect(), split)
}

/// Apply line splitting if lines exceed the configured length
///
/// Preprocessor lines are left alone: a C preprocessor directive continues
/// with a trailing backslash and a fypp directive not at all, so breaking
/// either one with Fortran's `&` would change what it means.
///
/// Returns a vector mapping each output line to its original line index.
fn split_lines_if_needed(
    output_lines: &mut Vec<String>,
    effective_line_length: usize,
    indent_size: usize,
    is_preprocessor: bool,
) -> Vec<usize> {
    // Track which original line each output line came from (for comment placement)
    // By default, output line i corresponds to original line i
    let mut line_origins: Vec<usize> = (0..output_lines.len()).collect();

    if effective_line_length < LINE_SPLIT_THRESHOLD && !is_preprocessor {
        // Get indents for splitting (use 0 if not computed)
        let line_indents: Vec<usize> = output_lines.iter().map(|line| indent_of(line)).collect();

        let (split_lines, split_indents, split_origins) = split_long_lines(
            output_lines,
            &line_indents,
            effective_line_length,
            indent_size,
        );

        // Only use split result if we actually split (more lines)
        if split_lines.len() > output_lines.len() {
            // Apply indentation to split lines
            // The split_long_lines function returns lines without indentation,
            // and split_indents contains the indentation level for each line
            *output_lines = split_lines
                .iter()
                .enumerate()
                .map(|(i, line)| {
                    let indent = split_indents.get(i).copied().unwrap_or(0);
                    let trimmed = line.trim_start();
                    if trimmed.is_empty() {
                        line.clone()
                    } else {
                        format!("{}{}", " ".repeat(indent), trimmed)
                    }
                })
                .collect();
            line_origins = split_origins;
        }
    }

    line_origins
}

/// Restore pre-ampersands and apply indentation to continuation lines
///
/// After removing leading `&` for formatting, this restores them and applies
/// the computed indentation to continuation lines.
#[allow(clippy::ptr_arg)]
fn apply_pre_ampersand_indentation(
    output_lines: &mut Vec<String>,
    computed_indents: &mut Vec<usize>,
    pre_ampersand: &[String],
    fortran_line: &FortranLine,
    label_shift: usize,
    is_fypp_line: bool,
) {
    // prepend_ampersands adds & and adjusts indent by -1
    *output_lines = prepend_ampersands(output_lines, computed_indents, pre_ampersand);

    // Now apply indentation for continuation lines (we skipped them in the loop)
    // Note: prepend_ampersands already added the & prefix (e.g., "&     content")
    // We should NOT trim_start here as that would remove the & prefix
    // Just prepend the computed indent spaces
    // Skip lines inside multiline strings - they are preserved as-is
    //
    // For labeled lines, apply label_shift to continuation indents.
    // When the first line's label spacing was normalized (reduced), continuation
    // lines need to shift left by the same amount to maintain relative positions.
    for (i, line) in output_lines.iter_mut().enumerate() {
        if i > 0 && i < computed_indents.len() {
            // Skip lines inside multiline strings
            if fortran_line.starts_in_string(i) {
                continue;
            }
            // Skip fypp continuation lines - they preserve original indentation
            // If first line is fypp, all continuation lines are special
            // Also skip if this specific line is a fypp directive
            let is_line_fypp = FYPP_LINE_RE.is_match(line.trim_start());
            if is_fypp_line || is_line_fypp {
                continue;
            }
            // Apply label_shift: subtract from indent for labeled lines
            let indent = computed_indents[i].saturating_sub(label_shift);
            // Line may have & prefix from prepend_ampersands
            // Trim the line first to remove any original indentation, then add computed indent
            if !line.trim().is_empty() {
                let trimmed = line.trim_start();
                // Handle OMP conditional prefix for continuation lines
                if fortran_line.omp_prefix.is_empty() {
                    *line = format!("{}{}", " ".repeat(indent), trimmed);
                } else {
                    let omp_len = fortran_line.omp_prefix.len();
                    let padding = indent.saturating_sub(omp_len);
                    *line = format!(
                        "{}{}{}",
                        fortran_line.omp_prefix,
                        " ".repeat(padding),
                        trimmed
                    );
                }
            }
        }
    }
}

/// Single formatting pass
///
/// Either whitespace formatting or indentation, controlled by flags
fn format_pass<R: BufRead, W: Write>(
    input: R,
    output: &mut W,
    config: &Config,
    impose_indent: bool,
    impose_whitespace: bool,
    inspect_result: Option<&InspectResult>,
) -> Result<()> {
    // Build scope parser
    let scope_parser = build_scope_parser(config.indent_fypp && impose_indent, config.indent_mod);

    // Get first_indent from inspection result, or default to 0
    let first_indent = inspect_result.map_or(0, |r| r.first_indent);

    // Create indenter if needed
    let mut indenter = if impose_indent {
        Some(F90Indenter::new(scope_parser, first_indent))
    } else {
        None
    };

    // Fortran line counter (for indexing into required_indents)
    let mut fortran_line_number: usize = 0;

    // Get whitespace_flags array for whitespace formatting
    let whitespace_flags = config.get_whitespace_flags();

    // Create pass context to group pass-level settings
    let pass_ctx = PassContext {
        config,
        whitespace_flags,
        impose_indent,
        impose_whitespace,
        inspect_result,
    };

    // Get case settings
    let case_settings = CaseSettings::from_dict(&config.case_dict);

    // Create input stream
    let mut stream = InputStream::new(input);

    // Track blank line state for suppressing consecutive blank lines
    let mut skip_blank = false;

    // Track block deactivation state for !&< ... !&> directives
    let mut in_deactivation_block = false;

    // Process each logical Fortran line
    while let Some(fortran_line) = stream.next_fortran_line()? {
        let is_blank = is_blank_line(&fortran_line);

        // Skip this line if it's blank and we just output a blank line
        if is_blank && skip_blank {
            continue;
        }

        // Count lines with content only, exactly as `inspect_file` records
        // them. Note this leaves fortran_line_number 1-indexed against a
        // 0-indexed vector, which shifts all values by 1: the alignment
        // preservation logic depends on using the NEXT line's required_indent
        // for scope-opening statements (IF/DO).
        if !is_blank {
            fortran_line_number += 1;
        }
        let mut output_lines = fortran_line.lines.clone();

        // Track whether each original line was indented (started with whitespace)
        // This must be captured early BEFORE any trimming or formatting
        // Used later to decide whether comment-only lines should be indented
        let lines_were_indented: Vec<bool> = fortran_line
            .lines
            .iter()
            .map(|line| line.starts_with(' ') || line.starts_with('\t'))
            .collect();

        // Strip OMP prefix from output lines if present
        if !fortran_line.omp_prefix.is_empty() {
            for line in &mut output_lines {
                if line.starts_with(&fortran_line.omp_prefix) {
                    // Replace OMP prefix with spaces to preserve alignment
                    *line = format!(
                        "{}{}",
                        " ".repeat(fortran_line.omp_prefix.len()),
                        &line[fortran_line.omp_prefix.len()..]
                    );
                }
            }
        }

        let labels = split_off_label(
            &fortran_line,
            &output_lines,
            fortran_line_number > 1 && pass_ctx.impose_indent,
        );

        // Detect formatting deactivation markers
        let skip_format = detect_skip_format(&fortran_line.comments, &mut in_deactivation_block);

        // Check if this is a fypp line directive (starts with #!, #:, $:, or @:)
        // These lines should not have whitespace formatting applied
        let is_fypp_line = FYPP_LINE_RE.is_match(&fortran_line.joined_line);

        // Check if this logical line contains any C preprocessor lines
        // (starts with # but not fypp). Used to skip whitespace formatting and case conversion.
        // Note: Individual physical lines will be checked separately for column 0 pinning.
        let is_cpp_line = CPP_LINE_RE.is_match(&fortran_line.joined_line);

        // Check if lines have leading & (which disables auto-alignment)
        let auto_align = should_auto_align(&output_lines);

        // Create formatting flags struct
        let flags = FormattingFlags {
            is_fypp_line,
            is_cpp_line,
            skip_format,
            auto_align,
        };

        // Extract pre-ampersands and apply whitespace formatting to continuation lines
        let (pre_ampersand, ampersand_sep, manual_lines_indent) =
            extract_and_format_pre_ampersands(&mut output_lines, &fortran_line, &pass_ctx, flags);

        // Apply whitespace formatting
        apply_whitespace_to_lines(
            &mut output_lines,
            &fortran_line,
            &pass_ctx,
            &labels,
            flags,
            &ampersand_sep,
        );

        // Apply relational operator replacement if enabled
        // This converts between Fortran-style (.lt., .eq., etc.) and C-style (<, ==, etc.)
        if pass_ctx.config.enable_replacements
            && !flags.skip_format
            && !flags.is_fypp_line
            && !flags.is_cpp_line
        {
            for (i, line) in output_lines.iter_mut().enumerate() {
                if !fortran_line.starts_in_string(i) {
                    *line = replace_relational_operators(line, pass_ctx.config.c_relations);
                }
            }
        }

        // Apply case conversion if enabled and not deactivated
        // Skip CPP and fypp lines: they are not Fortran code, and fypp only
        // recognizes its directives (#:if, #:endif, ...) in lower case
        if case_settings.is_enabled()
            && !flags.skip_format
            && !flags.is_cpp_line
            && !flags.is_fypp_line
        {
            for (i, line) in output_lines.iter_mut().enumerate() {
                if !fortran_line.starts_in_string(i) {
                    *line = convert_case(line, &case_settings);
                }
            }
        }

        // Store computed indents for use in comment handling
        let mut computed_indents: Vec<usize> = Vec::new();

        // Apply indentation if requested and not deactivated
        if pass_ctx.impose_indent && !flags.skip_format {
            if flags.is_cpp_line {
                // C preprocessor lines are pinned to column 0
                // Only strip indentation from lines that actually ARE preprocessor directives
                // This preserves indentation for Fortran code in the same logical line
                for line in &mut output_lines {
                    let line_trimmed = line.trim_start();
                    // Only pin actual CPP directive lines to column 0
                    if CPP_LINE_RE.is_match(line_trimmed) {
                        *line = line_trimmed.to_string();
                    }
                    // Other lines (Fortran code) keep their indentation
                }
                // Don't update indenter scope - CPP lines don't affect Fortran scope
            } else if let Some(ref mut ind) = indenter {
                compute_and_apply_indentation(
                    &mut output_lines,
                    &mut computed_indents,
                    ind,
                    &fortran_line,
                    &pass_ctx,
                    &labels,
                    fortran_line_number,
                    &pre_ampersand,
                    manual_lines_indent.as_deref(),
                    flags.is_fypp_line,
                );
            }
        } else if !fortran_line.omp_prefix.is_empty() && !output_lines.is_empty() {
            // When indentation is disabled, we still need to add OMP prefix back
            // to ALL lines (not just line 0) so that Pass 2 sees consistent prefixes.
            // Without this, continuation lines lose their !$ prefix and Pass 2's
            // alignment computation drifts by the prefix length on each run.
            let prefix = &fortran_line.omp_prefix;
            for line in &mut output_lines {
                let trimmed = line.trim_start();
                *line = format!("{prefix}{trimmed}");
            }
        }

        // Prepend ampersands back to continuation lines if we extracted them earlier
        if !pre_ampersand.is_empty() && pass_ctx.impose_indent {
            apply_pre_ampersand_indentation(
                &mut output_lines,
                &mut computed_indents,
                &pre_ampersand,
                &fortran_line,
                labels.label_shift,
                flags.is_fypp_line,
            );
        }

        // Apply line splitting if line_length is configured
        let effective_line_length = if pass_ctx.config.line_length == 0 {
            LINE_SPLIT_THRESHOLD
        } else {
            pass_ctx.config.line_length
        };
        let line_origins = split_lines_if_needed(
            &mut output_lines,
            effective_line_length,
            pass_ctx.config.indent,
            flags.is_cpp_line || flags.is_fypp_line,
        );

        // Compute comment placement indices
        let (comment_line_indices, split_origins) = compute_comment_indices(&line_origins);

        // Create write context
        let write_ctx = LineWriteContext {
            computed_indents: &computed_indents,
            lines_were_indented: &lines_were_indented,
            comment_line_indices: &comment_line_indices,
            split_origins: &split_origins,
            effective_line_length,
        };

        // Write output lines
        for (i, line) in output_lines.iter().enumerate() {
            let origin = line_origins.get(i).copied().unwrap_or(i);
            write_output_line(
                output,
                line,
                i,
                origin,
                &fortran_line,
                &pass_ctx,
                &labels,
                &write_ctx,
                indenter.as_ref(),
            )?;
        }

        // Set skip_blank for next iteration
        // Skip subsequent blank lines if this line was blank and had no special content
        skip_blank = is_blank && labels.label.is_empty();
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    #[test]
    fn test_opens_indented_scope() {
        // The constructs the old hand-written copy of these patterns missed:
        // an IF written flush with one of these used to be read as stacked
        // against it, leaving the body unindented
        assert!(opens_indented_scope("critical"));
        assert!(opens_indented_scope("change team (t)"));
        assert!(opens_indented_scope("enumeration type :: e"));
        assert!(opens_indented_scope("outer: associate (y => x)"));
        assert!(opens_indented_scope("type :: matrix(k, n)"));
        assert!(opens_indented_scope("interface read(formatted)"));

        // An END closes a scope, and several opener patterns match one
        assert!(!opens_indented_scope("end critical"));
        assert!(!opens_indented_scope("end associate outer"));
        assert!(!opens_indented_scope("end type matrix"));

        // IF and DO are what stacking is about, and a program unit's body is
        // left alone on purpose
        assert!(!opens_indented_scope("if (x) then"));
        assert!(!opens_indented_scope("do i = 1, 10"));
        assert!(!opens_indented_scope("program p"));
        assert!(!opens_indented_scope("subroutine s(a)"));
    }

    #[test]
    fn test_over_limit_warnings() {
        // Conforming input says nothing
        assert!(over_limit_warnings("program p\nx = 1\nend program p\n").is_empty());

        // A line past 10 000 characters (6.3.2.1)
        let long = format!("x = {}\n", "a".repeat(MAX_LINE_LENGTH));
        let warnings = over_limit_warnings(&long);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("line 1"), "{}", warnings[0]);

        // A statement past 1 000 000 characters, spread over continuations
        // (6.3.2.6), reported once and by its first line
        let chunk = format!("x = x + {} &\n", "a".repeat(1000));
        let statement = format!("program p\n{}x = x\n", chunk.repeat(1001));
        let warnings = over_limit_warnings(&statement);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("line 2"), "{}", warnings[0]);
    }

    #[test]
    fn test_format_file_whitespace_only() {
        let input = "x=1+2\n";
        let config = Config {
            impose_whitespace: true,
            impose_indent: false,
            whitespace: 2,
            ..Default::default()
        };

        let cursor = Cursor::new(input.as_bytes());
        let reader = BufReader::new(cursor);
        let mut output = Vec::new();

        format_file(reader, &mut output, &config).unwrap();

        let result = String::from_utf8(output).unwrap();
        // Should have spaces around operators
        assert!(result.contains(" = "));
        assert!(result.contains(" + "));
    }

    #[test]
    fn test_format_file_indent_only() {
        let input = "if (x > 0) then\nx = 1\nend if\n";
        let config = Config {
            impose_whitespace: false,
            impose_indent: true,
            indent: 3,
            ..Default::default()
        };

        let cursor = Cursor::new(input.as_bytes());
        let reader = BufReader::new(cursor);
        let mut output = Vec::new();

        format_file(reader, &mut output, &config).unwrap();

        let result = String::from_utf8(output).unwrap();
        let lines: Vec<&str> = result.lines().collect();

        // IF should have no indent (or starts with "if")
        assert!(lines[0].trim_start() == lines[0] || lines[0].starts_with("if"));

        // x = 1 should be indented
        assert!(lines[1].starts_with("   "));
    }

    #[test]
    fn test_format_file_both_passes() {
        let input = "if (x>0) then\nx=1\nend if\n";
        let config = Config {
            impose_whitespace: true,
            impose_indent: true,
            indent: 3,
            whitespace: 2,
            ..Default::default()
        };

        let cursor = Cursor::new(input.as_bytes());
        let reader = BufReader::new(cursor);
        let mut output = Vec::new();

        format_file(reader, &mut output, &config).unwrap();

        let result = String::from_utf8(output).unwrap();
        // Should have both whitespace and indentation
        assert!(result.contains(" = "));
        assert!(result.lines().nth(1).unwrap().starts_with("   "));
    }
}
