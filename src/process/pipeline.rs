//! Two-pass formatting pipeline
//!
//! Implements the main formatting pipeline:
//! - Pass 1 (optional): Whitespace formatting
//! - Pass 2 (optional): Indentation

use std::borrow::Cow;
use std::collections::HashSet;
use std::io::{BufRead, BufReader, Cursor, Write};
use std::sync::LazyLock;

use regex::Regex;

use crate::config::Config;
use crate::format::{
    convert_case, format_line, format_line_with_level, get_manual_alignment, prepend_ampersands,
    remove_pre_ampersands, replace_relational_operators, should_auto_align, split_long_lines,
    CaseSettings, F90Indenter, IndentParams,
};
use crate::parser::patterns::{
    CPP_LINE_RE, DO_RE, IF_RE, MOD_RE, OMP_DIR_RE, PROG_RE, STATEMENT_LABEL_RE,
};
use crate::parser::{CharFilter, FortranLine, InputStream};
use crate::scope::build_scope_parser;
use crate::Result;

/// Maximum line length at which automatic line splitting is attempted.
///
/// Lines longer than this are assumed to be intentionally long (e.g., data
/// arrays, generated code) and are left unsplit to avoid breaking them.
/// This also prevents excessive processing time on pathologically long lines.
const LINE_SPLIT_THRESHOLD: usize = 1024;

/// Pre-allocated buffer of spaces for indentation.
/// Using a static buffer avoids allocating a new Vec for each indent write.
const SPACES: &[u8; 256] = &[b' '; 256];

/// Write `count` spaces to output efficiently using pre-allocated buffer.
fn write_spaces<W: Write>(output: &mut W, count: usize) -> std::io::Result<()> {
    if count == 0 {
        return Ok(());
    }
    if count <= SPACES.len() {
        output.write_all(&SPACES[..count])
    } else {
        // Fall back to allocation for unusually large indents
        output.write_all(&vec![b' '; count])
    }
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
struct LineLabels<'a> {
    /// The joined logical line with label removed
    joined_no_label: &'a str,
    /// The first physical line with label removed
    first_no_label: &'a str,
    /// The label extracted from first physical line
    first_label: &'a str,
    /// The label extracted from joined line
    label: &'a str,
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

    while let Some(fortran_line) = stream.next_fortran_line()? {
        // Skip empty lines
        if fortran_line.lines.is_empty() {
            continue;
        }

        // Check if this line is blank (empty or whitespace only)
        // Must match the logic in pass_flines to keep line counts in sync
        let is_blank = fortran_line.joined_line.trim().is_empty()
            && fortran_line
                .comments
                .iter()
                .all(std::string::String::is_empty)
            && fortran_line.omp_prefix.is_empty();

        // Skip consecutive blank lines (same as formatting pass)
        if is_blank && skip_blank {
            continue;
        }
        skip_blank = is_blank;

        // Calculate offset (leading spaces) of first line
        let first_line = &fortran_line.lines[0];
        let offset = first_line.len() - first_line.trim_start().len();

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
        if IF_RE.is_match(joined_trimmed) || DO_RE.is_match(joined_trimmed) {
            let indent_misaligned = indent_size > 0 && offset % indent_size != 0;
            if prev_offset != offset || strict_indent || indent_misaligned {
                required_indent = indent_size;
            }
            // Otherwise keep required_indent as delta (usually 0 if same offset)
        } else {
            // For non-IF/DO statements, always use indent_size
            required_indent = indent_size;
        }

        required_indents.push(required_indent);
        prev_offset = offset;
    }

    Ok(InspectResult {
        required_indents,
        first_indent: first_indent.unwrap_or(0),
    })
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
pub fn format_file<R: BufRead, W: Write>(
    input: R,
    output: &mut W,
    config: &Config,
    _filename: &str,
) -> Result<()> {
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

    // Pre-inspect file to compute required_indents (for IF/DO indentation preservation)
    // This is only needed when impose_indent is true and strict_indent is false
    let inspect_result = if config.impose_indent && !config.strict_indent {
        let cursor = Cursor::new(&input_buffer);
        let inspect_reader = BufReader::new(cursor);
        Some(inspect_file(
            inspect_reader,
            config.indent,
            config.strict_indent,
        )?)
    } else {
        None
    };

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

    // Pass 2: Indentation
    if config.impose_indent {
        let cursor = Cursor::new(intermediate);
        let reader = BufReader::new(cursor);
        format_pass(
            reader,
            output,
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
            reader, output, config, false, // No indent
            false, // No whitespace (case conversion and line splitting will still run)
            None,
        )?;
    } else {
        // No indentation pass, write intermediate output directly
        output.write_all(&intermediate)?;
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
            let is_in_string =
                i < fortran_line.lines_in_string.len() && fortran_line.lines_in_string[i];
            is_fypp_continuation || is_fypp || is_in_string
        })
        .collect();

    // Remove pre-ampersands from lines
    let Ok(result) = remove_pre_ampersands(output_lines, &is_special) else {
        return (vec![], vec![], None);
    };

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
            // Check if this line is inside a multiline string
            let is_in_string =
                i < fortran_line.lines_in_string.len() && fortran_line.lines_in_string[i];
            if is_in_string {
                continue;
            }
            // Format the line content (& has been stripped)
            // Only treat trailing & as continuation if it's NOT inside a string
            let has_continuation = {
                let trimmed = line.trim_end();
                if trimmed.ends_with('&') {
                    // Check if the trailing & is outside a string using CharFilter
                    let mut last_amp_outside_string = None;
                    for (pos, c) in CharFilter::new(trimmed, true, true, true) {
                        if c == '&' {
                            last_amp_outside_string = Some(pos);
                        }
                    }
                    last_amp_outside_string.is_some_and(|p| p == trimmed.len() - 1)
                } else {
                    false
                }
            };
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
    labels: &LineLabels<'_>,
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
                output_lines[0].clone_from(&labels.first_no_label.to_string());
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

                // Check if this line starts inside a multiline string
                // If so, skip whitespace formatting to preserve string content
                let is_in_string =
                    i < fortran_line.lines_in_string.len() && fortran_line.lines_in_string[i];
                if is_in_string {
                    // Preserve lines inside multiline strings as-is
                    continue;
                }

                // Remove continuation marker for formatting, then restore
                // Note: Only treat trailing & as continuation if it's OUTSIDE strings
                // (A & inside a string is part of string content, not a continuation marker)
                let has_continuation = {
                    let trimmed = line.trim_end();
                    if trimmed.ends_with('&') {
                        // Check if the & is inside a string using CharFilter
                        let mut last_outside_string = None;
                        for (pos, c) in CharFilter::new(trimmed, true, true, true) {
                            if c == '&' {
                                last_outside_string = Some(pos);
                            }
                        }
                        // If the & at the end is visible to CharFilter, it's a continuation
                        last_outside_string.is_some_and(|p| p == trimmed.len() - 1)
                    } else {
                        false
                    }
                };
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
                labels.joined_no_label,
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
        output_lines[0] = labels.first_no_label.to_string();
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
    labels: &LineLabels<'_>,
    fortran_line_number: usize,
    pre_ampersand: &[String],
    manual_lines_indent: Option<&[usize]>,
    is_fypp_line: bool,
) -> Result<()> {
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
        use_same_line: fortran_line.use_same_line,
        semicolon_line_index: fortran_line.semicolon_line_index,
    };

    // Process the logical line for indentation (without label)
    // Use output_lines (which may have been formatted) for alignment computation
    indenter.process_logical_line(labels.joined_no_label, output_lines, &indent_params)?;

    // Get computed indents and save for comment handling
    let indents = indenter.get_lines_indent();
    *computed_indents = indents.to_vec();

    // Shift indents if there's a label (to account for label position)
    // The indenter trims lines before computing, so we need to add back
    // the leading whitespace from the first line (which encodes label position)
    //
    // IMPORTANT: Only shift the FIRST line's indent when there are continuation
    // lines with leading & (pre_ampersand). Continuation lines with leading &
    // should use normal scope-based indent, not label-shifted indent.
    if !labels.label.is_empty() && !computed_indents.is_empty() && !output_lines.is_empty() {
        // Get leading whitespace from first line (this includes label spacing)
        let first_line_leading = output_lines[0].len() - output_lines[0].trim_start().len();
        if first_line_leading > 0 {
            let has_pre_amp = !pre_ampersand.is_empty();

            // For labeled lines with manual alignment (leading &),
            // continuation lines should NOT have base_indent.
            // The indenter adds base_indent to all continuation indents, but for
            // manual alignment with labels, we want just the manual_indent values.
            // These will later have label_shift applied in the prepend_ampersands loop.
            // Note: We do NOT modify computed_indents[0] here - the first line uses
            // the target indent from the indenter, not the processed line's spacing.
            if has_pre_amp {
                if !computed_indents.is_empty() {
                    // Compute base_indent from first line
                    let base_indent = computed_indents[0];
                    // Continuation lines: subtract base_indent to get just manual_indent
                    for ind in computed_indents.iter_mut().skip(1) {
                        *ind = ind.saturating_sub(base_indent);
                    }
                }
            } else {
                // For labeled lines with auto-alignment (no leading &),
                // continuation indents should align relative to where content starts
                // in the OUTPUT (not the intermediate representation).
                //
                // In the output, the first line is: label + padding + content
                // where padding = max(0, target_indent - label.len())
                //
                // So content starts at position: max(label.len(), target_indent)
                // This is the effective_leading we should use for continuation alignment.

                // Compute base_indent from first line (this is target_indent for the content)
                let base_indent = computed_indents[0];

                // In output format: label (len) + padding + content
                // padding = max(0, base_indent - labels.label.len())
                // So content starts at: max(labels.label.len(), base_indent)
                let effective_leading = labels.label.len().max(base_indent);

                // Adjustment = effective_leading - base_indent
                // This replaces base_indent with effective_leading
                #[allow(clippy::cast_possible_wrap)]
                let adjustment = effective_leading as isize - base_indent as isize;

                // Only adjust continuation lines (skip the first line).
                // The first line should use the target indent from the indenter,
                // not be replaced with first_line_leading (which is normalized to label.len()).
                for ind in computed_indents.iter_mut().skip(1) {
                    *ind = adjust_indent(*ind, adjustment);
                }
            }
        }
    }

    // Check if this is a multi-line fypp directive (first line is fypp + continuation)
    // If so, preserve original indent for ALL lines
    let is_multiline_fypp_directive = is_fypp_line && output_lines.len() > 1;

    // Apply indents to output lines
    // When we have pre_ampersand (lines with leading &), skip applying indent here
    // because prepend_ampersands needs the lines without indent applied
    let has_pre_ampersand = !pre_ampersand.is_empty();

    for (i, line) in output_lines.iter_mut().enumerate() {
        if i < computed_indents.len() {
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
                    let original_indent = line.len() - line.trim_start().len();
                    let base_indent = computed_indents.first().copied().unwrap_or(0);
                    let new_indent = base_indent + original_indent;
                    *line = format!("{}{}", " ".repeat(new_indent), line.trim_start());
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

    Ok(())
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
    labels: &LineLabels<'_>,
    write_ctx: &LineWriteContext<'_>,
    indenter: Option<&F90Indenter>,
) -> std::io::Result<()> {
    let has_comment = origin < fortran_line.comments.len()
        && !fortran_line.comments[origin].is_empty()
        && write_ctx.comment_line_indices.contains(&line_index);

    // Determine what to write for the line portion
    // If there's a comment and we're stripping, trim trailing spaces from the line
    // For comment-only lines that were originally indented, preserve one space
    // so that Pass 2 of two-pass formatting can detect the original indentation
    let line_to_write = if has_comment && pass_ctx.config.normalize_comment_spacing {
        let trimmed = line.trim_end();
        // If the line is now empty (comment-only) but was originally indented,
        // preserve one space as a marker for Pass 2
        if trimmed.is_empty()
            && origin < write_ctx.lines_were_indented.len()
            && write_ctx.lines_were_indented[origin]
        {
            " "
        } else {
            trimmed
        }
    } else if has_comment {
        // Even without normalize_comment_spacing, we should trim trailing spaces
        // since the comment provides the separation
        // But preserve one space for originally-indented comment-only lines
        let trimmed = line.trim_end();
        if trimmed.is_empty()
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
        let original_indent = original_line.len() - original_line.trim_start().len();
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
                line_to_write.len() - line_to_write.trim_start().len()
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
/// Returns a vector mapping each output line to its original line index.
fn split_lines_if_needed(
    output_lines: &mut Vec<String>,
    effective_line_length: usize,
    indent_size: usize,
) -> Vec<usize> {
    // Track which original line each output line came from (for comment placement)
    // By default, output line i corresponds to original line i
    let mut line_origins: Vec<usize> = (0..output_lines.len()).collect();

    if effective_line_length < LINE_SPLIT_THRESHOLD {
        // Get indents for splitting (use 0 if not computed)
        let line_indents: Vec<usize> = output_lines
            .iter()
            .map(|line| line.len() - line.trim_start().len())
            .collect();

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
            let is_in_string =
                i < fortran_line.lines_in_string.len() && fortran_line.lines_in_string[i];
            if is_in_string {
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
        Some(F90Indenter::new(scope_parser.clone(), first_indent))
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
        // Increment Fortran line counter at start of loop
        // Note: This intentionally makes fortran_line_number 1-indexed when
        // used for required_indents, which shifts all values by 1. The
        // alignment preservation logic depends on using the NEXT line's
        // required_indent for scope-opening statements (IF/DO).
        fortran_line_number += 1;

        // Check if this line is blank (empty or whitespace only)
        let is_blank = fortran_line.joined_line.trim().is_empty()
            && fortran_line
                .comments
                .iter()
                .all(std::string::String::is_empty)
            && fortran_line.omp_prefix.is_empty();

        // Skip this line if it's blank and we just output a blank line
        if is_blank && skip_blank {
            continue;
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

        // Extract statement label from joined_line
        let (label, joined_line_no_label) = extract_label(&fortran_line.joined_line);

        // Also extract label from first physical line for output
        let (first_line_label, first_line_no_label) = if output_lines.is_empty() {
            (String::new(), String::new())
        } else {
            extract_label(&output_lines[0])
        };

        // For label_shift computation, use the ORIGINAL line from fortran_line.lines[0]
        // not output_lines[0], because output_lines may have been modified in Pass 1.
        // This ensures we compute the correct shift even in Pass 2.
        let (_, original_first_line_no_label) = if fortran_line.lines.is_empty() {
            (String::new(), String::new())
        } else {
            extract_label(&fortran_line.lines[0])
        };

        // Normalize label spacing for non-first lines of the file
        //
        // Also track the label_shift: how much the first line content position changed
        // due to normalization. This shift is applied to continuation line indents.
        //
        // IMPORTANT: Only do normalization in Pass 2 (impose_indent=true), not Pass 1.
        // In two-pass mode, Pass 1 output becomes Pass 2 input, so normalizing in Pass 1
        // would lose the original spacing information needed to compute label_shift in Pass 2.
        let (label, joined_line_no_label, first_line_label, first_line_no_label, label_shift) =
            if fortran_line_number > 1 && !label.is_empty() && pass_ctx.impose_indent {
                // The regex captures digits + one space, leaving extra spaces in rest
                // Normalize by stripping extra leading spaces from line_no_label
                // so that leading_spaces = label.len()
                let target_spaces = label.len();

                // Compute label_shift from ORIGINAL first line (not modified output)
                let original_leading = original_first_line_no_label.len()
                    - original_first_line_no_label.trim_start().len();
                let shift = original_leading.saturating_sub(target_spaces);
                let normalize_line = |line: String| -> String {
                    let leading = line.len() - line.trim_start().len();
                    if leading > target_spaces {
                        // Strip excess leading spaces
                        line[(leading - target_spaces)..].to_string()
                    } else {
                        line
                    }
                };

                (
                    label,
                    normalize_line(joined_line_no_label),
                    first_line_label,
                    normalize_line(first_line_no_label),
                    shift,
                )
            } else {
                (
                    label,
                    joined_line_no_label,
                    first_line_label,
                    first_line_no_label,
                    0,
                )
            };

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

        // Create line labels struct
        let labels = LineLabels {
            joined_no_label: &joined_line_no_label,
            first_no_label: &first_line_no_label,
            first_label: &first_line_label,
            label: &label,
            label_shift,
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
            for line in &mut output_lines {
                *line = replace_relational_operators(line, pass_ctx.config.c_relations);
            }
        }

        // Apply case conversion if enabled and not deactivated
        // Skip for CPP lines since they are not Fortran code
        if case_settings.is_enabled() && !flags.skip_format && !flags.is_cpp_line {
            for line in &mut output_lines {
                *line = convert_case(line, &case_settings);
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
                )?;
            }
        } else if !fortran_line.omp_prefix.is_empty() && !output_lines.is_empty() {
            // When indentation is disabled, we still need to add OMP prefix back
            // Since whitespace formatting doesn't add it anymore
            let trimmed = output_lines[0].trim_start();
            output_lines[0] = format!("{}{}", fortran_line.omp_prefix, trimmed);
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

        format_file(reader, &mut output, &config, "test.f90").unwrap();

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

        format_file(reader, &mut output, &config, "test.f90").unwrap();

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

        format_file(reader, &mut output, &config, "test.f90").unwrap();

        let result = String::from_utf8(output).unwrap();
        // Should have both whitespace and indentation
        assert!(result.contains(" = "));
        assert!(result.lines().nth(1).unwrap().starts_with("   "));
    }
}
