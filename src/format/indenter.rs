/// `F90Indenter` - Scope-based indentation tracker
///
/// Uses a stack to track open scopes (IF, DO, MODULE, etc.) and
/// calculates indentation levels based on scope depth.
use crate::format::aligner::F90Aligner;
use crate::parser::char_filter::CharFilter;
use crate::parser::patterns::DO_LABEL_RE;
use crate::scope::{ScopeParser, SCOPES};

/// Parameters for indentation processing
#[derive(Default)]
pub struct IndentParams<'a> {
    /// Relative indent size for new scopes
    pub relative_indent: usize,
    /// Indent for continuation lines
    pub continuation_indent: usize,
    /// Whether to indent fypp preprocessor blocks
    pub indent_fypp: bool,
    /// Manual indent override from aligner
    pub manual_lines_indent: Option<&'a [usize]>,
    /// If Some(idx), force minimal indent only for lines AFTER idx
    pub semicolon_line_index: Option<usize>,
    /// Statement label of this line, stripped before matching ("" if none)
    pub label: &'a str,
}

impl IndentParams<'_> {
    /// Create `IndentParams` with common defaults
    #[must_use]
    pub fn new(indent: usize) -> IndentParams<'static> {
        IndentParams {
            relative_indent: indent,
            continuation_indent: indent,
            ..Default::default()
        }
    }
}

/// Result of scanning a logical line for END statements
struct EndDetection {
    /// Any part of the line (including after semicolons) is an END statement
    is_any: bool,
    /// The leading END validly closed the innermost scope (already popped)
    valid: bool,
    /// Number of ENDs after semicolons that close outer scopes
    additional_count: usize,
    /// An END after a semicolon matched the current scope
    valid_after_semicolon: bool,
}

/// `F90Indenter` tracks scope nesting and calculates indentation
pub struct F90Indenter {
    /// Stack of open scopes, as indices into [`crate::scope::SCOPES`]
    scope_storage: Vec<usize>,
    /// Stack of absolute indent levels
    indent_storage: Vec<usize>,
    /// Output: indent for each physical line in current `logical_line`
    line_indents: Vec<usize>,
    /// Scope parser (regex matchers)
    parser: ScopeParser,
    /// Aligner for continuation lines
    aligner: F90Aligner,
    /// Whether this is the first line (used for initial indent)
    initial: bool,
    /// Open labeled DO constructs, as (`scope_storage` depth after the push,
    /// terminating statement label)
    do_labels: Vec<(usize, String)>,
}

impl F90Indenter {
    /// Create a new `F90Indenter`
    ///
    /// # Arguments
    /// * `parser` - Scope parser with regex matchers
    /// * `first_indent` - Initial indent level
    #[must_use]
    pub fn new(parser: ScopeParser, first_indent: usize) -> Self {
        Self {
            scope_storage: Vec::new(),
            indent_storage: vec![first_indent],
            line_indents: Vec::new(),
            parser,
            aligner: F90Aligner::new(),
            initial: true,
            do_labels: Vec::new(),
        }
    }

    /// Process lines of a Fortran logical line
    ///
    /// This is the core indentation calculation function.
    ///
    /// # Arguments
    /// * `logical_line` - The logical Fortran line (continuations joined)
    /// * `lines` - The original physical lines
    /// * `params` - Indentation parameters (indent sizes, flags, overrides)
    pub fn process_logical_line(
        &mut self,
        logical_line: &str,
        lines: &[String],
        params: &IndentParams<'_>,
    ) {
        self.line_indents.clear();

        // Filter the line to get only code (no strings/comments)
        let filtered_line: String = CharFilter::code(logical_line).map(|(_, c)| c).collect();

        // Split by semicolon for multi-statement handling
        let parts: Vec<&str> = filtered_line.split(';').collect();

        // Check for scope closing (end) FIRST
        // This must be checked before NEW because patterns like SUBR_RE can match "end subroutine"
        let end = self.detect_end_statements(&parts, &filtered_line, params.indent_fypp);

        // Check for scope continuation (e.g., ELSE, CASE, CONTAINS)
        let (is_continue, fypp_continue_pop_to) = self.detect_continuation(&filtered_line);

        // Check for scope opening (new) LAST - only if this is NOT an end
        // statement (to avoid SUBR_RE matching "end subroutine";
        // scope_storage was already popped if it was an END) and not a
        // continuation: no statement both continues a construct and opens
        // one, and the shapes overlap (TYPE IS (t) reads as a derived type
        // definition with a type-param-list).
        let (new_scope, additional_scopes) = if end.is_any || is_continue {
            (None, Vec::new())
        } else {
            self.detect_new_scopes(&parts, &filtered_line)
        };

        // Calculate indent for this line
        // Note: scope_storage was already popped if the line is an END
        let line_indent = self.calculate_indent(
            is_continue,
            end.is_any,
            end.valid || end.valid_after_semicolon,
        );

        // For continuation lines, calculate alignment BEFORE updating scope stack
        if lines.len() > 1 {
            // Get continuation indents from either:
            // 1. Manual alignment (when lines have leading &) - preserves original positioning
            // 2. Automatic alignment via F90Aligner
            let continuation_indents: Vec<usize> =
                if let Some(manual_indent) = params.manual_lines_indent {
                    // Use manual indents directly (for lines with leading &)
                    // These are already relative offsets
                    manual_indent.to_vec()
                } else {
                    // Strip leading whitespace from lines before passing to aligner
                    let trimmed_lines: Vec<String> = lines
                        .iter()
                        .map(|line| line.trim_start().to_string())
                        .collect();

                    // Process with aligner to get continuation indents
                    self.aligner.process_logical_line(
                        logical_line,
                        &trimmed_lines,
                        params.continuation_indent,
                    );

                    self.aligner.get_lines_indent().to_vec()
                };

            // First line gets the calculated indent
            self.line_indents.push(line_indent);

            // Determine base indent to add to alignment results
            // - valid END/CONTINUE: indents[-2] (grandparent)
            // - invalid END: indents[-1] (current)
            // - NEW scopes with manual alignment: line_indent (first line's actual indent)
            // - NEW scopes without manual alignment: indents[-1] (current)
            // - Regular: indents[-1] (current)
            let base_indent = if (end.is_any && end.valid) || is_continue {
                // For valid END and CONTINUE, use grandparent indent
                if self.indent_storage.len() >= 2 {
                    self.indent_storage[self.indent_storage.len() - 2]
                } else {
                    *self.indent_storage.last().unwrap_or(&0)
                }
            } else if new_scope.is_some() && params.manual_lines_indent.is_some() {
                // For NEW scopes with manual alignment: use the first line's indent
                // This prevents double-counting indent when manual alignment already
                // captures the relative positioning from the original formatting
                line_indent
            } else {
                // For invalid END, NEW scopes (auto-aligned), and regular lines: use current indent
                *self.indent_storage.last().unwrap_or(&0)
            };

            // Continuation lines: alignment returns relative offsets, add base
            // When semicolon_line_index=Some(idx), force minimal indent only for lines AFTER idx
            if let Some(semicolon_idx) = params.semicolon_line_index {
                // Force indent=1 only for lines after the semicolon line
                for (i, &align_offset) in continuation_indents.iter().skip(1).enumerate() {
                    let line_idx = i + 1; // actual line index (0-based)
                    if line_idx > semicolon_idx {
                        // This line comes after the semicolon, use minimal indent
                        self.line_indents.push(1);
                    } else {
                        // This line comes before/at the semicolon, use normal alignment
                        self.line_indents.push(align_offset + base_indent);
                    }
                }
            } else {
                for &align_offset in continuation_indents.iter().skip(1) {
                    self.line_indents.push(align_offset + base_indent);
                }
            }
        } else {
            // Single line - just use the calculated indent
            self.line_indents.push(line_indent);
        }

        // Update indent_storage AFTER calculating alignment
        // Note: scope_storage was already popped during END detection
        if end.is_any && (end.valid || end.valid_after_semicolon) {
            // Only pop for part 0 END if end.valid is true (not just end.valid_after_semicolon)
            // This prevents double-popping when the END is after a semicolon
            if end.valid {
                if self.indent_storage.len() > 1 {
                    self.indent_storage.pop();
                } else if let Some(last) = self.indent_storage.last_mut() {
                    *last = 0;
                }
            }

            // Pop additional END scopes found after semicolons
            for _ in 0..end.additional_count {
                // Pop from scope_storage
                if !self.scope_storage.is_empty() {
                    self.scope_storage.pop();
                }
                // Pop from indent_storage (valid by default for additional ENDs)
                if self.indent_storage.len() > 1 {
                    self.indent_storage.pop();
                } else if let Some(last) = self.indent_storage.last_mut() {
                    *last = 0;
                }
            }
        }

        if is_continue {
            // ELSE/CASE - normally don't change stack
            // Exception: fypp continuation directives (#:else, #:elif) need to pop
            // inner Fortran scopes from scope_storage only (not indent_storage).
            // This allows:
            // - The continuation directive to use grandparent indent
            // - Content after the continuation to use parent indent (same level as
            //   content inside the original fypp branch)
            // - The closing directive (#:endif) to use grandparent indent
            if let Some(target_len) = fypp_continue_pop_to {
                while self.scope_storage.len() > target_len {
                    self.scope_storage.pop();
                    // Note: we DON'T pop from indent_storage here.
                    // This keeps indent level for content inside the else branch.
                }
            }
        }

        if let Some(scope) = new_scope {
            // Push new scope with appropriate indentation
            // relative_indent: 0 for aligned blocks, indent_size otherwise
            self.push_scope(scope, line_indent, params.relative_indent);
        }

        // Push additional scopes found after semicolons (even when part 0
        // opened no scope, e.g. "x = 1; if (y) then").
        // Each additional scope adds another level of indentation
        for scope in additional_scopes {
            let current_indent = *self.indent_storage.last().unwrap_or(&0);
            self.push_scope(scope, current_indent, params.continuation_indent);
        }

        self.close_labeled_do(&filtered_line, params.label, new_scope.is_some());

        self.initial = false;
    }

    /// Track and close labeled DO constructs.
    ///
    /// `do 100 i = 1, 10` is closed by the statement carrying label 100, not
    /// by `END DO`. Without this the scope would never be popped and every
    /// following line would drift one level deeper. Several DOs may share a
    /// terminating label, so close as many as match.
    ///
    /// The terminating statement belongs to the loop body, so this runs after
    /// the line's own indent has been calculated.
    fn close_labeled_do(&mut self, filtered_line: &str, label: &str, opened_scope: bool) {
        // Drop entries whose scope an `END DO` already closed
        self.do_labels
            .retain(|(depth, _)| *depth <= self.scope_storage.len());

        let label = label.trim();
        if !label.is_empty() {
            while let Some((depth, do_label)) = self.do_labels.last() {
                if do_label != label {
                    break;
                }
                let depth = *depth;
                while self.scope_storage.len() >= depth {
                    self.scope_storage.pop();
                    if self.indent_storage.len() > 1 {
                        self.indent_storage.pop();
                    }
                }
                self.do_labels.pop();
            }
        }

        if !opened_scope {
            return;
        }
        if let Some(caps) = DO_LABEL_RE.captures(filtered_line) {
            if let Some(do_label) = caps.get(2) {
                self.do_labels
                    .push((self.scope_storage.len(), do_label.as_str().to_string()));
            }
        }
    }

    /// Scan the line for END statements and pop the scope stack for a
    /// leading END. Checks each semicolon-separated part separately so
    /// lines like "end do; end do" count every END.
    fn detect_end_statements(
        &mut self,
        parts: &[&str],
        filtered_line: &str,
        indent_fypp: bool,
    ) -> EndDetection {
        let mut end = EndDetection {
            is_any: false,
            valid: false,
            additional_count: 0,
            valid_after_semicolon: false,
        };

        for (part_idx, part) in parts.iter().enumerate() {
            let part_check = format!("  {}", part.trim());

            for (scope_idx, scope) in self.parser.iter() {
                if let Some(close) = scope.close {
                    if close.is_match(&part_check) {
                        // Set flag if ANY part contains an END statement (not just part 0)
                        // This prevents opening a new scope when there's a balanced
                        // open/close on the same line (e.g., "do i=1,n; ...; end do")
                        end.is_any = true;

                        if part_idx == 0 {
                            // - If scopes is non-empty: ALWAYS pop, then check if matches
                            // - If scopes is empty: the END is valid
                            if self.scope_storage.is_empty() {
                                end.valid = true;
                            } else {
                                // Pop from scope_storage
                                // Safe: we just verified scope_storage is not empty
                                let popped_scope = self
                                    .scope_storage
                                    .pop()
                                    .expect("scope_storage should be non-empty (checked above)");
                                // Check if it matches
                                // When indent_fypp=True and the popped scope was a fypp scope,
                                // consider the END valid. This allows Fortran END statements
                                // to close orphan fypp scopes.
                                let popped_fypp_scope = SCOPES[popped_scope].is_fypp();
                                if !scope.spec
                                    || popped_scope == scope_idx
                                    || (indent_fypp && popped_fypp_scope)
                                {
                                    end.valid = true;
                                } else {
                                    // The END closes a construct we never opened
                                    // (an opener pattern we don't recognize). Put
                                    // the scope back: dropping it would desync
                                    // scope_storage from indent_storage and dedent
                                    // the rest of the file.
                                    self.scope_storage.push(popped_scope);
                                }
                            }
                        } else {
                            // Additional END statements after semicolon
                            // Check if this END is balanced by an opener on the same line
                            // If yes, don't count it (they cancel out)
                            let has_matching_opener = self
                                .parser
                                .get(scope_idx)
                                .and_then(|s| s.open)
                                .is_some_and(|open| open.is_match(filtered_line));

                            if !has_matching_opener {
                                // No matching opener on this line, so this END closes an outer scope
                                // Check if this END matches the current scope (for indentation purposes)
                                if !self.scope_storage.is_empty() {
                                    let current_scope = self.scope_storage.last().copied();
                                    if let Some(open_idx) = current_scope {
                                        let popped_fypp_scope = SCOPES[open_idx].is_fypp();
                                        if !scope.spec
                                            || open_idx == scope_idx
                                            || (indent_fypp && popped_fypp_scope)
                                        {
                                            end.valid_after_semicolon = true;
                                        }
                                    }
                                }
                                end.additional_count += 1;
                            }
                        }
                        break;
                    }
                }
            }
        }

        end
    }

    /// Check whether the line continues an open scope (ELSE, CASE, CONTAINS,
    /// #:else, ...). Returns `(is_continue, fypp_continue_pop_to)`: the
    /// latter is the target stack length when a fypp continuation needs
    /// inner Fortran scopes popped after the indent is calculated.
    fn detect_continuation(&self, filtered_line: &str) -> (bool, Option<usize>) {
        for (scope_idx, scope) in self.parser.iter() {
            if let Some(cont) = scope.cont {
                if cont.is_match(filtered_line) {
                    // For fypp continuation directives (#:else, #:elif, etc.),
                    // we need to search the ENTIRE scope stack, not just the top.
                    // This handles cases where Fortran scopes (like DO) are opened
                    // inside a fypp block - the #:else should continue the fypp scope,
                    // not the inner Fortran scope.
                    if scope.is_fypp() {
                        // Search entire stack for matching fypp scope
                        let found_idx = self
                            .scope_storage
                            .iter()
                            .rposition(|open_idx| *open_idx == scope_idx);
                        if let Some(idx) = found_idx {
                            // Found matching fypp scope
                            // Record position to pop inner scopes AFTER calculating indent
                            // This way the continuation directive uses grandparent indent,
                            // but subsequent lines will use the fypp body level
                            let pop_to = if self.scope_storage.len() > idx + 1 {
                                Some(idx + 1)
                            } else {
                                None
                            };
                            return (true, pop_to);
                        }
                    } else {
                        // Regular Fortran continuation - check top of stack only
                        if self.scope_storage.last() == Some(&scope_idx) {
                            return (true, None);
                        }
                    }
                }
            }
        }
        (false, None)
    }

    /// Check whether the line opens new scopes. Returns the scope opened by
    /// the first statement plus any opened by statements after semicolons.
    fn detect_new_scopes(
        &self,
        parts: &[&str],
        filtered_line: &str,
    ) -> (Option<usize>, Vec<usize>) {
        let mut new_scope = None;
        let mut additional_scopes: Vec<usize> = Vec::new();

        for (scope_idx, scope) in self.parser.iter() {
            if let Some(open) = scope.open {
                if open.is_match(filtered_line) {
                    // WHERE/FORALL only open a scope if nothing follows the
                    // closing parenthesis: "WHERE (x > 0)" opens one, but
                    // "WHERE (x > 0) y = 1" is a single statement.
                    if !scope.conditional || Self::is_where_forall_block(filtered_line) {
                        new_scope = Some(scope_idx);
                    }
                    break;
                }
            }
        }

        // Check for additional scope openers after semicolons
        // Split by semicolon and check each part (except the first which was checked above)
        // Scan regardless of whether part 0 opened a scope: a statement like
        // "x = 1; if (y) then" opens an IF scope after the semicolon.
        if filtered_line.contains(';') {
            for part in parts.iter().skip(1) {
                let part_trimmed = part.trim();
                for (scope_idx, scope) in self.parser.iter() {
                    if let Some(open) = scope.open {
                        // Create a temporary string that looks like a line start
                        let temp_line = format!("  {part_trimmed}");
                        if open.is_match(&temp_line) {
                            if !scope.conditional || Self::is_where_forall_block(&temp_line) {
                                additional_scopes.push(scope_idx);
                            }
                            break;
                        }
                    }
                }
            }
        }

        (new_scope, additional_scopes)
    }

    /// Calculate indent for the current line
    fn calculate_indent(
        &self,
        is_continue: bool,
        is_any_end_statement: bool,
        valid_end: bool,
    ) -> usize {
        let parent_indent = *self.indent_storage.last().unwrap_or(&0);

        if is_any_end_statement && valid_end {
            // Valid END: use grandparent indent
            if self.indent_storage.len() >= 2 {
                self.indent_storage[self.indent_storage.len() - 2]
            } else if self.initial {
                parent_indent
            } else {
                0
            }
        } else if is_continue {
            // CONTINUE (ELSE/CASE for Fortran, #:else/#:elif for fypp):
            // MCNP style: CASE statements should NOT be indented (same level as SELECT)
            // Standard style: ELSE statements use grandparent indent
            // All continuation statements use grandparent indent
            if self.indent_storage.len() >= 2 {
                self.indent_storage[self.indent_storage.len() - 2]
            } else {
                0
            }
        } else {
            // Invalid END, NEW scope, or regular line: use current indent
            parent_indent
        }
    }

    /// Push a new scope onto the stack
    fn push_scope(&mut self, scope: usize, current_indent: usize, relative_indent: usize) {
        self.scope_storage.push(scope);
        self.indent_storage.push(current_indent + relative_indent);
    }

    /// Get the calculated line indents
    #[must_use]
    pub fn get_lines_indent(&self) -> &[usize] {
        &self.line_indents
    }

    /// Get the current Fortran line indent (scope indent)
    #[must_use]
    pub fn get_scope_indent(&self) -> usize {
        *self.indent_storage.last().unwrap_or(&0)
    }

    /// Get current scope depth. The indent this produces is the public
    /// signal; this exists so the tests can watch the stack itself unwind.
    #[cfg(test)]
    fn scope_depth(&self) -> usize {
        self.scope_storage.len()
    }

    /// Check if WHERE/FORALL line is a block construct (not single-line)
    ///
    /// Returns true if there's nothing after the closing parenthesis (block construct),
    /// false if there's an assignment after it (single-line statement)
    ///
    /// Examples:
    /// - `WHERE (x > 0)` -> true (block)
    /// - `WHERE (x > 0) y = 1` -> false (single-line)
    fn is_where_forall_block(line: &str) -> bool {
        let mut level = 0;
        let mut in_parens = false;

        // Comments and strings are masked, but not fypp: a `${...}` after the
        // `)` is content, so this is the one caller that cannot use code()
        for (pos, ch) in CharFilter::new(line, true, true, false) {
            match ch {
                '(' => {
                    level += 1;
                    in_parens = true;
                }
                ')' => {
                    level -= 1;
                    if level == 0 && in_parens {
                        // Found the closing paren of WHERE/FORALL
                        // Check if there's anything after it (besides whitespace/comments)
                        let after = &line[pos + 1..];
                        // If only whitespace or comment remains, it's a block construct
                        let trimmed = after.trim();
                        return trimmed.is_empty() || trimmed.starts_with('!');
                    }
                }
                _ => {}
            }
        }
        // If we didn't find a closing paren, assume it's a block
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scope::build_scope_parser;

    #[test]
    fn test_simple_if() {
        let parser = build_scope_parser(false, false);
        let mut indenter = F90Indenter::new(parser, 0);
        let params = IndentParams::new(3);

        // IF (x > 0) THEN
        let lines = vec!["if (x > 0) then".to_string()];
        indenter.process_logical_line("if (x > 0) then", &lines, &params);

        let indents = indenter.get_lines_indent();
        assert_eq!(indents[0], 0); // IF line at base level

        // x = 5
        let lines = vec!["x = 5".to_string()];
        indenter.process_logical_line("x = 5", &lines, &params);

        let indents = indenter.get_lines_indent();
        assert_eq!(indents[0], 3); // Inside IF, indented

        // END IF
        let lines = vec!["end if".to_string()];
        indenter.process_logical_line("end if", &lines, &params);

        let indents = indenter.get_lines_indent();
        assert_eq!(indents[0], 0); // END IF back to base
    }

    #[test]
    fn test_labeled_do_closes_on_its_label() {
        // `do 100 ...` is closed by the statement labeled 100, not by END DO
        let parser = build_scope_parser(false, true);
        let mut indenter = F90Indenter::new(parser, 0);
        let mut params = IndentParams::new(3);

        let line = |s: &str| vec![s.to_string()];
        indenter.process_logical_line("do 100 i = 1, 10", &line("do 100 i = 1, 10"), &params);
        assert_eq!(indenter.get_lines_indent()[0], 0);

        indenter.process_logical_line("x = 1", &line("x = 1"), &params);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // The terminating statement is part of the loop body
        params.label = "100";
        indenter.process_logical_line("continue", &line("continue"), &params);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // ... and the loop is closed after it
        params.label = "";
        indenter.process_logical_line("y = 2", &line("y = 2"), &params);
        assert_eq!(indenter.get_lines_indent()[0], 0);
    }

    #[test]
    fn test_labeled_do_shared_terminator() {
        // Nested DOs may share one terminating label
        let parser = build_scope_parser(false, true);
        let mut indenter = F90Indenter::new(parser, 0);
        let mut params = IndentParams::new(3);

        let line = |s: &str| vec![s.to_string()];
        indenter.process_logical_line("do 200 i = 1, 3", &line("do 200 i = 1, 3"), &params);
        indenter.process_logical_line("do 200 j = 1, 3", &line("do 200 j = 1, 3"), &params);
        indenter.process_logical_line("x = 1", &line("x = 1"), &params);
        assert_eq!(indenter.get_lines_indent()[0], 6);

        params.label = "200";
        indenter.process_logical_line("continue", &line("continue"), &params);
        params.label = "";
        indenter.process_logical_line("y = 2", &line("y = 2"), &params);
        assert_eq!(indenter.get_lines_indent()[0], 0);
    }

    #[test]
    fn test_labeled_block_do_still_ends_with_end_do() {
        // The obsolescent labeled block DO ends at `100 END DO`; the label
        // bookkeeping must not pop a second time.
        let parser = build_scope_parser(false, true);
        let mut indenter = F90Indenter::new(parser, 0);
        let mut params = IndentParams::new(3);

        let line = |s: &str| vec![s.to_string()];
        indenter.process_logical_line("module m", &line("module m"), &params);
        indenter.process_logical_line("do 100 i = 1, 10", &line("do 100 i = 1, 10"), &params);
        indenter.process_logical_line("x = 1", &line("x = 1"), &params);
        assert_eq!(indenter.get_lines_indent()[0], 6);

        params.label = "100";
        indenter.process_logical_line("end do", &line("end do"), &params);
        params.label = "";
        indenter.process_logical_line("y = 2", &line("y = 2"), &params);
        assert_eq!(indenter.get_lines_indent()[0], 3);
    }

    #[test]
    fn test_unmatched_end_keeps_scope() {
        // An END for a construct we never opened (its opener pattern is not
        // recognized) must not pop the enclosing scope, or every later line
        // dedents.
        let parser = build_scope_parser(false, true);
        let mut indenter = F90Indenter::new(parser, 0);
        let params = IndentParams::new(3);

        indenter.process_logical_line("module m", &["module m".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 0);

        // Not recognized as a TYPE opener (parameterized derived type)
        let pdt = "type :: matrix(k, n)";
        indenter.process_logical_line(pdt, &[pdt.to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        indenter.process_logical_line("end type matrix", &["end type matrix".to_string()], &params);

        // Still inside the module
        indenter.process_logical_line("integer :: i", &["integer :: i".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 3);
        // CONTAINS still recognized as continuing the module scope
        indenter.process_logical_line("contains", &["contains".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 0);
    }

    #[test]
    fn test_if_else() {
        let parser = build_scope_parser(false, false);
        let mut indenter = F90Indenter::new(parser, 0);
        let params = IndentParams::new(3);

        // IF
        indenter.process_logical_line("if (x) then", &["if (x) then".to_string()], &params);

        // Inside IF
        indenter.process_logical_line("x = 1", &["x = 1".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // ELSE
        indenter.process_logical_line("else", &["else".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 0); // ELSE at same level as IF

        // Inside ELSE
        indenter.process_logical_line("x = 2", &["x = 2".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 3);
    }

    #[test]
    fn test_nested_if() {
        let parser = build_scope_parser(false, false);
        let mut indenter = F90Indenter::new(parser, 0);
        let params = IndentParams::new(3);

        // Outer IF
        indenter.process_logical_line("if (a) then", &["if (a) then".to_string()], &params);
        assert_eq!(indenter.scope_depth(), 1);

        // Inner IF
        indenter.process_logical_line("if (b) then", &["if (b) then".to_string()], &params);
        assert_eq!(indenter.scope_depth(), 2);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // Inside inner IF
        indenter.process_logical_line("x = 1", &["x = 1".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 6);

        // End inner IF
        indenter.process_logical_line("end if", &["end if".to_string()], &params);
        assert_eq!(indenter.scope_depth(), 1);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // End outer IF
        indenter.process_logical_line("end if", &["end if".to_string()], &params);
        assert_eq!(indenter.scope_depth(), 0);
        assert_eq!(indenter.get_lines_indent()[0], 0);
    }

    #[test]
    fn test_do_loop() {
        let parser = build_scope_parser(false, false);
        let mut indenter = F90Indenter::new(parser, 0);
        let params = IndentParams::new(3);

        // DO
        indenter.process_logical_line("do i = 1, 10", &["do i = 1, 10".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 0);

        // Inside DO
        indenter.process_logical_line("x = i", &["x = i".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // END DO
        indenter.process_logical_line("end do", &["end do".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 0);
    }

    #[test]
    fn test_subroutine() {
        let parser = build_scope_parser(false, false);
        let mut indenter = F90Indenter::new(parser, 0);
        let params = IndentParams::new(3);

        // SUBROUTINE
        indenter.process_logical_line(
            "subroutine foo()",
            &["subroutine foo()".to_string()],
            &params,
        );

        // Inside subroutine
        indenter.process_logical_line("x = 1", &["x = 1".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // END SUBROUTINE
        indenter.process_logical_line("end subroutine", &["end subroutine".to_string()], &params);
        assert_eq!(indenter.get_lines_indent()[0], 0);
    }

    #[test]
    fn test_fypp_if_scope() {
        // Test that fypp #:if opens a scope and adds indentation
        let parser = build_scope_parser(true, false); // indent_fypp = true
        let mut indenter = F90Indenter::new(parser, 0);
        let params = IndentParams::new(3);

        // #:if DEBUG > 0
        let lines = vec!["#:if DEBUG > 0".to_string()];
        indenter.process_logical_line("#:if DEBUG > 0", &lines, &params);

        // The #:if line itself should be at indent 0
        assert_eq!(
            indenter.get_lines_indent()[0],
            0,
            "#:if should be at column 0"
        );
        // Scope should be opened
        assert_eq!(
            indenter.scope_depth(),
            1,
            "Scope should be opened after #:if"
        );

        // print *, "inside if"
        let lines = vec!["print *, \"inside if\"".to_string()];
        indenter.process_logical_line("print *, \"inside if\"", &lines, &params);

        // This should be indented (inside the #:if scope)
        assert_eq!(
            indenter.get_lines_indent()[0],
            3,
            "Code inside #:if should be indented"
        );

        // #:endif
        let lines = vec!["#:endif".to_string()];
        indenter.process_logical_line("#:endif", &lines, &params);

        // The #:endif line should close the scope
        assert_eq!(
            indenter.get_lines_indent()[0],
            0,
            "#:endif should be at column 0"
        );
        assert_eq!(
            indenter.scope_depth(),
            0,
            "Scope should be closed after #:endif"
        );
    }
}
