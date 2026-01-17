/// `F90Indenter` - Scope-based indentation tracker
///
/// Uses a stack to track open scopes (IF, DO, MODULE, etc.) and
/// calculates indentation levels based on scope depth.
use crate::error::Result;
use crate::format::aligner::F90Aligner;
use crate::parser::char_filter::CharFilter;
use crate::scope::{ScopeParser, ScopeType};

/// `F90Indenter` tracks scope nesting and calculates indentation
pub struct F90Indenter {
    /// Stack of open scope types
    scope_storage: Vec<ScopeType>,
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
        }
    }

    /// Process lines of a Fortran logical line
    ///
    /// This is the core indentation calculation function.
    ///
    /// # Arguments
    /// * `logical_line` - The logical Fortran line (continuations joined)
    /// * `lines` - The original physical lines
    /// * `relative_indent` - Relative indent size
    /// * `continuation_indent` - Relative indent for continuations
    /// * `indent_fypp` - Whether to indent fypp preprocessor blocks
    /// * `manual_lines_indent` - Manual indent override from aligner
    /// * `use_same_line` - If true and there are continuations, force minimal indent for all
    /// * `semicolon_line_index` - If Some(idx), force minimal indent only for lines AFTER idx
    #[allow(clippy::too_many_arguments)]
    pub fn process_logical_line(
        &mut self,
        logical_line: &str,
        lines: &[String],
        relative_indent: usize,
        continuation_indent: usize,
        indent_fypp: bool,
        manual_lines_indent: Option<&[usize]>,
        use_same_line: bool,
        semicolon_line_index: Option<usize>,
    ) -> Result<()> {
        self.line_indents.clear();

        // Filter the line to get only code (no strings/comments)
        let filtered_line = CharFilter::new(logical_line, true, true, true).filter_all();

        // Split by semicolon for multi-statement handling
        let parts: Vec<&str> = filtered_line.split(';').collect();

        // Check for scope closing (end) FIRST
        // This must be checked before NEW because patterns like SUBR_RE can match "end subroutine"
        // Also detect multiple END statements on the same line (semicolon separated)
        // For lines like "end do; end do", we need to check each part separately
        let mut is_any_end_statement = false;
        let mut valid_end = false;
        let mut additional_end_count = 0;

        for (part_idx, part) in parts.iter().enumerate() {
            let part_check = format!("  {}", part.trim());

            for (scope_idx, parser_opt) in self.parser.closing.iter().enumerate() {
                if let Some(parser) = parser_opt {
                    if parser.is_match(&part_check) {
                        if part_idx == 0 {
                            is_any_end_statement = true;
                            // - If scopes is non-empty: ALWAYS pop, then check if matches
                            // - If scopes is empty: valid_end = true
                            if self.scope_storage.is_empty() {
                                valid_end = true;
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
                                let popped_fypp_scope = popped_scope.is_fypp_scope();
                                if !parser.spec
                                    || popped_scope.as_index() == scope_idx
                                    || (indent_fypp && popped_fypp_scope)
                                {
                                    valid_end = true;
                                }
                                // Note: we don't put it back
                            }
                        } else {
                            // Additional END statements after semicolon
                            additional_end_count += 1;
                        }
                        break;
                    }
                }
            }
        }

        // Check for scope continuation (e.g., ELSE, CASE, CONTAINS)
        let mut is_continue = false;
        // Track where to pop scopes for fypp continuation
        let mut fypp_continue_pop_to: Option<usize> = None;
        for (scope_idx, parser_opt) in self.parser.continue_.iter().enumerate() {
            if let Some(parser) = parser_opt {
                if parser.is_match(&filtered_line) {
                    // For fypp continuation directives (#:else, #:elif, etc.),
                    // we need to search the ENTIRE scope stack, not just the top.
                    // This handles cases where Fortran scopes (like DO) are opened
                    // inside a fypp block - the #:else should continue the fypp scope,
                    // not the inner Fortran scope.
                    let is_fypp_scope = ScopeType::is_fypp_scope_index(scope_idx);

                    if is_fypp_scope {
                        // Search entire stack for matching fypp scope
                        let mut found_idx = None;
                        for (i, &scope) in self.scope_storage.iter().enumerate().rev() {
                            if scope.as_index() == scope_idx {
                                found_idx = Some(i);
                                break;
                            }
                        }
                        if let Some(idx) = found_idx {
                            // Found matching fypp scope
                            // Record position to pop inner scopes AFTER calculating indent
                            // This way the continuation directive uses grandparent indent,
                            // but subsequent lines will use the fypp body level
                            is_continue = true;
                            if self.scope_storage.len() > idx + 1 {
                                fypp_continue_pop_to = Some(idx + 1);
                            }
                            break;
                        }
                    } else {
                        // Regular Fortran continuation - check top of stack only
                        if let Some(&current_scope) = self.scope_storage.last() {
                            if current_scope.as_index() == scope_idx {
                                is_continue = true;
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Check for scope opening (new) LAST
        // Skip if END statement (to avoid SUBR_RE matching "end subroutine")
        let mut new_scope = None;
        let mut additional_scopes: Vec<ScopeType> = Vec::new();

        // Only check for new scope if this is NOT an end statement
        // (scope_storage was already popped if it was an END)
        if !is_any_end_statement {
            for (scope_idx, parser_opt) in self.parser.opening.iter().enumerate() {
                if let Some(parser) = parser_opt {
                    if parser.is_match(&filtered_line) {
                        new_scope = ScopeType::from_index(scope_idx);
                        break;
                    }
                }
            }

            // Check for additional scope openers after semicolons
            // Split by semicolon and check each part (except the first which was checked above)
            if new_scope.is_some() && filtered_line.contains(';') {
                for part in parts.iter().skip(1) {
                    let part_trimmed = part.trim();
                    for (scope_idx, parser_opt) in self.parser.opening.iter().enumerate() {
                        if let Some(parser) = parser_opt {
                            // Create a temporary string that looks like a line start
                            let temp_line = format!("  {part_trimmed}");
                            if parser.is_match(&temp_line) {
                                if let Some(scope) = ScopeType::from_index(scope_idx) {
                                    additional_scopes.push(scope);
                                }
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Calculate indent for this line
        // Note: scope_storage was already popped if is_any_end_statement=true
        let line_indent =
            self.calculate_indent(new_scope, is_continue, is_any_end_statement, valid_end);

        // For continuation lines, calculate alignment BEFORE updating scope stack
        if lines.len() > 1 {
            // Get continuation indents from either:
            // 1. Manual alignment (when lines have leading &) - preserves original positioning
            // 2. Automatic alignment via F90Aligner
            let continuation_indents: Vec<usize> = if let Some(manual_indent) = manual_lines_indent
            {
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
                    continuation_indent,
                )?;

                self.aligner.get_lines_indent().to_vec()
            };

            // First line gets the calculated indent
            self.line_indents.push(line_indent);

            // Determine base indent to add to alignment results
            // - valid END/CONTINUE: indents[-2] (grandparent)
            // - invalid END: indents[-1] (current)
            // - NEW scopes: indents[-1] (current)
            // - Regular: indents[-1] (current)
            let base_indent = if (is_any_end_statement && valid_end) || is_continue {
                // For valid END and CONTINUE, use grandparent indent
                if self.indent_storage.len() >= 2 {
                    self.indent_storage[self.indent_storage.len() - 2]
                } else {
                    *self.indent_storage.last().unwrap_or(&0)
                }
            } else {
                // For invalid END, NEW scopes, and regular lines: use current indent
                *self.indent_storage.last().unwrap_or(&0)
            };

            // Continuation lines: alignment returns relative offsets, add base
            // When use_same_line=true, force minimal indent (1) for ALL continuations
            // When semicolon_line_index=Some(idx), force minimal indent only for lines AFTER idx
            if use_same_line {
                // Force indent=1 for all continuation lines
                for _ in continuation_indents.iter().skip(1) {
                    self.line_indents.push(1);
                }
            } else if let Some(semicolon_idx) = semicolon_line_index {
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
        if is_any_end_statement && valid_end {
            if self.indent_storage.len() > 1 {
                self.indent_storage.pop();
            } else if let Some(last) = self.indent_storage.last_mut() {
                *last = 0;
            }

            // Pop additional END scopes found after semicolons
            for _ in 0..additional_end_count {
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
            self.push_scope(scope, line_indent, relative_indent);

            // Push additional scopes found after semicolons
            // Each additional scope adds another level of indentation
            for scope in additional_scopes {
                let current_indent = *self.indent_storage.last().unwrap_or(&0);
                self.push_scope(scope, current_indent, continuation_indent);
            }
        }

        self.initial = false;
        Ok(())
    }

    /// Calculate indent for the current line
    fn calculate_indent(
        &self,
        new_scope: Option<ScopeType>,
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
        } else if is_any_end_statement {
            // Invalid END: use current indent
            parent_indent
        } else if is_continue {
            // CONTINUE (ELSE/CASE for Fortran, #:else/#:elif for fypp):
            // use grandparent indent (like valid END)
            if self.indent_storage.len() >= 2 {
                self.indent_storage[self.indent_storage.len() - 2]
            } else {
                0
            }
        } else if new_scope.is_some() {
            // NEW scope: use current indent
            parent_indent
        } else {
            // Regular line: use current indent
            parent_indent
        }
    }

    /// Push a new scope onto the stack
    fn push_scope(&mut self, scope: ScopeType, current_indent: usize, relative_indent: usize) {
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

    /// Get current scope depth
    #[must_use]
    pub fn scope_depth(&self) -> usize {
        self.scope_storage.len()
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

        // IF (x > 0) THEN
        let lines = vec!["if (x > 0) then".to_string()];
        indenter
            .process_logical_line("if (x > 0) then", &lines, 3, 3, false, None, false, None)
            .unwrap();

        let indents = indenter.get_lines_indent();
        assert_eq!(indents[0], 0); // IF line at base level

        // x = 5
        let lines = vec!["x = 5".to_string()];
        indenter
            .process_logical_line("x = 5", &lines, 3, 3, false, None, false, None)
            .unwrap();

        let indents = indenter.get_lines_indent();
        assert_eq!(indents[0], 3); // Inside IF, indented

        // END IF
        let lines = vec!["end if".to_string()];
        indenter
            .process_logical_line("end if", &lines, 3, 3, false, None, false, None)
            .unwrap();

        let indents = indenter.get_lines_indent();
        assert_eq!(indents[0], 0); // END IF back to base
    }

    #[test]
    fn test_if_else() {
        let parser = build_scope_parser(false, false);
        let mut indenter = F90Indenter::new(parser, 0);

        // IF
        indenter
            .process_logical_line(
                "if (x) then",
                &["if (x) then".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();

        // Inside IF
        indenter
            .process_logical_line(
                "x = 1",
                &["x = 1".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // ELSE
        indenter
            .process_logical_line(
                "else",
                &["else".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.get_lines_indent()[0], 0); // ELSE at same level as IF

        // Inside ELSE
        indenter
            .process_logical_line(
                "x = 2",
                &["x = 2".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.get_lines_indent()[0], 3);
    }

    #[test]
    fn test_nested_if() {
        let parser = build_scope_parser(false, false);
        let mut indenter = F90Indenter::new(parser, 0);

        // Outer IF
        indenter
            .process_logical_line(
                "if (a) then",
                &["if (a) then".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.scope_depth(), 1);

        // Inner IF
        indenter
            .process_logical_line(
                "if (b) then",
                &["if (b) then".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.scope_depth(), 2);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // Inside inner IF
        indenter
            .process_logical_line(
                "x = 1",
                &["x = 1".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.get_lines_indent()[0], 6);

        // End inner IF
        indenter
            .process_logical_line(
                "end if",
                &["end if".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.scope_depth(), 1);
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // End outer IF
        indenter
            .process_logical_line(
                "end if",
                &["end if".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.scope_depth(), 0);
        assert_eq!(indenter.get_lines_indent()[0], 0);
    }

    #[test]
    fn test_do_loop() {
        let parser = build_scope_parser(false, false);
        let mut indenter = F90Indenter::new(parser, 0);

        // DO
        indenter
            .process_logical_line(
                "do i = 1, 10",
                &["do i = 1, 10".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.get_lines_indent()[0], 0);

        // Inside DO
        indenter
            .process_logical_line(
                "x = i",
                &["x = i".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // END DO
        indenter
            .process_logical_line(
                "end do",
                &["end do".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.get_lines_indent()[0], 0);
    }

    #[test]
    fn test_subroutine() {
        let parser = build_scope_parser(false, false);
        let mut indenter = F90Indenter::new(parser, 0);

        // SUBROUTINE
        indenter
            .process_logical_line(
                "subroutine foo()",
                &["subroutine foo()".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();

        // Inside subroutine
        indenter
            .process_logical_line(
                "x = 1",
                &["x = 1".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.get_lines_indent()[0], 3);

        // END SUBROUTINE
        indenter
            .process_logical_line(
                "end subroutine",
                &["end subroutine".to_string()],
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();
        assert_eq!(indenter.get_lines_indent()[0], 0);
    }

    #[test]
    fn test_fypp_if_scope() {
        // Test that fypp #:if opens a scope and adds indentation
        let parser = build_scope_parser(true, false); // indent_fypp = true
        let mut indenter = F90Indenter::new(parser, 0);

        // #:if DEBUG > 0
        let lines = vec!["#:if DEBUG > 0".to_string()];
        indenter
            .process_logical_line("#:if DEBUG > 0", &lines, 3, 3, false, None, false, None)
            .unwrap();

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
        indenter
            .process_logical_line(
                "print *, \"inside if\"",
                &lines,
                3,
                3,
                false,
                None,
                false,
                None,
            )
            .unwrap();

        // This should be indented (inside the #:if scope)
        assert_eq!(
            indenter.get_lines_indent()[0],
            3,
            "Code inside #:if should be indented"
        );

        // #:endif
        let lines = vec!["#:endif".to_string()];
        indenter
            .process_logical_line("#:endif", &lines, 3, 3, false, None, false, None)
            .unwrap();

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
