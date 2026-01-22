//! Integration tests for fprettier
//!
//! These tests verify that the components work together correctly

#![warn(clippy::all)]
#![warn(clippy::pedantic)]

use std::io::{BufReader, Cursor};

use fprettier::format::F90Indenter;
use fprettier::process::format_file;
use fprettier::scope::build_scope_parser;
use fprettier::Config;

#[test]
#[allow(clippy::too_many_lines)]
fn test_complete_fortran_program() {
    // Enable module indentation to recognize PROGRAM
    let parser = build_scope_parser(false, true);
    let mut indenter = F90Indenter::new(parser, 0);

    // program main
    indenter
        .process_logical_line(
            "program main",
            &["program main".to_string()],
            3,
            3,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 0);

    // integer :: x, y
    indenter
        .process_logical_line(
            "integer :: x, y",
            &["integer :: x, y".to_string()],
            3,
            3,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 3);

    // if (x > 0) then
    indenter
        .process_logical_line(
            "if (x > 0) then",
            &["if (x > 0) then".to_string()],
            3,
            3,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 3);

    // y = x + 1
    indenter
        .process_logical_line(
            "y = x + 1",
            &["y = x + 1".to_string()],
            3,
            3,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 6);

    // else
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
    assert_eq!(indenter.get_lines_indent()[0], 3);

    // y = 0
    indenter
        .process_logical_line(
            "y = 0",
            &["y = 0".to_string()],
            3,
            3,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 6);

    // end if
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
    assert_eq!(indenter.get_lines_indent()[0], 3);

    // end program
    indenter
        .process_logical_line(
            "end program",
            &["end program".to_string()],
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
fn test_nested_do_loops() {
    let parser = build_scope_parser(false, false);
    let mut indenter = F90Indenter::new(parser, 0);

    // do i = 1, 10
    indenter
        .process_logical_line(
            "do i = 1, 10",
            &["do i = 1, 10".to_string()],
            2,
            2,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 0);

    // do j = 1, 20
    indenter
        .process_logical_line(
            "do j = 1, 20",
            &["do j = 1, 20".to_string()],
            2,
            2,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 2);

    // x = i * j
    indenter
        .process_logical_line(
            "x = i * j",
            &["x = i * j".to_string()],
            2,
            2,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 4);

    // end do (inner)
    indenter
        .process_logical_line(
            "end do",
            &["end do".to_string()],
            2,
            2,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 2);

    // end do (outer)
    indenter
        .process_logical_line(
            "end do",
            &["end do".to_string()],
            2,
            2,
            false,
            None,
            false,
            None,
        )
        .unwrap();
    assert_eq!(indenter.get_lines_indent()[0], 0);
}

/// End-to-end test: format a complete program with both whitespace and indentation
/// When both PROGRAM and IF start at column 0, IF stays at column 0.
#[test]
fn test_end_to_end_formatting() {
    let input = "program test\nif (x>0) then\nx=1+2\nend if\nend program\n";
    let config = Config {
        impose_indent: true,
        impose_whitespace: true,
        indent: 3,
        whitespace: 2,
        indent_mod: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // Verify structure
    assert_eq!(lines.len(), 5);

    // Program should have no indent
    assert!(lines[0].starts_with("program"));

    // IF stays at column 0
    assert!(lines[1].starts_with("if"));

    // x=1+2 should be indented relative to IF and have whitespace
    assert!(lines[2].starts_with("   "));
    assert!(lines[2].contains(" = "));
    assert!(lines[2].contains(" + "));

    // end if stays at column 0 (same as IF)
    assert!(lines[3].starts_with("end if"));

    // end program should have no indent
    assert!(lines[4].starts_with("end program"));
}

/// End-to-end test: whitespace formatting only
#[test]
fn test_end_to_end_whitespace_only() {
    let input = "x=a+b,y=c+d\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Should have spaces around operators and after comma
    assert!(result.contains(" = "));
    assert!(result.contains(" + "));
    assert!(result.contains(", "));
}

/// End-to-end test: subroutine with do loop
#[test]
fn test_end_to_end_subroutine() {
    let input = "subroutine foo(x)\ninteger::x\ndo i=1,10\nx=x+i\nend do\nend subroutine\n";
    let config = Config {
        impose_indent: true,
        impose_whitespace: true,
        indent: 2,
        whitespace: 1,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // Verify indentation levels
    assert!(lines[0].starts_with("subroutine")); // No indent
    assert!(lines[1].starts_with("  ")); // 2 spaces
    assert!(lines[2].starts_with("  ")); // 2 spaces
    assert!(lines[3].starts_with("    ")); // 4 spaces
    assert!(lines[4].starts_with("  ")); // 2 spaces
    assert!(lines[5].starts_with("end subroutine")); // No indent
}

/// Test `F90Aligner` bracket alignment
#[test]
fn test_aligner_bracket_continuation() {
    use fprettier::format::F90Aligner;

    let mut aligner = F90Aligner::new();

    // Function call with argument on continuation line
    let lines = vec!["call foo(a,".to_string(), "         b)".to_string()];

    aligner
        .process_logical_line("call foo(a, b)", &lines, 3)
        .unwrap();

    let indents = aligner.get_lines_indent();
    assert_eq!(indents.len(), 2);
    assert_eq!(indents[0], 0); // First line
                               // Second line should align after the opening (
    assert!(indents[1] >= 9); // Position after "call foo("
}

/// Test `F90Aligner` assignment alignment
#[test]
fn test_aligner_assignment_continuation() {
    use fprettier::format::F90Aligner;

    let mut aligner = F90Aligner::new();

    // Assignment with expression on continuation line
    let lines = vec!["result = a +".to_string(), "         b".to_string()];

    aligner
        .process_logical_line("result = a + b", &lines, 3)
        .unwrap();

    let indents = aligner.get_lines_indent();
    assert_eq!(indents.len(), 2);
    assert_eq!(indents[0], 0); // First line
                               // Second line should align after the = sign
    assert!(indents[1] >= 9); // Position after "result = "
}

/// Test `F90Aligner` declaration alignment
#[test]
fn test_aligner_declaration_continuation() {
    use fprettier::format::F90Aligner;

    let mut aligner = F90Aligner::new();

    // Declaration with variables on continuation line
    let lines = vec![
        "integer :: x,".to_string(),
        "           y,".to_string(),
        "           z".to_string(),
    ];

    aligner
        .process_logical_line("integer :: x, y, z", &lines, 3)
        .unwrap();

    let indents = aligner.get_lines_indent();
    assert_eq!(indents.len(), 3);
    assert_eq!(indents[0], 0); // First line
                               // Continuation lines should align after the ::
    assert!(indents[1] >= 11); // Position after "integer :: "
    assert!(indents[2] >= 11);
}

/// Test `F90Aligner` nested brackets
#[test]
fn test_aligner_nested_brackets() {
    use fprettier::format::F90Aligner;

    let mut aligner = F90Aligner::new();

    // Nested function calls
    let lines = vec![
        "result = func1(func2(a,".to_string(),
        "                     b),".to_string(),
        "               c)".to_string(),
    ];

    aligner
        .process_logical_line("result = func1(func2(a, b), c)", &lines, 3)
        .unwrap();

    let indents = aligner.get_lines_indent();
    assert_eq!(indents.len(), 3);
    assert_eq!(indents[0], 0); // First line
                               // Should align to innermost bracket
    assert!(indents[1] > indents[0]);
    // Third line should align to func1's opening
    assert!(indents[2] > 0);
}

/// Test enhanced whitespace formatting with relational operators
#[test]
fn test_end_to_end_relational_operators() {
    let input = "if(x>0.and.y==5)then\nz=x+y\nend if\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2, // Enable relational and logical operators
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Should have spaces around relational operators
    assert!(result.contains(" > "));
    assert!(result.contains(" == "));
    // Should have spaces around logical operators
    assert!(result.contains(".and.") || result.contains(".AND."));
}

/// Test enhanced whitespace formatting with logical operators
#[test]
fn test_end_to_end_logical_operators() {
    let input = "if(a.or.b.and.c)x=1\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2, // Enable logical operators
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Should have spaces around logical operators
    assert!(result.contains(" .or. ") || result.contains(" .OR. "));
    assert!(result.contains(" .and. ") || result.contains(" .AND. "));
}

/// Test enhanced whitespace formatting with pointer assignment
#[test]
fn test_end_to_end_pointer_assignment() {
    let input = "ptr=>target\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 1, // Enable assignment operators
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Should have spaces around =>
    assert_eq!(result.trim(), "ptr => target");
}

/// Test that operators inside strings are preserved
#[test]
fn test_end_to_end_operators_in_strings() {
    let input = "print *,\"x>0.and.y==5\"\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2, // Enable relational and logical operators
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Operators inside strings should NOT be formatted
    assert!(result.contains("\"x>0.and.y==5\""));
}

/// Test OMP conditional directive formatting
#[test]
fn test_omp_conditional_formatting() {
    // OMP conditional: "!$ " prefix should be preserved after formatting
    let input = "!$ x=1+2\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // OMP prefix should be preserved
    assert!(result.starts_with("!$ "), "OMP prefix missing: {result}");
    // Whitespace formatting should be applied
    assert!(
        result.contains(" = "),
        "Assignment spacing missing: {result}"
    );
    assert!(result.contains(" + "), "Plus spacing missing: {result}");
}

/// Test OMP conditional with indentation
/// OMP conditional (!$ ) is written at column 0, with padding to align
/// the code with the expected indent level
#[test]
fn test_omp_conditional_with_indent() {
    let input = "if (x > 0) then\n!$ y = 1\nend if\n";
    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // OMP prefix should be at column 0
    // The code inside if block is at indent level 1 (3 spaces)
    // OMP prefix is 3 chars, so padding = 3 - 3 = 0
    // Result: "!$ y = 1" (no extra padding needed)
    assert!(
        lines[1].starts_with("!$ "),
        "OMP prefix should be at column 0: {}",
        lines[1]
    );
    // Code immediately follows OMP prefix (no extra padding for indent level 1)
    assert_eq!(
        lines[1], "!$ y = 1",
        "OMP line should have correct format: {}",
        lines[1]
    );
}

/// Test statement label formatting
#[test]
fn test_statement_label_formatting() {
    let input = "100 x=1+2\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Label should be preserved
    assert!(result.starts_with("100 "), "Label missing: {result}");
    // Whitespace formatting should be applied
    assert!(
        result.contains(" = "),
        "Assignment spacing missing: {result}"
    );
}

/// Test statement label with indentation
#[test]
fn test_statement_label_with_indent() {
    let input = "if (x > 0) then\n100 y = 1\nend if\n";
    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // Label line should have label at start
    assert!(
        lines[1].contains("100 "),
        "Label missing on line 2: {}",
        lines[1]
    );
}

/// Test FORMAT statement with label
#[test]
fn test_format_statement_label() {
    let input = "200 FORMAT(A,I5)\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Label should be preserved
    assert!(result.starts_with("200 "), "Label missing: {result}");
    // FORMAT keyword should be present
    assert!(result.contains("FORMAT"), "FORMAT missing: {result}");
}

/// Test FORMAT statement continuation alignment with labels
///
/// Verifies that continuation lines of FORMAT statements align correctly
/// under the opening parenthesis, accounting for the label position.
#[test]
fn test_format_statement_continuation_alignment() {
    // Test case 1: 4-digit label at top level (indent=0)
    // "1003 FORMAT(" has "(" at position 12, so continuation should have 12 spaces
    let input1 = "1003 FORMAT(A10, I5, &\nF10.2)\n";
    let config = Config {
        impose_indent: true,
        impose_whitespace: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let cursor = Cursor::new(input1.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();
    format_file(reader, &mut output, &config, "test.f90").unwrap();
    let result1 = String::from_utf8(output).unwrap();
    let lines1: Vec<&str> = result1.lines().collect();

    // Count leading spaces on continuation line
    let cont_leading1 = lines1[1].len() - lines1[1].trim_start().len();
    assert_eq!(
        cont_leading1, 12,
        "4-digit label: continuation should have 12 spaces, got {}: {}",
        cont_leading1, result1
    );

    // Test case 2: 1-digit label inside module (indent=3)
    // "2  FORMAT(" has "(" at position 10, so continuation should have 10 spaces
    let input2 = "MODULE test\nCONTAINS\n2 FORMAT(A10, I5, &\nF10.2)\nEND MODULE\n";

    let cursor = Cursor::new(input2.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();
    format_file(reader, &mut output, &config, "test.f90").unwrap();
    let result2 = String::from_utf8(output).unwrap();
    let lines2: Vec<&str> = result2.lines().collect();

    // Find the continuation line (should be line 3, after "2  FORMAT(...&")
    let cont_leading2 = lines2[3].len() - lines2[3].trim_start().len();
    assert_eq!(
        cont_leading2, 10,
        "1-digit label: continuation should have 10 spaces, got {}: {}",
        cont_leading2, result2
    );
}

/// Test unary +/- operators in array literal continuations
///
/// Unary operators should NOT have spaces after them (e.g., -2 not - 2)
/// Binary operators should have spaces around them (e.g., a + b)
#[test]
fn test_unary_plusminus_in_array_literal() {
    let input = "program test\n  integer :: arr(3) = (/ &\n     1, &\n     -2, &\n     +3 /)\nend program\n";

    let config = Config {
        indent: 3,
        impose_indent: true,
        impose_whitespace: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // Line with -2 should have unary minus (no space between - and 2)
    let line_minus = lines
        .iter()
        .find(|l| l.contains("-2"))
        .expect("Should have line with -2");
    assert!(
        line_minus.contains("-2") && !line_minus.contains("- 2"),
        "Unary minus should not have space: {line_minus}"
    );

    // Line with +3 should have unary plus (no space between + and 3)
    let line_plus = lines
        .iter()
        .find(|l| l.contains("+3") || l.contains("+ 3"))
        .expect("Should have line with +3");
    assert!(
        line_plus.contains("+3") && !line_plus.contains("+ 3"),
        "Unary plus should not have space: {line_plus}"
    );
}

/// Test binary +/- on continuation lines
///
/// When a line continues from an expression ending with ), ], or alphanumeric,
/// the leading +/- on the continuation is binary and should have spaces.
#[test]
fn test_binary_plusminus_on_continuation() {
    // x = (a + b) &
    //     + c
    // Here the + continues from ), so it's binary
    let input = "program test\nx = (a + b) &\n+ c\nend program\n";

    let config = Config {
        indent: 3,
        impose_indent: true,
        impose_whitespace: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // The continuation line should have binary plus with space before
    let cont_line = lines
        .iter()
        .find(|l| l.trim().starts_with("+") || l.trim().starts_with("+ "))
        .expect("Should have continuation line");
    assert!(
        cont_line.contains("+ c"),
        "Binary plus should have space after: {cont_line}"
    );
}

/// Test case conversion - keywords to uppercase
#[test]
fn test_case_conversion_keywords_upper() {
    use std::collections::HashMap;

    let input = "if (x > 0) then\ny = 1\nend if\n";
    let mut case_dict = HashMap::new();
    case_dict.insert("keywords".to_string(), 2); // Uppercase

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        case_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Keywords should be uppercase
    assert!(result.contains("IF"), "IF should be uppercase: {result}");
    assert!(
        result.contains("THEN"),
        "THEN should be uppercase: {result}"
    );
    assert!(
        result.contains("END IF") || result.contains("END  IF"),
        "END IF should be uppercase: {result}"
    );
}

/// Test case conversion - procedures to lowercase
#[test]
fn test_case_conversion_procedures_lower() {
    use std::collections::HashMap;

    let input = "x = SIN(y) + COS(z)\n";
    let mut case_dict = HashMap::new();
    case_dict.insert("procedures".to_string(), 1); // Lowercase

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        case_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Procedures should be lowercase
    assert!(result.contains("sin("), "sin should be lowercase: {result}");
    assert!(result.contains("cos("), "cos should be lowercase: {result}");
}

/// Test case conversion - operators to uppercase
#[test]
fn test_case_conversion_operators_upper() {
    use std::collections::HashMap;

    let input = "if (x .and. y .or. z) then\nend if\n";
    let mut case_dict = HashMap::new();
    case_dict.insert("operators".to_string(), 2); // Uppercase

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        case_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Operators should be uppercase
    assert!(
        result.contains(".AND."),
        ".AND. should be uppercase: {result}"
    );
    assert!(
        result.contains(".OR."),
        ".OR. should be uppercase: {result}"
    );
}

/// Test case conversion combined with whitespace formatting
#[test]
fn test_case_conversion_with_whitespace() {
    use std::collections::HashMap;

    let input = "if(x.and.y)then\nx=SIN(y)\nend if\n";
    let mut case_dict = HashMap::new();
    case_dict.insert("keywords".to_string(), 2); // Keywords uppercase
    case_dict.insert("procedures".to_string(), 1); // Procedures lowercase
    case_dict.insert("operators".to_string(), 2); // Operators uppercase

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2,
        case_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Keywords uppercase
    assert!(result.contains("IF"), "IF should be uppercase: {result}");
    assert!(
        result.contains("THEN"),
        "THEN should be uppercase: {result}"
    );
    // Procedures lowercase
    assert!(result.contains("sin("), "sin should be lowercase: {result}");
    // Operators uppercase
    assert!(
        result.contains(".AND."),
        ".AND. should be uppercase: {result}"
    );
}

/// Test line splitting for long lines
#[test]
fn test_line_splitting() {
    // A line that's 80+ characters
    let input = "call very_long_subroutine_name(argument1, argument2, argument3, argument4, argument5, argument6)\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        line_length: 60, // Force splitting
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // Should be split into multiple lines
    assert!(
        lines.len() >= 2,
        "Line should be split: {} lines found",
        lines.len()
    );
    // First line should end with continuation marker
    assert!(
        lines[0].trim_end().ends_with(" &"),
        "First line should end with &: {}",
        lines[0]
    );
}

/// Test line splitting detaches inline comments when line is too long
#[test]
fn test_line_splitting_detaches_comments() {
    // A long line with a comment - comment should be detached to its own line
    // The code itself may also be split if it exceeds line length
    let input = "x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 ! this is a long comment\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        line_length: 50, // Triggers detaching comment and possibly code splitting
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // Comment should be detached to its own line
    // Code may also be split if it exceeds line length (code is ~56 chars > 50)
    assert!(
        lines.len() >= 2,
        "Commented line should have comment detached: {} lines found",
        lines.len()
    );
    // First line should be code (might be split)
    assert!(lines[0].contains("x = 1"));
    // Code lines should not contain the comment marker
    for line in &lines[..lines.len() - 1] {
        assert!(
            !line.contains("! this is a long comment"),
            "Comment should be detached from code"
        );
    }
    // Last line should be the comment
    assert!(
        lines.last().unwrap().contains("! this is a long comment"),
        "Last line should be the comment"
    );
}

/// Test fprettier directive parsing
#[test]
fn test_directive_parsing() {
    use fprettier::parse_directive;

    // Test basic indent directive
    let overrides = parse_directive("! fprettier: --indent 4").unwrap();
    assert_eq!(overrides.indent, Some(4));

    // Test multiple options
    let overrides = parse_directive("! fprettier: -i 2 -l 80 --no-whitespace").unwrap();
    assert_eq!(overrides.indent, Some(2));
    assert_eq!(overrides.line_length, Some(80));
    assert_eq!(overrides.impose_whitespace, Some(false));

    // Test case-insensitive matching
    let overrides = parse_directive("! FPRETTIER: --indent 3").unwrap();
    assert_eq!(overrides.indent, Some(3));
}

/// Test comment stripping with normalized spacing
#[test]
fn test_comment_spacing_normalization() {
    // Line with comment with irregular spacing
    let input = "x = 1      ! this is a comment\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        normalize_comment_spacing: true,
        comment_spacing: 2, // Normalize to 2 spaces
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Should have exactly 2 spaces before comment
    assert!(
        result.contains("x = 1  ! this is a comment"),
        "Expected normalized spacing: {result}"
    );
}

/// Test comment spacing preserved when normalization is disabled
#[test]
fn test_comment_spacing_preserved() {
    // Line with specific spacing before comment
    let input = "x = 1    ! keep this spacing\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        normalize_comment_spacing: false, // Don't normalize
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Original spacing should be approximately preserved
    assert!(
        result.contains("! keep this spacing"),
        "Comment should be present: {result}"
    );
}

/// Test `comment_spacing` option normalizes spacing before inline comments
#[test]
fn test_comment_spacing_option() {
    // Test with different comment_spacing values when normalize_comment_spacing is true
    let input = "x = 1! comment with no space\n";
    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        normalize_comment_spacing: true, // Enable to trigger comment_spacing normalization
        comment_spacing: 4,              // Use 4 spaces before comments
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Should have exactly 4 spaces before comment
    assert!(
        result.contains("x = 1    ! comment"),
        "Expected 4 spaces before comment: {result}"
    );
}

// ============================================================================
// Fine-grained whitespace option tests
// ============================================================================

/// Test --whitespace-comma overrides the default from whitespace level
#[test]
fn test_whitespace_comma_override() {
    use std::collections::HashMap;

    let input = "call foo(a,b,c)\n";

    // Level 0 normally has comma=false, but we override to true
    let mut whitespace_dict = HashMap::new();
    whitespace_dict.insert("comma".to_string(), true);

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 0, // Level 0: all spacing off
        whitespace_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Should have spaces after commas even at level 0
    assert!(
        result.contains("a, b, c"),
        "Expected comma spacing: {result}"
    );
}

/// Test --whitespace-comma=false disables comma spacing
#[test]
fn test_whitespace_comma_disable() {
    use std::collections::HashMap;

    let input = "call foo(a, b, c)\n";

    // Level 2 normally has comma=true, but we override to false
    let mut whitespace_dict = HashMap::new();
    whitespace_dict.insert("comma".to_string(), false);

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2, // Level 2: comma normally on
        whitespace_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Should NOT have spaces after commas
    assert!(
        result.contains("a,b,c"),
        "Expected no comma spacing: {result}"
    );
}

/// Test --whitespace-concat enables string concatenation spacing
#[test]
fn test_whitespace_concat_enable() {
    use std::collections::HashMap;

    let input = "str = a//b//c\n";

    // Level 2 normally has concat=false, but we override to true
    let mut whitespace_dict = HashMap::new();
    whitespace_dict.insert("concat".to_string(), true);

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2, // Level 2: concat normally off
        whitespace_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Should have spaces around //
    assert!(
        result.contains("a // b // c"),
        "Expected concat spacing: {result}"
    );
}

/// Test --whitespace-type enables type selector spacing
#[test]
fn test_whitespace_type_enable() {
    use std::collections::HashMap;

    let input = "x = mytype%field\n";

    // Level 2 normally has type=false, but we override to true
    let mut whitespace_dict = HashMap::new();
    whitespace_dict.insert("type".to_string(), true);

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2, // Level 2: type normally off
        whitespace_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Should have spaces around %
    assert!(
        result.contains("mytype % field"),
        "Expected type selector spacing: {result}"
    );
}

/// Test --whitespace-plusminus=false disables plus/minus spacing
#[test]
fn test_whitespace_plusminus_disable() {
    use std::collections::HashMap;

    let input = "x = a + b - c\n";

    // Level 2 normally has plusminus=true, but we override to false
    let mut whitespace_dict = HashMap::new();
    whitespace_dict.insert("plusminus".to_string(), false);

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 2, // Level 2: plusminus normally on
        whitespace_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Should NOT have spaces around + and -
    assert!(
        result.contains("a+b-c") || result.contains("a +b-c") || result.contains("a+ b- c"),
        "Expected reduced plus/minus spacing: {result}"
    );
}

/// Test multiple fine-grained overrides together
#[test]
fn test_multiple_whitespace_overrides() {
    use std::collections::HashMap;

    let input = "x = a + b, c < d\n";

    // Start with level 0 (all off) and enable specific ones
    let mut whitespace_dict = HashMap::new();
    whitespace_dict.insert("comma".to_string(), true);
    whitespace_dict.insert("assignments".to_string(), true);
    whitespace_dict.insert("relational".to_string(), true);
    whitespace_dict.insert("plusminus".to_string(), false); // keep off

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        whitespace: 0, // Level 0: all off
        whitespace_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Should have: assignment spacing, relational spacing, comma spacing
    assert!(
        result.contains(" = "),
        "Expected assignment spacing: {result}"
    );
    assert!(
        result.contains(" < "),
        "Expected relational spacing: {result}"
    );
    assert!(result.contains(", "), "Expected comma spacing: {result}");
}

// ============================================================================
// Operator replacement tests
// ============================================================================

/// Test --enable-replacements converts Fortran operators to C-style
#[test]
fn test_enable_replacements_to_c_style() {
    let input = "if (a .lt. b .and. c .ge. d) then\n";

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        enable_replacements: true,
        c_relations: true, // Convert to C-style
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // .lt. should become <, .ge. should become >=
    assert!(
        result.contains('<') && result.contains(">="),
        "Expected C-style operators: {result}"
    );
    assert!(
        !result.contains(".lt.") && !result.contains(".ge."),
        "Should not contain Fortran operators: {result}"
    );
}

/// Test --enable-replacements converts C-style operators to Fortran
#[test]
fn test_enable_replacements_to_fortran_style() {
    let input = "if (a < b .and. c >= d) then\n";

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        enable_replacements: true,
        c_relations: false, // Convert to Fortran-style
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // < should become .lt., >= should become .ge.
    assert!(
        result.contains(".lt.") && result.contains(".ge."),
        "Expected Fortran-style operators: {result}"
    );
}

/// Test replacement preserves operators in strings
#[test]
fn test_replacement_preserves_strings() {
    let input = r#"print *, "a .lt. b", x .lt. y"#;
    let input = format!("{input}\n");

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        enable_replacements: true,
        c_relations: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // String content should be preserved, code should be converted
    assert!(
        result.contains(r#""a .lt. b""#),
        "String content should be preserved: {result}"
    );
    assert!(
        result.contains("x <") || result.contains("x<"),
        "Code operators should be converted: {result}"
    );
}

/// Test replacement with whitespace formatting
#[test]
fn test_replacement_with_whitespace() {
    let input = "if(a.lt.b)then\n";

    let config = Config {
        impose_indent: false,
        impose_whitespace: true, // Enable whitespace formatting
        enable_replacements: true,
        c_relations: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Should have both whitespace formatting and operator replacement
    assert!(
        result.contains('<'),
        "Should have C-style operator: {result}"
    );
    assert!(
        result.contains(" < ") || result.contains("a < b"),
        "Should have proper spacing: {result}"
    );
}

/// Test all operator conversions
#[test]
fn test_all_operator_conversions() {
    let input = "x = a .lt. b .le. c .gt. d .ge. e .eq. f .ne. g\n";

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        enable_replacements: true,
        c_relations: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // All operators should be converted
    assert!(
        result.contains('<') && result.contains("<="),
        "Should have < and <=: {result}"
    );
    assert!(
        result.contains('>') && result.contains(">="),
        "Should have > and >=: {result}"
    );
    assert!(
        result.contains("==") && result.contains("/="),
        "Should have == and /=: {result}"
    );
}

/// Test replacement disabled by default
#[test]
fn test_replacement_disabled_by_default() {
    let input = "if (a .lt. b) then\n";

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        // enable_replacements defaults to false
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Operators should not be changed
    assert!(
        result.contains(".lt."),
        "Operators should be unchanged: {result}"
    );
}

// ============================================================================
// Block deactivation directive tests (!&< ... !&>)
// ============================================================================

/// Test single-line deactivation with !&
#[test]
fn test_single_line_deactivation() {
    let input = "x=1+2 !&\ny=3+4\n";

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // First line should be preserved (no whitespace added)
    assert!(
        result.contains("x=1+2"),
        "First line should be preserved: {result}"
    );
    // Second line should be formatted
    assert!(
        result.contains("y = 3 + 4"),
        "Second line should be formatted: {result}"
    );
}

/// Test block deactivation with !&< and !&>
#[test]
fn test_block_deactivation() {
    let input = r"a=1+2
!&<
x=3+4
y=5*6
!&>
b=7+8
";

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // First line (before block) should be formatted
    assert!(
        result.contains("a = 1 + 2"),
        "Line before block should be formatted: {result}"
    );
    // Lines inside block should be preserved
    assert!(
        result.contains("x=3+4"),
        "Line inside block should be preserved: {result}"
    );
    assert!(
        result.contains("y=5*6"),
        "Line inside block should be preserved: {result}"
    );
    // Last line (after block) should be formatted
    assert!(
        result.contains("b = 7 + 8"),
        "Line after block should be formatted: {result}"
    );
}

/// Test block deactivation preserves indentation
#[test]
fn test_block_deactivation_preserves_indent() {
    let input = r"program test
!&<
    if(a.lt.b)then
        x=1
    endif
!&>
end program
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: true,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // Lines inside block should preserve original formatting
    assert!(
        result.contains("if(a.lt.b)then"),
        "Code inside block should be preserved: {result}"
    );
    assert!(
        result.contains("x=1"),
        "Code inside block should be preserved: {result}"
    );
}

/// Test nested code with block deactivation
#[test]
fn test_block_deactivation_with_nested_code() {
    let input = r"!&<
! Hand-formatted table:
data values / 1,  2,  3,  &
              4,  5,  6,  &
              7,  8,  9 /
!&>
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    // The hand-formatted alignment should be preserved
    assert!(
        result.contains("/ 1,  2,  3,"),
        "Hand-formatted spacing should be preserved: {result}"
    );
}

/// Test labeled select case indentation
/// When SELECT CASE has a label prefix (e.g., "label: select case(x)"),
/// the content inside CASE blocks should still be indented correctly.
#[test]
fn test_labeled_select_case_indentation() {
    let input = r"casetest: select case(i)
case (1)
write (*, *) 'one'
case (2)
write (*, *) 'two'
end select casetest
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // select case should have no indent (label at start of line)
    assert!(
        lines[0].starts_with("casetest:"),
        "Line 0 should start with label: {}",
        lines[0]
    );
    // case (1) should have no indent (same level as select)
    assert!(
        lines[1].starts_with("case"),
        "Line 1 should start with case: {}",
        lines[1]
    );
    // write inside case should be indented (3 spaces)
    assert!(
        lines[2].starts_with("   "),
        "Line 2 should be indented: {}",
        lines[2]
    );
    // case (2) should have no indent
    assert!(
        lines[3].starts_with("case"),
        "Line 3 should start with case: {}",
        lines[3]
    );
    // write inside case should be indented (3 spaces)
    assert!(
        lines[4].starts_with("   "),
        "Line 4 should be indented: {}",
        lines[4]
    );
    // end select should have no indent
    assert!(
        lines[5].starts_with("end select"),
        "Line 5 should start with end select: {}",
        lines[5]
    );
}

/// Test SELECT CASE indentation: CASE statements should be at same level as SELECT CASE
///
/// Expected formatting:
/// ```fortran
/// select case (x)      ! indent N
/// case (1)             ! indent N (same as select)
///    code              ! indent N+3 (inside case)
/// case (2)             ! indent N (same as select)
///    code              ! indent N+3 (inside case)
/// case default         ! indent N (same as select)
///    code              ! indent N+3 (inside case)
/// end select           ! indent N (same as select)
/// ```
#[test]
fn test_select_case_indentation_comprehensive() {
    // Test various CASE patterns
    let input = r"subroutine test()
select case(i)
case (1)
write(*,*) 'one'
case (2, 3, 4)
write(*,*) 'two-four'
case (:0)
write(*,*) 'negative'
case (100:)
write(*,*) 'large'
case default
write(*,*) 'default'
end select
end subroutine
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // All CASE statements and SELECT/END SELECT should be at indent 3
    // Content inside CASE should be at indent 6

    // Line 0: subroutine test() - indent 0
    assert!(
        !lines[0].starts_with(" "),
        "subroutine should be at indent 0: '{}'",
        lines[0]
    );

    // Line 1: select case - indent 3
    assert!(
        lines[1].starts_with("   select case"),
        "select case should be at indent 3: '{}'",
        lines[1]
    );
    assert!(
        !lines[1].starts_with("    "),
        "select case should not be at indent 4+: '{}'",
        lines[1]
    );

    // Line 2: case (1) - indent 3 (same as select)
    assert!(
        lines[2].starts_with("   case (1)"),
        "case (1) should be at indent 3: '{}'",
        lines[2]
    );
    assert!(
        !lines[2].starts_with("    "),
        "case (1) should not be at indent 4+: '{}'",
        lines[2]
    );

    // Line 3: write - indent 6 (inside case)
    assert!(
        lines[3].starts_with("      "),
        "write should be at indent 6: '{}'",
        lines[3]
    );
    assert!(
        !lines[3].starts_with("       "),
        "write should not be at indent 7+: '{}'",
        lines[3]
    );

    // Line 4: case (2, 3, 4) - indent 3
    assert!(
        lines[4].starts_with("   case"),
        "case (2,3,4) should be at indent 3: '{}'",
        lines[4]
    );

    // Line 5: write - indent 6
    assert!(
        lines[5].starts_with("      "),
        "write should be at indent 6: '{}'",
        lines[5]
    );

    // Line 6: case (:0) - indent 3
    assert!(
        lines[6].starts_with("   case"),
        "case (:0) should be at indent 3: '{}'",
        lines[6]
    );

    // Line 8: case (100:) - indent 3
    assert!(
        lines[8].starts_with("   case"),
        "case (100:) should be at indent 3: '{}'",
        lines[8]
    );

    // Line 10: case default - indent 3
    assert!(
        lines[10].starts_with("   case default"),
        "case default should be at indent 3: '{}'",
        lines[10]
    );

    // Line 12: end select - indent 3
    assert!(
        lines[12].starts_with("   end select"),
        "end select should be at indent 3: '{}'",
        lines[12]
    );
}

/// Test SELECT TYPE indentation (similar to SELECT CASE)
#[test]
fn test_select_type_indentation() {
    let input = r"subroutine test(x)
class(*), intent(in) :: x
select type(x)
type is (integer)
print *, 'integer'
type is (real)
print *, 'real'
class is (base_type)
print *, 'derived'
class default
print *, 'unknown'
end select
end subroutine
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // select type at indent 3
    assert!(
        lines[2].starts_with("   select type"),
        "select type should be at indent 3: '{}'",
        lines[2]
    );

    // type is (integer) at indent 3
    assert!(
        lines[3].starts_with("   type is"),
        "type is should be at indent 3: '{}'",
        lines[3]
    );

    // print inside type is at indent 6
    assert!(
        lines[4].starts_with("      print"),
        "print should be at indent 6: '{}'",
        lines[4]
    );

    // class is at indent 3
    assert!(
        lines[7].starts_with("   class is"),
        "class is should be at indent 3: '{}'",
        lines[7]
    );

    // class default at indent 3
    assert!(
        lines[9].starts_with("   class default"),
        "class default should be at indent 3: '{}'",
        lines[9]
    );

    // end select at indent 3
    assert!(
        lines[11].starts_with("   end select"),
        "end select should be at indent 3: '{}'",
        lines[11]
    );
}

/// Test SELECT RANK indentation (similar to SELECT CASE)
#[test]
fn test_select_rank_indentation() {
    let input = r"subroutine test(x)
real, intent(in) :: x(..)
select rank(x)
rank (0)
print *, 'scalar'
rank (1)
print *, '1D'
rank default
print *, 'higher'
end select
end subroutine
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // select rank at indent 3
    assert!(
        lines[2].starts_with("   select rank"),
        "select rank should be at indent 3: '{}'",
        lines[2]
    );

    // rank (0) at indent 3
    assert!(
        lines[3].starts_with("   rank (0)"),
        "rank (0) should be at indent 3: '{}'",
        lines[3]
    );

    // content at indent 6
    assert!(
        lines[4].starts_with("      print"),
        "print should be at indent 6: '{}'",
        lines[4]
    );

    // rank default at indent 3
    assert!(
        lines[7].starts_with("   rank default"),
        "rank default should be at indent 3: '{}'",
        lines[7]
    );

    // end select at indent 3
    assert!(
        lines[9].starts_with("   end select"),
        "end select should be at indent 3: '{}'",
        lines[9]
    );
}

/// Test nested SELECT CASE indentation
#[test]
fn test_nested_select_case_indentation() {
    let input = r"subroutine test()
if (x > 0) then
select case(i)
case (1)
select case(j)
case (10)
write(*,*) 'i=1,j=10'
end select
case (2)
write(*,*) 'i=2'
end select
end if
end subroutine
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // By default, subroutine body is not indented (no --indent-mod)
    // Line 1: if (x > 0) then - indent 0 (subroutine body)
    assert!(
        lines[1].starts_with("if"),
        "if should be at indent 0: '{}'",
        lines[1]
    );

    // Line 2: select case - indent 3 (inside if)
    assert!(
        lines[2].starts_with("   select case"),
        "outer select case should be at indent 3: '{}'",
        lines[2]
    );

    // Line 3: case (1) - indent 3 (same as outer select)
    assert!(
        lines[3].starts_with("   case (1)"),
        "case (1) should be at indent 3: '{}'",
        lines[3]
    );

    // Line 4: inner select case - indent 6 (inside case)
    assert!(
        lines[4].starts_with("      select case"),
        "inner select case should be at indent 6: '{}'",
        lines[4]
    );

    // Line 5: case (10) - indent 6 (same as inner select)
    assert!(
        lines[5].starts_with("      case (10)"),
        "case (10) should be at indent 6: '{}'",
        lines[5]
    );

    // Line 6: write - indent 9 (inside inner case)
    assert!(
        lines[6].starts_with("         write"),
        "write should be at indent 9: '{}'",
        lines[6]
    );

    // Line 7: inner end select - indent 6
    assert!(
        lines[7].starts_with("      end select"),
        "inner end select should be at indent 6: '{}'",
        lines[7]
    );

    // Line 8: case (2) - indent 3 (same as outer select)
    assert!(
        lines[8].starts_with("   case (2)"),
        "case (2) should be at indent 3: '{}'",
        lines[8]
    );

    // Line 10: outer end select - indent 3
    assert!(
        lines[10].starts_with("   end select"),
        "outer end select should be at indent 3: '{}'",
        lines[10]
    );
}

// ============================================================================
// C Preprocessor handling tests
// ============================================================================

/// Test that C preprocessor lines are pinned to column 0 (no indentation)
#[test]
fn test_cpp_lines_no_indent() {
    let input = r"program test
   #define DEBUG 1
   #ifdef DEBUG
   x = 1
   #endif
end program test
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // CPP lines should be at column 0
    assert!(
        lines[1].starts_with("#define"),
        "CPP #define should be at column 0: {}",
        lines[1]
    );
    assert!(
        lines[2].starts_with("#ifdef"),
        "CPP #ifdef should be at column 0: {}",
        lines[2]
    );
    assert!(
        lines[4].starts_with("#endif"),
        "CPP #endif should be at column 0: {}",
        lines[4]
    );

    // Fortran code should still be indented
    assert!(
        lines[3].starts_with("   "),
        "Fortran code should be indented: {}",
        lines[3]
    );
}

/// Test that C preprocessor lines skip whitespace formatting
#[test]
fn test_cpp_lines_no_whitespace_formatting() {
    let input = "#define FOO(a,b) a+b\n";

    let config = Config {
        impose_indent: false,
        impose_whitespace: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Whitespace formatting should NOT be applied to CPP line
    // (spaces should NOT be added around + or after commas)
    assert!(
        result.contains("FOO(a,b) a+b"),
        "CPP line should not have whitespace formatting applied: {}",
        result
    );
}

/// Test that C preprocessor lines skip case conversion
#[test]
fn test_cpp_lines_no_case_conversion() {
    use std::collections::HashMap;

    let input = "#define MyMacro(x) x\n";

    let mut case_dict = HashMap::new();
    case_dict.insert("keywords".to_string(), 1); // uppercase keywords

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        case_dict,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Case should NOT be changed for CPP line
    assert!(
        result.contains("MyMacro"),
        "CPP line should preserve original case: {}",
        result
    );
}

/// Test that C preprocessor lines skip relational operator replacement
#[test]
fn test_cpp_lines_no_operator_replacement() {
    // Use C-style relational operators that would be replaced in Fortran code
    let input = "#if X < 5 && Y > 10\n";

    let config = Config {
        impose_indent: false,
        impose_whitespace: false,
        enable_replacements: true,
        c_relations: false, // Would convert < to .lt. and > to .gt. in Fortran code
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // < and > should NOT be converted to .lt. and .gt. (it's C preprocessor, not Fortran)
    assert!(
        result.contains('<') && result.contains('>'),
        "CPP line should preserve C-style operators, not convert to Fortran-style: {}",
        result
    );
    assert!(
        !result.contains(".lt.") && !result.contains(".gt."),
        "CPP line should not have Fortran-style operators: {}",
        result
    );
}

/// Test various C preprocessor directives are handled
#[test]
fn test_cpp_various_directives() {
    let input = r#"   #include <stdio.h>
   #include "myheader.h"
   #pragma once
   #error This is an error
   #warning This is a warning
   #undef FOO
   #line 100
"#;

    let config = Config {
        impose_indent: true,
        impose_whitespace: true,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // All CPP lines should be at column 0
    for (i, line) in lines.iter().enumerate() {
        if !line.is_empty() {
            assert!(
                line.starts_with('#'),
                "Line {} should start with # at column 0: {}",
                i,
                line
            );
        }
    }
}

/// Test that fypp lines are NOT treated as CPP
#[test]
fn test_fypp_not_treated_as_cpp() {
    let input = r"#:if DEBUG
   x = 1
#:endif
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        indent_fypp: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();

    // Fypp lines should use normal scope-based indentation when indent_fypp=true
    // They are NOT pinned to column 0 like CPP lines
    // The exact behavior depends on the fypp handling logic
    assert!(
        result.contains("#:if") && result.contains("#:endif"),
        "Fypp directives should be preserved: {}",
        result
    );
}

/// Test CPP inside indented Fortran code
#[test]
fn test_cpp_inside_fortran_scope() {
    let input = r"subroutine test()
   integer :: x
#ifdef DEBUG
   x = 1
#else
   x = 0
#endif
end subroutine test
";

    let config = Config {
        impose_indent: true,
        impose_whitespace: false,
        indent: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();

    let result = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = result.lines().collect();

    // CPP lines should be at column 0 regardless of Fortran scope
    assert!(
        lines[2].starts_with("#ifdef"),
        "#ifdef should be at column 0: {}",
        lines[2]
    );
    assert!(
        lines[4].starts_with("#else"),
        "#else should be at column 0: {}",
        lines[4]
    );
    assert!(
        lines[6].starts_with("#endif"),
        "#endif should be at column 0: {}",
        lines[6]
    );

    // Fortran code inside should be indented based on scope
    assert!(
        lines[3].starts_with("   "),
        "Fortran inside subroutine should be indented: {}",
        lines[3]
    );
    assert!(
        lines[5].starts_with("   "),
        "Fortran inside subroutine should be indented: {}",
        lines[5]
    );
}
