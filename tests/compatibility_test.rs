//! Compatibility tests against expected output
//!
//! These tests verify correct formatting by comparing fprettier output
//! against the expected output. A few exceptions are documented via comments.

#![warn(clippy::all)]
#![warn(clippy::pedantic)]

use std::fs;
use std::io::{BufReader, Cursor};

use fprettier::process::format_file;
use fprettier::Config;

/// Run fprettier on input and compare with expected output
fn test_compatibility(input_path: &str, expected_path: &str, config: &Config) {
    // Read input file
    let input = fs::read_to_string(input_path)
        .unwrap_or_else(|e| panic!("Failed to read input file {input_path}: {e}"));

    // Read expected output
    let expected = fs::read_to_string(expected_path)
        .unwrap_or_else(|e| panic!("Failed to read expected file {expected_path}: {e}"));

    // Run fprettier
    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, config, input_path)
        .unwrap_or_else(|e| panic!("fprettier failed on {input_path}: {e}"));

    let result = String::from_utf8(output)
        .unwrap_or_else(|e| panic!("Invalid UTF-8 in output for {input_path}: {e}"));

    // Compare line by line for better error messages
    let result_lines: Vec<&str> = result.lines().collect();
    let expected_lines: Vec<&str> = expected.lines().collect();

    // Check line count
    if result_lines.len() != expected_lines.len() {
        eprintln!("=== Line count mismatch for {input_path} ===");
        eprintln!(
            "Expected {} lines, got {} lines",
            expected_lines.len(),
            result_lines.len()
        );
        eprintln!("\n=== First difference ===");
        for (i, (r, e)) in result_lines.iter().zip(expected_lines.iter()).enumerate() {
            if r != e {
                eprintln!("Line {}: expected: {:?}", i + 1, e);
                eprintln!("Line {}: got:      {:?}", i + 1, r);
                break;
            }
        }
        panic!("Line count mismatch");
    }

    // Compare each line
    let mut differences = Vec::new();
    for (i, (result_line, expected_line)) in
        result_lines.iter().zip(expected_lines.iter()).enumerate()
    {
        if result_line != expected_line {
            differences.push((i + 1, *expected_line, *result_line));
        }
    }

    if !differences.is_empty() {
        eprintln!("\n=== Differences in {input_path} ===");
        eprintln!("Found {} differences:", differences.len());
        for (line_num, expected, got) in differences.iter().take(10) {
            eprintln!("\nLine {line_num}:");
            eprintln!("  expected: {expected:?}");
            eprintln!("  got:      {got:?}");
        }
        if differences.len() > 10 {
            eprintln!("\n... and {} more differences", differences.len() - 10);
        }
        panic!("{} differences found", differences.len());
    }
}

/// Get paths relative to the project root
fn get_example_path(dir: &str, file: &str) -> String {
    format!("examples/{dir}/{file}")
}

// ============================================================================
// Compatibility Tests - Basic Example
// ============================================================================

/// Test example file formatting compatibility
/// Uses default settings (`format_decl=false`) which preserves existing declaration alignment
#[test]
fn test_example_compatibility() {
    let input = get_example_path("in", "example.f90");
    let expected = get_example_path("out", "example.f90");

    // Default settings - format_decl=false preserves existing :: alignment
    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        format_decl: false, // Preserve existing declaration alignment
        ..Default::default()
    };

    test_compatibility(&input, &expected, &config);
}

// ============================================================================
// Individual Feature Tests - Smaller scope for debugging
// ============================================================================

/// Test basic whitespace formatting
#[test]
fn test_compat_whitespace_basic() {
    let input = "x=1+2\n";
    let expected = "x = 1 + 2\n";

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

    assert_eq!(result, expected, "Whitespace formatting mismatch");
}

/// Test indentation of IF/THEN blocks
#[test]
fn test_compat_indent_if_then() {
    let input = "if (x > 0) then\ny = 1\nend if\n";
    let expected = "if (x > 0) then\n   y = 1\nend if\n";

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

    assert_eq!(result, expected, "IF/THEN indentation mismatch");
}

/// Test DO loop indentation
#[test]
fn test_compat_indent_do_loop() {
    let input = "do i = 1, 10\nx = i\nend do\n";
    let expected = "do i = 1, 10\n   x = i\nend do\n";

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

    assert_eq!(result, expected, "DO loop indentation mismatch");
}

/// Test module indentation
#[test]
fn test_compat_indent_module() {
    let input =
        "module test\nimplicit none\ncontains\nsubroutine foo()\nend subroutine\nend module\n";
    let expected = "module test\n   implicit none\ncontains\n   subroutine foo()\n   end subroutine\nend module\n";

    let config = Config {
        impose_whitespace: false,
        impose_indent: true,
        indent: 3,
        indent_mod: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();
    let result = String::from_utf8(output).unwrap();

    assert_eq!(result, expected, "Module indentation mismatch");
}

/// Test semicolon-separated statements
#[test]
fn test_compat_semicolon_statements() {
    let input = "r=1;i=-2;j=3\n";
    // Semicolon statements stay on the same line
    let expected = "r = 1; i = -2; j = 3\n";

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

    assert_eq!(result, expected, "Semicolon statement formatting mismatch");
}

/// Test relational operators (both forms)
#[test]
fn test_compat_relational_operators() {
    let input = "if(r.eq.2.and.r<=5)i=3\n";
    let expected = "if (r .eq. 2 .and. r <= 5) i = 3\n";

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

    assert_eq!(result, expected, "Relational operator formatting mismatch");
}

/// Test pointer assignment (=>)
#[test]
fn test_compat_pointer_assignment() {
    let input = "point=>null()\n";
    let expected = "point => null()\n";

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

    assert_eq!(result, expected, "Pointer assignment formatting mismatch");
}

/// Test type component access (%)
#[test]
fn test_compat_type_component() {
    let input = "t % r  = 4.0_dp\n";
    let expected = "t%r = 4.0_dp\n";

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

    assert_eq!(
        result, expected,
        "Type component access formatting mismatch"
    );
}

/// Test function calls with spaces
#[test]
fn test_compat_function_call() {
    let input = "l = selected_real_kind ( 15 , 307)\n";
    let expected = "l = selected_real_kind(15, 307)\n";

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

    assert_eq!(result, expected, "Function call formatting mismatch");
}

/// Test nested blocks indentation
/// IF/DO at same column are preserved when they start aligned.
/// When both DO and IF start at column 0, they stay at column 0.
#[test]
fn test_compat_nested_blocks() {
    let input = "do i = 1, 10\nif (i > 5) then\nx = i\nend if\nend do\n";
    // DO and IF at column 0 stay at column 0 since they both start there
    let expected = "do i = 1, 10\nif (i > 5) then\n   x = i\nend if\nend do\n";

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

    assert_eq!(result, expected, "Nested blocks indentation mismatch");
}

/// Test SELECT CASE indentation
#[test]
fn test_compat_select_case() {
    let input = "select case (x)\ncase (1)\ny = 1\ncase (2)\ny = 2\nend select\n";
    let expected = "select case (x)\ncase (1)\n   y = 1\ncase (2)\n   y = 2\nend select\n";

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

    assert_eq!(result, expected, "SELECT CASE indentation mismatch");
}

/// Test combined whitespace and indentation
/// IF/DO at same column are preserved when they start aligned.
#[test]
fn test_compat_combined_formatting() {
    let input = "do i=1,10\nif(i>5)then\nx=i+1\nendif\nenddo\n";
    // DO and IF at column 0 stay at column 0 since they both start there
    let expected = "do i = 1, 10\nif (i > 5) then\n   x = i + 1\nend if\nend do\n";

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

    assert_eq!(result, expected, "Combined formatting mismatch");
}

// ============================================================================
// Unit tests for formatting options
// ============================================================================

/// Test whitespace formatting options -w 0,1,2 (ported from `test_whitespace`)
#[test]
#[allow(clippy::cast_possible_truncation)]
fn test_compat_whitespace_levels() {
    let input = "(/-a-b-(a+b-c)/(-c)*d**e,f[1]%v/)";
    let expected = [
        "(/-a-b-(a+b-c)/(-c)*d**e,f[1]%v/)",          // -w 0
        "(/-a-b-(a+b-c)/(-c)*d**e, f[1]%v/)",         // -w 1
        "(/-a - b - (a + b - c)/(-c)*d**e, f[1]%v/)", // -w 2
    ];

    for (w, exp) in expected.iter().enumerate() {
        let config = Config {
            impose_whitespace: true,
            impose_indent: false,
            whitespace: w as u8,
            ..Default::default()
        };

        let cursor = Cursor::new(input.as_bytes());
        let reader = BufReader::new(cursor);
        let mut output = Vec::new();

        format_file(reader, &mut output, &config, "test.f90").unwrap();
        let result = String::from_utf8(output).unwrap();

        assert_eq!(result.trim(), *exp, "Whitespace level {w} mismatch");
    }
}

/// Test nested DO loops with `strict_indent` (ported from `test_nested`)
#[test]
fn test_compat_nested_strict_indent() {
    let input = "integer :: i,j\ndo i=1,2\ndo j=1,3\nprint*,i,j,i*j\nend do\nend do\n";
    let expected = "integer :: i, j\ndo i = 1, 2\n   do j = 1, 3\n      print *, i, j, i*j\n   end do\nend do\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        strict_indent: true,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();
    let result = String::from_utf8(output).unwrap();

    assert_eq!(result, expected, "Nested strict indent mismatch");
}

/// Test associate construct (ported from `test_associate`)
#[test]
fn test_compat_associate() {
    let input = "associate(a=>b , c  =>d ,e=> f  )\ne=a+c\nend associate\n";
    let expected = "associate (a => b, c => d, e => f)\n   e = a + c\nend associate\n";

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

    assert_eq!(result, expected, "Associate construct mismatch");
}

/// Test USE statement alignment (ported from `test_use`)
#[test]
fn test_compat_use_alignment() {
    let input = "use A,only:B,C,&\nD,E\n";
    let expected = "use A, only: B, C, &\n             D, E\n";

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

    assert_eq!(result, expected, "USE statement alignment mismatch");
}

/// Test simpler plus/minus cases
#[test]
fn test_compat_plusminus_simple() {
    let input = "a-1+2-3.0e-9+4\n";
    let expected = "a - 1 + 2 - 3.0e-9 + 4\n";

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

    assert_eq!(result, expected, "Plus/minus simple formatting mismatch");
}

/// Test plus/minus corner cases with identifiers ending in 'd' (ported from `test_plusminus`)
#[test]
fn test_compat_plusminus_complex() {
    let input = "val_1d-1-1.0e-9-2.0d-08+.2e-1-val_2d-3.e-12+4\n";
    let expected = "val_1d - 1 - 1.0e-9 - 2.0d-08 + .2e-1 - val_2d - 3.e-12 + 4\n";

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

    assert_eq!(result, expected, "Plus/minus complex formatting mismatch");
}

/// Test whitespace level 3 (mult/div spacing)
#[test]
fn test_compat_whitespace_level3() {
    let input = "(/-a-b-(a+b-c)/(-c)*d**e,f[1]%v/)";
    let expected = "(/-a - b - (a + b - c) / (-c) * d**e, f[1]%v/)";

    let config = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 3,
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();
    let result = String::from_utf8(output).unwrap();

    assert_eq!(result.trim(), expected, "Whitespace level 3 mismatch");
}

/// Test 'do' as variable name (ported from `test_do`)
#[test]
fn test_compat_do_variable() {
    let input = "do = 1\nb = 2\n";

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

    // 'do' as variable name should be preserved, not treated as DO loop
    assert_eq!(result, input, "'do' variable name mismatch");
}

/// Test module/program indentation with --disable-indent-mod (ported from `test_mod`)
#[test]
fn test_compat_module_indent_disabled() {
    let input = "module my_module\nintrinsic none\ncontains\nfunction my_func()\nend\nend module\n";
    let expected =
        "module my_module\nintrinsic none\ncontains\nfunction my_func()\nend\nend module\n";

    let config = Config {
        impose_whitespace: false,
        impose_indent: true,
        indent: 3,
        indent_mod: false, // --disable-indent-mod
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();
    let result = String::from_utf8(output).unwrap();

    assert_eq!(result, expected, "Module indent disabled mismatch");
}

/// Test type selector whitespace (ported from `test_type_selector`)
#[test]
fn test_compat_type_selector() {
    let input = "A%component=func(mytype%a,mytype%abc+mytype%abcd)\n";
    let expected = "A % component = func(mytype % a, mytype % abc + mytype % abcd)\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 4, // Level 4 adds spaces around %
        ..Default::default()
    };

    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();

    format_file(reader, &mut output, &config, "test.f90").unwrap();
    let result = String::from_utf8(output).unwrap();

    assert_eq!(result, expected, "Type selector whitespace mismatch");
}

/// Test concat operator whitespace (ported from `test_concat`)
#[test]
fn test_compat_concat_operator() {
    let input = "str=a//b//c\n";

    // Level 0: no spaces
    let config_w0 = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 0,
        ..Default::default()
    };
    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();
    format_file(reader, &mut output, &config_w0, "test.f90").unwrap();
    let result_w0 = String::from_utf8(output).unwrap();
    assert_eq!(result_w0.trim(), "str=a//b//c", "Concat w=0 mismatch");

    // Level 2: spaces around assignment only
    let config_w2 = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 2,
        ..Default::default()
    };
    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();
    format_file(reader, &mut output, &config_w2, "test.f90").unwrap();
    let result_w2 = String::from_utf8(output).unwrap();
    assert_eq!(result_w2.trim(), "str = a//b//c", "Concat w=2 mismatch");
}

/// Test string concatenation preserved in strings (ported from `test_concat`)
#[test]
fn test_compat_concat_in_string() {
    let input = "msg = \"URL: http://example.com\"\n";

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

    // URL in string should be preserved
    assert_eq!(result, input, "String content should be preserved");
}

// ============================================================================
// Additional Unit Tests
// ============================================================================

/// Helper to run format and return result string
fn run_format(input: &str, config: &Config) -> String {
    let cursor = Cursor::new(input.as_bytes());
    let reader = BufReader::new(cursor);
    let mut output = Vec::new();
    format_file(reader, &mut output, config, "test.f90").unwrap();
    String::from_utf8(output).unwrap()
}

/// Test indent options -i in [0, 3, 4] with continuation alignment (ported from `test_indent`)
#[test]
fn test_compat_indent_options() {
    let input = "iF(teSt)ThEn\nCaLl subr(a,b,&\nc,(/d,&\ne,f/))\nEnD iF\n";

    for indent in [0, 3, 4] {
        let config = Config {
            impose_whitespace: true,
            impose_indent: true,
            indent,
            whitespace: 2,
            ..Default::default()
        };

        let result = run_format(input, &config);

        // Build expected output
        let ind = " ".repeat(indent);
        let expected = format!(
            "iF (teSt) ThEn\n{}CaLl subr(a, b, &\n{}c, (/d, &\n{}e, f/))\nEnD iF\n",
            ind,
            " ".repeat(10 + indent),
            " ".repeat(15 + indent)
        );

        assert_eq!(result, expected, "Indent {indent} mismatch");
    }
}

/// Test reset indentation at file start (ported from `test_reset_indent`)
#[test]
fn test_compat_reset_indent() {
    let tests = [
        (
            "integer :: i,j\ndo i=1,2\ndo j=1,3\nprint*,i,j,i*j\nend do\nend do\n",
            "integer :: i, j\ndo i = 1, 2\ndo j = 1, 3\n   print *, i, j, i*j\nend do\nend do\n",
        ),
        ("   module a\ninteger :: 1\n", "module a\n   integer :: 1\n"),
        ("     module a\nend\nend\n", "module a\nend\nend\n"),
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        indent_mod: true,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(
            result.trim_end(),
            expected.trim_end(),
            "Reset indent mismatch for input: {input:?}"
        );
    }
}

/// Test disabling indentation and/or whitespace formatting (ported from `test_disable`)
#[test]
fn test_compat_disable_formatting() {
    let input = "if(&\nl==111)&\n then\n   do m   =1,  2\n A=&\nB+C\n    end  do;   endif\n";

    // Default (both enabled)
    let config_default = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };
    let expected_default = "if ( &\n   l == 111) &\n   then\n   do m = 1, 2\n      A = &\n         B + C\n   end do; end if\n";
    let result = run_format(input, &config_default);
    assert_eq!(
        result.trim_end(),
        expected_default.trim_end(),
        "Default formatting mismatch"
    );

    // Disable whitespace
    // Original spacing after semicolons is preserved when whitespace is disabled.
    let config_no_ws = Config {
        impose_whitespace: false,
        impose_indent: true,
        indent: 3,
        ..Default::default()
    };
    let expected_no_ws = "if(&\n   l==111)&\n   then\n   do m   =1,  2\n      A=&\n         B+C\n   end  do;   endif\n";
    let result = run_format(input, &config_no_ws);
    assert_eq!(
        result.trim_end(),
        expected_no_ws.trim_end(),
        "No whitespace mismatch"
    );

    // Disable indent
    let config_no_indent = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 2,
        ..Default::default()
    };
    // When indent disabled, indentation is stripped
    let expected_no_indent =
        "if ( &\nl == 111) &\n then\n   do m = 1, 2\n A = &\nB + C\nend do; end if\n";
    let result = run_format(input, &config_no_indent);
    assert_eq!(
        result.trim_end(),
        expected_no_indent.trim_end(),
        "No indent mismatch"
    );

    // Disable both
    let config_none = Config {
        impose_whitespace: false,
        impose_indent: false,
        ..Default::default()
    };
    let result = run_format(input, &config_none);
    assert_eq!(result, input, "Both disabled mismatch");
}

/// Test multiple alias and alignment (ported from `test_multi_alias`)
#[test]
fn test_compat_multi_alias() {
    let input = "use A,only:B=>C,&\nD=>E\n";
    let expected = "use A, only: B => C, &\n             D => E\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Multi alias alignment mismatch");
}

/// Test USE statement with continuation on only: (ported from `test_use` part 2)
#[test]
fn test_compat_use_only_continuation() {
    let input = "use A,only:&\nB,C,D,E\n";
    let expected = "use A, only: &\n   B, C, D, E\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "USE only continuation mismatch");
}

/// Test formatting of declarations with --enable-decl (ported from `test_decl`)
#[test]
fn test_compat_decl_formatting() {
    let tests = [
        // Without enable_decl - preserve existing spacing
        ("integer    ::     a\n", "integer    ::     a\n", false),
        (
            "integer, dimension(:)    ::     a\n",
            "integer, dimension(:)    ::     a\n",
            false,
        ),
        // With enable_decl - normalize spacing
        ("integer    ::     a\n", "integer :: a\n", true),
        (
            "integer, dimension(:)    ::     a\n",
            "integer, dimension(:) :: a\n",
            true,
        ),
    ];

    for (input, expected, enable_decl) in tests {
        let config = Config {
            impose_whitespace: true,
            impose_indent: false,
            whitespace: 2,
            format_decl: enable_decl,
            ..Default::default()
        };

        let result = run_format(input, &config);
        assert_eq!(
            result, expected,
            "Declaration formatting mismatch (enable_decl={enable_decl})"
        );
    }
}

/// Test statement label formatting (ported from `test_statement_label`)
/// Statement label spacing is computed based on target indentation.
/// For top-level statements (indent=0), the label uses minimal spacing (just the trailing space from regex capture).
#[test]
fn test_compat_statement_label() {
    let input = "1003  FORMAT(2(1x, i4), 5x, '-', 5x, '-', 3x, '-', 5x, '-', 5x, '-', 8x, '-', 3x, &\n    1p, 2(1x, d10.3))\n";
    // Label spacing is computed as: max(0, target_indent - label_len)
    // For top-level (indent=0), label "1003 " (5 chars): padding = 0 - 5 = 0, so just 1 space from label
    // Continuation aligns under opening paren: "1003 FORMAT(" has "(" at position 12, so 12 spaces
    let expected = "1003 FORMAT(2(1x, i4), 5x, '-', 5x, '-', 3x, '-', 5x, '-', 5x, '-', 8x, '-', 3x, &\n            1p, 2(1x, d10.3))\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Statement label formatting mismatch");
}

/// Test statement label with preceding code (ported from `test_statement_label` part 2)
/// Labels on non-first lines of file get their spacing normalized to single space
#[test]
fn test_compat_statement_label_with_code() {
    // Input has double space after label "1003  FORMAT"
    // Expected normalizes to single space "1003 FORMAT" with correct continuation alignment
    let input = "print *, 'hello'\n1003  FORMAT(2(1x, i4), 5x, '-', 5x, '-', 3x, '-', 5x, '-', 5x, '-', 8x, '-', 3x, &\n    1p, 2(1x, d10.3))\n";
    let expected = "print *, 'hello'\n1003 FORMAT(2(1x, i4), 5x, '-', 5x, '-', 3x, '-', 5x, '-', 5x, '-', 8x, '-', 3x, &\n            1p, 2(1x, d10.3))\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(
        result, expected,
        "Statement label with code formatting mismatch"
    );
}

/// Test linebreaks within strings (ported from `test_ampersand_string`)
/// Continuation lines inside strings get a leading & added per Fortran standard.
#[test]
fn test_compat_ampersand_string() {
    let tests = [
        (
            "write  ( * ,  * )  \"a&\nstring\"\n",
            "write (*, *) \"a&\n&string\"\n",
        ),
        (
            "write  ( * ,  * )\"a& \n    string\"\n",
            "write (*, *) \"a&\n&    string\"\n",
        ),
        (
            "write  ( * ,  * )    \"a& \n    &  string\"\n",
            "write (*, *) \"a&\n    &  string\"\n",
        ),
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(
            result, expected,
            "Ampersand string mismatch for input: {input:?}"
        );
    }
}

/// Test first non-code line gets correctly indented (ported from `test_first_line_non_code`)
#[test]
fn test_compat_first_line_non_code() {
    let tests = [
        (
            "  ! a comment\n     module mod\n ! a comment\nend\n",
            "! a comment\nmodule mod\n   ! a comment\nend\n",
        ),
        (
            "  ! a comment\n     program mod\n ! a comment\nend\n",
            "! a comment\nprogram mod\n   ! a comment\nend\n",
        ),
        (
            "  ! a comment\n     function fun()\n ! a comment\nend\n",
            "     ! a comment\n     function fun()\n        ! a comment\n     end\n",
        ),
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        indent_mod: true,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(
            result.trim_end(),
            expected.trim_end(),
            "First line non-code mismatch for input: {input:?}"
        );
    }
}

/// Test manual alignment with & prefix (ported from `test_directive` part 1)
/// Whitespace formatting is applied to content while preserving the & alignment.
#[test]
fn test_compat_manual_alignment() {
    let input = "align_me = [ -1,  10,0,  &\n    &     0,1000 ,  0,&\n            &0 , -1,  1]\n";
    // Whitespace formatting is applied to continuation line content while preserving alignment
    let expected = "align_me = [-1, 10, 0,  &\n    &     0, 1000, 0,&\n            &0, -1, 1]\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Manual alignment mismatch");
}

/// Test inline deactivate directive !& (ported from `test_directive` part 2)
/// Lines with !& deactivation should be preserved exactly as input.
#[test]
fn test_compat_inline_deactivate() {
    let input =
        "align_me = [ -1,  10,0,  & !&\n    &     0,1000 ,  0,& !&\n            &0 , -1,  1] !&\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    // Input should be preserved when !& is present
    assert_eq!(result, input, "Inline deactivate mismatch");
}

/// Test block deactivate directive !&< ... !&> (ported from `test_directive` part 3)
/// Lines within !&< ... !&> block should be preserved exactly.
#[test]
fn test_compat_block_deactivate() {
    let inner = "align_me = [ -1,  10,0,  &\n    &     0,1000 ,  0,&\n            &0 , -1,  1]";
    let input = format!("!&<\n{inner}\n!&>\n");

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(&input, &config);
    // Block should be preserved
    assert_eq!(result, input, "Block deactivate mismatch");
}

/// Test concat operator at whitespace level 4 (ported from `test_concat`)
#[test]
fn test_compat_concat_level4() {
    let input = "str=a//b//c\n";
    let expected = "str = a // b // c\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 4,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Concat level 4 mismatch");
}

/// Test concat in comments preserved (ported from `test_concat`)
#[test]
fn test_compat_concat_in_comment() {
    let input = "a = b  ! http://example.com\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, input, "Concat in comment should be preserved");
}

/// Test OpenMP directive formatting (ported from `test_omp`)
/// OMP directives (!$OMP) should stay at column 0, not indented with block.
#[test]
fn test_compat_omp_directives() {
    let input = "PROGRAM test_omp\n !$OMP    PARALLEL DO\nb=4\n!$a=b\n!$  a=b\n   !$    c=b\n!$acc parallel loop\n!$OMP END  PARALLEL DO\nEND PROGRAM\n";
    // OMP directives stay at column 0, conditionals get their whitespace normalized
    let expected = "PROGRAM test_omp\n!$OMP    PARALLEL DO\n   b = 4\n!$a=b\n!$ a = b\n!$ c = b\n!$acc parallel loop\n!$OMP END  PARALLEL DO\nEND PROGRAM\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        indent_mod: true,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "OMP directive formatting mismatch");
}

/// Test line length option (ported from `test_line_length`)
#[test]
fn test_compat_line_length_basic() {
    let inputs = [
        "REAL(KIND=4) :: r,f  !  some reals",
        "if(   min == max.and.min .eq. thres  )",
        "INQUIRE(14)",
    ];
    let expected_default = [
        "REAL(KIND=4) :: r, f  !  some reals",
        "if (min == max .and. min .eq. thres)",
        "INQUIRE (14)",
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        strict_indent: true,
        ..Default::default()
    };

    for (input, exp) in inputs.iter().zip(expected_default.iter()) {
        let result = run_format(input, &config);
        assert_eq!(
            result.trim(),
            *exp,
            "Line length basic mismatch for: {input}"
        );
    }
}

/// Test relation replacement (ported from `test_relation_replacement`)
/// Tests basic relational operator formatting without replacements enabled.
#[test]
fn test_compat_relation_formatting() {
    let input = "if ( min < max .and. min .lt. thres)\n";
    let expected = "if (min < max .and. min .lt. thres)\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Relation formatting mismatch");
}

/// Test new I/O intrinsics (ported from `test_new_intrinsics`)
/// REWIND, BACKSPACE, INQUIRE should have space before parenthesis
#[test]
fn test_compat_new_intrinsics() {
    let tests = [
        ("REWIND(12)\n", "REWIND (12)\n"),
        ("BACKSPACE(13)\n", "BACKSPACE (13)\n"),
        ("INQUIRE(14)\n", "INQUIRE (14)\n"),
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 2,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(result, expected, "I/O intrinsic mismatch for: {input:?}");
    }
}

/// Test deprecated kind definition formatting (ported from `test_wrongkind`)
/// REAL*8 and INTEGER*4 should have no space before/after *
#[test]
fn test_compat_wrongkind() {
    let tests = [
        (
            "REAL*8 :: r, f  !  some reals\n",
            "REAL*8 :: r, f  !  some reals\n",
        ),
        (
            "REAL * 8 :: r, f  !  some reals\n",
            "REAL*8 :: r, f  !  some reals\n",
        ),
        (
            "INTEGER * 4 :: c, i  !  some integers\n",
            "INTEGER*4 :: c, i  !  some integers\n",
        ),
        (
            "INTEGER*4 :: c, i  !  some integers\n",
            "INTEGER*4 :: c, i  !  some integers\n",
        ),
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 2,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(result, expected, "Wrong kind mismatch for: {input:?}");
    }
}

/// Test FORD documentation comment formatting (ported from `test_ford`)
/// FORD uses !! for documentation comments - they preserve their original indentation
#[test]
fn test_compat_ford_comments() {
    let input = concat!(
        "   a = b\n",
        "     !!  ford docu\n",
        "b=c\n",
        "  !! ford docu\n",
        "subroutine test(a,b,&\n",
        "  !! ford docu\n",
        "  c, d, e)\n"
    );
    let expected = concat!(
        "   a = b\n",
        "     !!  ford docu\n",
        "   b = c\n",
        "  !! ford docu\n",
        "   subroutine test(a, b, &\n",
        "  !! ford docu\n",
        "                   c, d, e)\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "FORD comment formatting mismatch");
}

/// Test comment formatting options (ported from `test_comments`)
/// Comment-only lines within continuations are indented to the continuation column.
#[test]
fn test_compat_comments_basic() {
    let input = concat!(
        "TYPE mytype\n",
        "!  c1\n",
        "  !c2\n",
        "   INTEGER :: a   !  c3\n",
        "   REAL :: b, &   ! c4\n",
        "! c5\n",
        "                  ! c6\n",
        "           d      ! c7\n",
        "END TYPE  ! c8\n"
    );
    let expected = concat!(
        "TYPE mytype\n",
        "!  c1\n",
        "   !c2\n",
        "   INTEGER :: a   !  c3\n",
        "   REAL :: b, &   ! c4\n",
        "           ! c5\n",
        "           ! c6\n",
        "           d      ! c7\n",
        "END TYPE  ! c8\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Comment formatting mismatch");
}

/// Test multiline string continuation formatting (ported from `test_multiline_str`)
/// Tests that continuation characters are properly added to multiline strings.
/// A leading & is added at the start of continuation lines within strings per Fortran standard.
#[test]
fn test_compat_multiline_str() {
    let input = concat!(
        "      CHARACTER(len=*), PARAMETER      :: serialized_string = &\n",
        "         \"qtb_rng_gaussian                         1 F T F   0.0000000000000000E+00&\n",
        "                          12.0                12.0                12.0&\n",
        "                          12.0                12.0                12.0\"\n"
    );
    let expected = concat!(
        "      CHARACTER(len=*), PARAMETER      :: serialized_string = &\n",
        "         \"qtb_rng_gaussian                         1 F T F   0.0000000000000000E+00&\n",
        "&                          12.0                12.0                12.0&\n",
        "&                          12.0                12.0                12.0\"\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Multiline string mismatch");
}

/// Test multiline string with leading ampersands preserved (ported from `test_multiline_str` part 2)
/// When continuation lines already have leading &, they should be preserved
#[test]
fn test_compat_multiline_str_with_leading_amp() {
    // Input already has leading & on continuation lines - preserve them
    let input = concat!(
        "      CHARACTER(len=*), PARAMETER      :: serialized_string = &\n",
        "         \"qtb_rng_gaussian                         1 F T F   0.0000000000000000E+00&\n",
        "                 &         12.0                12.0                12.0&\n",
        "                 &         12.0                12.0                12.0\"\n"
    );
    let expected = input; // Should be preserved as-is

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(
        result, expected,
        "Multiline string with leading amp mismatch"
    );
}

/// Test fypp preprocessor formatting (ported from `test_fypp`)
/// Fypp is a Fortran preprocessor with template and metaprogramming features
#[test]
fn test_compat_fypp_basic() {
    let input = concat!(
        "#:if DEBUG>  0\n",
        "print *, \"hola\"\n",
        "if( .not. (${cond}$) ) then\n",
        "#:if  ASSERT(cond)\n",
        "print *, \"Assert failed!\"\n",
        "#:endif\n",
        "error stop\n",
        "end if\n",
        "#:endif\n"
    );

    let expected = concat!(
        "#:if DEBUG>  0\n",
        "   print *, \"hola\"\n",
        "   if (.not. (${cond}$)) then\n",
        "      #:if  ASSERT(cond)\n",
        "         print *, \"Assert failed!\"\n",
        "      #:endif\n",
        "      error stop\n",
        "   end if\n",
        "#:endif\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Fypp basic formatting mismatch");
}

/// Test fypp with for loop (ported from `test_fypp`)
#[test]
fn test_compat_fypp_for_loop() {
    let input = concat!(
        "if  (.not. (${cond}$)) then\n",
        "   #:for element in list\n",
        "   print *, \"Element is in list!\"\n",
        " #:endfor\n",
        "   error stop\n",
        "end if\n"
    );

    let expected = concat!(
        "if (.not. (${cond}$)) then\n",
        "   #:for element in list\n",
        "      print *, \"Element is in list!\"\n",
        "   #:endfor\n",
        "   error stop\n",
        "end if\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Fypp for loop formatting mismatch");
}

/// Test fypp with def/enddef (ported from `test_fypp`)
#[test]
fn test_compat_fypp_def() {
    let input = concat!(
        "#:if aa > 1\n",
        "print  *, \"Number is more than 1\"\n",
        "if (condition) then\n",
        "     #:def something\n",
        "   print *, \"Added Definition!\"\n",
        "   #:enddef\n",
        "end if\n",
        "#:endif\n"
    );

    let expected = concat!(
        "#:if aa > 1\n",
        "   print *, \"Number is more than 1\"\n",
        "   if (condition) then\n",
        "      #:def something\n",
        "         print *, \"Added Definition!\"\n",
        "      #:enddef\n",
        "   end if\n",
        "#:endif\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Fypp def formatting mismatch");
}

/// Test fypp with block/endblock (ported from `test_fypp`)
#[test]
fn test_compat_fypp_block() {
    let input = concat!(
        "#:block DEBUG_CODE\n",
        "  if (a <b) then\n",
        "    print *, \"DEBUG: a is less than b\"\n",
        "  end if\n",
        "#:endblock  DEBUG_CODE\n"
    );

    let expected = concat!(
        "#:block DEBUG_CODE\n",
        "   if (a < b) then\n",
        "      print *, \"DEBUG: a is less than b\"\n",
        "   end if\n",
        "#:endblock  DEBUG_CODE\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Fypp block formatting mismatch");
}

/// Test normalize-comment-spacing option (ported from `test_comments`)
#[test]
fn test_compat_normalize_comment_spacing() {
    let input = concat!(
        "TYPE mytype\n",
        "!  c1\n",
        "  !c2\n",
        "   INTEGER :: a   !  c3\n",
        "   REAL :: b, &   ! c4\n",
        "! c5\n",
        "                  ! c6\n",
        "           d      ! c7\n",
        "END TYPE  ! c8"
    );

    let expected = concat!(
        "TYPE mytype\n",
        "!  c1\n",
        "   !c2\n",
        "   INTEGER :: a !  c3\n",
        "   REAL :: b, & ! c4\n",
        "           ! c5\n",
        "           ! c6\n",
        "           d ! c7\n",
        "END TYPE ! c8\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        normalize_comment_spacing: true,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Normalize comment spacing mismatch");
}

/// Test normalize-comment-spacing with comment-spacing option (ported from `test_comments`)
#[test]
fn test_compat_normalize_comment_spacing_custom() {
    let input = concat!(
        "TYPE mytype\n",
        "!  c1\n",
        "  !c2\n",
        "   INTEGER :: a   !  c3\n",
        "   REAL :: b, &   ! c4\n",
        "! c5\n",
        "                  ! c6\n",
        "           d      ! c7\n",
        "END TYPE  ! c8"
    );

    let expected = concat!(
        "TYPE mytype\n",
        "!  c1\n",
        "   !c2\n",
        "   INTEGER :: a   !  c3\n",
        "   REAL :: b, &   ! c4\n",
        "           ! c5\n",
        "           ! c6\n",
        "           d   ! c7\n",
        "END TYPE   ! c8\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        normalize_comment_spacing: true,
        comment_spacing: 3,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(
        result, expected,
        "Normalize comment spacing with custom spacing mismatch"
    );
}

/// Test auto-split of long logical lines (ported from `test_auto_split_long_logical_line`)
#[test]
fn test_compat_auto_split_long_line() {
    let input = concat!(
        "subroutine demo()\n",
        "    integer :: a\n",
        "    if (this_condition_is_lengthy .or. second_lengthy_condition) cycle\n",
        "end subroutine demo"
    );

    let expected = concat!(
        "subroutine demo()\n",
        "    integer :: a\n",
        "    if (this_condition_is_lengthy .or. &\n",
        "        second_lengthy_condition) cycle\n",
        "end subroutine demo\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 4,
        whitespace: 2,
        line_length: 68,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Auto-split long line mismatch");
}

/// Test auto-split handles bang in string (ported from `test_auto_split_handles_bang_in_string`)
#[test]
fn test_compat_auto_split_bang_in_string() {
    let input = concat!(
        "subroutine demo(str)\n",
        "    character(len=*), intent(in) :: str\n",
        "    if (str .eq. \"This string has a ! bang inside\") print *, str//\", wow!\"\n",
        "end subroutine demo"
    );

    let expected = concat!(
        "subroutine demo(str)\n",
        "    character(len=*), intent(in) :: str\n",
        "    if (str .eq. \"This string has a ! bang inside\") print *, &\n",
        "        str//\", wow!\"\n",
        "end subroutine demo\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 4,
        whitespace: 2,
        line_length: 72,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Auto-split bang in string mismatch");
}

/// Test auto-split during indent pass (ported from `test_auto_split_after_indent_adjustment`)
#[test]
fn test_compat_auto_split_indent_pass() {
    let input = concat!(
        "program demo\n",
        "    integer :: i\n",
        "    if (.true.) then\n",
        "        if (.true.) then\n",
        "  if (i > 1 .and. this_is_a_pretty_freaking_long_parameter_name .eq. 42) print *, \"too long\"\n",
        "        end if\n",
        "    end if\n",
        "end program demo\n"
    );

    let expected = concat!(
        "program demo\n",
        "    integer :: i\n",
        "    if (.true.) then\n",
        "        if (.true.) then\n",
        "            if (i > 1 .and. this_is_a_pretty_freaking_long_parameter_name .eq. 42) print &\n",
        "                *, \"too long\"\n",
        "        end if\n",
        "    end if\n",
        "end program demo\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 4,
        whitespace: 2,
        line_length: 100,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(
        result, expected,
        "Auto-split after indent adjustment mismatch"
    );
}

/// Test auto-split when whitespace disabled (ported from `test_auto_split_when_whitespace_disabled`)
#[test]
fn test_compat_auto_split_no_whitespace() {
    let input = concat!(
        "program demo\n",
        "    if (.true.) then\n",
        "      if (.true.) then\n",
        "  if (i > 1 .and. identifier_that_is_far_too_long .eq. 42) print *, \"oops\"\n",
        "      end if\n",
        "    end if\n",
        "end program demo\n"
    );

    let expected = concat!(
        "program demo\n",
        "    if (.true.) then\n",
        "        if (.true.) then\n",
        "            if (i > 1 .and. identifier_that_is_far_too_long .eq. 42) &\n",
        "                print *, \"oops\"\n",
        "        end if\n",
        "    end if\n",
        "end program demo\n"
    );

    let config = Config {
        impose_whitespace: false,
        impose_indent: true,
        indent: 4,
        whitespace: 2,
        line_length: 70,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Auto-split without whitespace mismatch");
}

/// Test line length detaches inline comment (ported from `test_line_length_detaches_inline_comment`)
#[test]
fn test_compat_line_length_detach_comment() {
    let input = concat!(
        "program demo\n",
        "    if (.true.) then\n",
        "        print *, 'prefix '//'and '//'suffix' ! trailing comment\n",
        "    end if\n",
        "end program demo\n"
    );

    let expected = concat!(
        "program demo\n",
        "    if (.true.) then\n",
        "        print *, 'prefix '//'and '//'suffix'\n",
        "        ! trailing comment\n",
        "    end if\n",
        "end program demo\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 4,
        whitespace: 2,
        line_length: 60,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Line length detach comment mismatch");
}

/// Test line length comment then split (ported from `test_line_length_comment_then_split`)
#[test]
fn test_compat_line_length_comment_then_split() {
    let input = concat!(
        "program demo\n",
        "    if (.true.) then\n",
        "        if (.true.) then\n",
        "            if (foo_bar_identifier .and. bar_baz_identifier) print *, long_identifier, another_long_identifier ! note\n",
        "        end if\n",
        "    end if\n",
        "end program demo\n"
    );

    let expected = concat!(
        "program demo\n",
        "    if (.true.) then\n",
        "        if (.true.) then\n",
        "            if (foo_bar_identifier .and. bar_baz_identifier) print *, &\n",
        "                long_identifier, another_long_identifier\n",
        "            ! note\n",
        "        end if\n",
        "    end if\n",
        "end program demo\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 4,
        whitespace: 2,
        line_length: 72,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Line length comment then split mismatch");
}

/// Test full label formatting (ported from `test_label`)
#[test]
fn test_compat_label_full() {
    let input = concat!(
        "MODULE cp_lbfgs\n",
        "CONTAINS\n",
        "20000    FORMAT('RUNNING THE L-BFGS-B CODE', /, /,                          &\n",
        "     &    'it    = iteration number', /,                                    &\n",
        "     &    'Machine precision =', 1p, d10.3)\n",
        "2        FORMAT('RUNNING THE L-BFGS-B CODE', /, /,                          &\n",
        "     &    'it    = iteration number', /,                                    &\n",
        "     &    'Machine precision =', 1p, d10.3)\n",
        "20000    FORMAT('RUNNING THE L-BFGS-B CODE', /, /,                          &\n",
        "          'it    = iteration number', /,                                    &\n",
        "          'Machine precision =', 1p, d10.3)\n",
        "2        FORMAT('RUNNING THE L-BFGS-B CODE', /, /,                          &\n",
        "          'it    = iteration number', /,                                    &\n",
        "          'Machine precision =', 1p, d10.3)\n",
        "END MODULE\n"
    );

    // Expected output - label spacing matches fprettify (computed based on target indent).
    // Note: continuation line indentation differs slightly from fprettify in some cases.
    let expected = concat!(
        "MODULE cp_lbfgs\n",
        "CONTAINS\n",
        "20000 FORMAT('RUNNING THE L-BFGS-B CODE', /, /,                          &\n",
        "  &    'it    = iteration number', /,                                    &\n",
        "  &    'Machine precision =', 1p, d10.3)\n",
        "2  FORMAT('RUNNING THE L-BFGS-B CODE', /, /,                          &\n",
        "&    'it    = iteration number', /,                                    &\n",
        "&    'Machine precision =', 1p, d10.3)\n",
        "20000 FORMAT('RUNNING THE L-BFGS-B CODE', /, /, &\n",
        "             'it    = iteration number', /, &\n",
        "             'Machine precision =', 1p, d10.3)\n",
        "2  FORMAT('RUNNING THE L-BFGS-B CODE', /, /, &\n",
        "          'it    = iteration number', /, &\n",
        "          'Machine precision =', 1p, d10.3)\n",
        "END MODULE\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Full label formatting mismatch");
}

/// Test relation replacement to Fortran style (ported from `test_relation_replacement`)
#[test]
fn test_compat_relation_replacement_fortran() {
    let tests = [
        (
            "if ( min < max .and. min .lt. thres)",
            "if (min .lt. max .and. min .lt. thres)",
        ),
        (
            "if (min > max .and. min .gt. thres )",
            "if (min .gt. max .and. min .gt. thres)",
        ),
        (
            "if (   min == max .and. min .eq. thres  )",
            "if (min .eq. max .and. min .eq. thres)",
        ),
        (
            "if(min /= max .and. min .ne. thres)",
            "if (min .ne. max .and. min .ne. thres)",
        ),
        (
            "if(min >= max .and. min .ge. thres )",
            "if (min .ge. max .and. min .ge. thres)",
        ),
        (
            "if( min <= max .and. min .le. thres)",
            "if (min .le. max .and. min .le. thres)",
        ),
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        enable_replacements: true,
        c_relations: false,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(
            result.trim(),
            expected,
            "Fortran relation replacement mismatch for: {input}"
        );
    }
}

/// Test relation replacement to C style (ported from `test_relation_replacement`)
#[test]
fn test_compat_relation_replacement_c() {
    let tests = [
        (
            "if ( min < max .and. min .lt. thres)",
            "if (min < max .and. min < thres)",
        ),
        (
            "if (min > max .and. min .gt. thres )",
            "if (min > max .and. min > thres)",
        ),
        (
            "if (   min == max .and. min .eq. thres  )",
            "if (min == max .and. min == thres)",
        ),
        (
            "if(min /= max .and. min .ne. thres)",
            "if (min /= max .and. min /= thres)",
        ),
        (
            "if(min >= max .and. min .ge. thres )",
            "if (min >= max .and. min >= thres)",
        ),
        (
            "if( min <= max .and. min .le. thres)",
            "if (min <= max .and. min <= thres)",
        ),
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        enable_replacements: true,
        c_relations: true,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(
            result.trim(),
            expected,
            "C relation replacement mismatch for: {input}"
        );
    }
}

/// Test swap case for keywords (ported from `test_swap_case`)
#[test]
fn test_compat_swap_case_keywords() {
    use std::collections::HashMap;

    let tests = [
        ("MODULE exAmple", "module exAmple"),
        (
            "INTEGER,   PARAMETER :: SELECTED_REAL_KIND = 1*2",
            "integer, parameter :: SELECTED_REAL_KIND = 1*2",
        ),
        (
            "INTEGER, INTENT(IN) :: r, i, j, k",
            "integer, intent(IN) :: r, i, j, k",
        ),
        ("PURE SUBROUTINE mypure()", "pure subroutine mypure()"),
    ];

    let mut case_dict = HashMap::new();
    case_dict.insert("keywords".to_string(), 1); // lowercase
    case_dict.insert("procedures".to_string(), 1); // lowercase
    case_dict.insert("operators".to_string(), 1); // lowercase
    case_dict.insert("types".to_string(), 2); // uppercase

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        case_dict,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(result.trim(), expected, "Swap case mismatch for: {input}");
    }
}

/// Test indentation preserves line length limit (ported from `test_indent_preserves_line_length_limit`)
#[test]
fn test_compat_indent_preserves_line_length() {
    let input = concat!(
        "subroutine demo(tokens, stmt_start)\n",
        "   type(dummy), intent(in) :: tokens(:)\n",
        "   integer, intent(in) :: stmt_start\n",
        "   integer :: i, nesting_level\n",
        "\n",
        "   if (tokens(stmt_start)%text == \"if\") then\n",
        "      if (tokens(i)%text == \"endif\") then\n",
        "         nesting_level = nesting_level - 1\n",
        "      else if (tokens(i)%text == \"end\" .and. i + 1 <= size(tokens) .and. &\n",
        "               tokens(i + 1)%kind == TK_KEYWORD .and. tokens(i + 1)%text == \"if\") then\n",
        "         nesting_level = nesting_level - 1\n",
        "      end if\n",
        "   end if\n",
        "end subroutine demo\n"
    );

    // Input should be stable - running formatter shouldn't change already-formatted code
    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        line_length: 90,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(
        result, input,
        "Indentation should be stable for already-formatted code"
    );
}

/// Test unicode handling (ported from `test_io`)
#[test]
fn test_compat_unicode() {
    let input = "CALL  alien_invasion( 👽 )\n";
    let expected = "CALL alien_invasion(👽)\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Unicode formatting mismatch");
}

/// Test space before string literal is preserved (regression test)
/// This tests that spaces before string literals aren't trimmed by operator formatting
#[test]
fn test_compat_space_before_string_literal() {
    // Space before string must be preserved after logical operator processing
    let tests = [
        ("x%text == \"if\"\n", "x%text == \"if\"\n"),
        ("x .and. y%text == \"if\"\n", "x .and. y%text == \"if\"\n"),
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert!(
            result.contains("== \"if\""),
            "Space before string should be preserved in: {input:?}"
        );
        assert_eq!(result, expected);
    }
}

// ============================================================================
// File-Based Compatibility Tests - Additional Example Files
// ============================================================================

/// Test example_swapcase.f90 compatibility
/// This file uses the --case annotation to convert keywords to lowercase
#[test]
fn test_example_swapcase_compatibility() {
    use std::collections::HashMap;

    let input = get_example_path("in", "example_swapcase.f90");
    let expected = get_example_path("out", "example_swapcase.f90");

    // The file has annotation: ! fprettier: --case 1 1 1 1
    // which means lowercase for keywords, procedures, operators, and types
    let mut case_dict = HashMap::new();
    case_dict.insert("keywords".to_string(), 1); // lowercase
    case_dict.insert("procedures".to_string(), 1); // lowercase
    case_dict.insert("operators".to_string(), 1); // lowercase
    case_dict.insert("types".to_string(), 1); // lowercase

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        format_decl: false,
        case_dict,
        ..Default::default()
    };

    test_compatibility(&input, &expected, &config);
}

/// Test test_fypp.f90 compatibility
/// This file tests fypp preprocessor directive handling including:
/// - Fypp directives embedded in Fortran continuation lines
/// - Nested fypp/Fortran scope tracking
/// - #:else and #:elif handling with inner Fortran scopes
#[test]
fn test_fypp_file_compatibility() {
    let input = get_example_path("in", "test_fypp.f90");
    let expected = get_example_path("out", "test_fypp.f90");

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        format_decl: false,
        ..Default::default()
    };

    test_compatibility(&input, &expected, &config);
}

/// Test test_invalid.f90 compatibility
/// This file contains intentionally invalid/incomplete Fortran code
/// to test error recovery and handling of malformed input
#[test]
fn test_invalid_file_compatibility() {
    let input = get_example_path("in", "test_invalid.f90");
    let expected = get_example_path("out", "test_invalid.f90");

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        format_decl: false,
        ..Default::default()
    };

    test_compatibility(&input, &expected, &config);
}

/// Test test_namelist_block_select.f90 compatibility
/// This file tests namelist, block construct, and select type handling
#[test]
fn test_namelist_block_select_compatibility() {
    let input = get_example_path("in", "test_namelist_block_select.f90");
    let expected = get_example_path("out", "test_namelist_block_select.f90");

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        format_decl: false,
        ..Default::default()
    };

    test_compatibility(&input, &expected, &config);
}

/// Test where_forall.f90 compatibility
/// This file tests WHERE and FORALL construct handling
#[test]
fn test_where_forall_compatibility() {
    let input = get_example_path("in", "where_forall.f90");
    let expected = get_example_path("out", "where_forall.f90");

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        format_decl: false,
        ..Default::default()
    };

    test_compatibility(&input, &expected, &config);
}

// ============================================================================
// Additional Unit Tests - Extended Coverage
// ============================================================================

/// Test fypp call/endcall directive (ported from `test_fypp`)
#[test]
fn test_compat_fypp_call() {
    let input = concat!(
        "#:call DEBUG_CODE\n",
        "  if (a < b) then\n",
        "    print *, \"DEBUG: a is less than b\"\n",
        "  end if\n",
        "#:endcall DEBUG_CODE\n"
    );

    let expected = concat!(
        "#:call DEBUG_CODE\n",
        "   if (a < b) then\n",
        "      print *, \"DEBUG: a is less than b\"\n",
        "   end if\n",
        "#:endcall DEBUG_CODE\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Fypp call/endcall formatting mismatch");
}

/// Test fypp mute/endmute directive (ported from `test_fypp`)
#[test]
fn test_compat_fypp_mute() {
    let input = concat!(
        "#:if DEBUG > 0\n",
        "print *, \"hola\"\n",
        "if (.not. (${cond}$)) then\n",
        "   #:mute\n",
        "   print *, \"Muted\"\n",
        "   #:endmute\n",
        "   error stop\n",
        "end if\n",
        "#:endif\n"
    );

    let expected = concat!(
        "#:if DEBUG > 0\n",
        "   print *, \"hola\"\n",
        "   if (.not. (${cond}$)) then\n",
        "      #:mute\n",
        "         print *, \"Muted\"\n",
        "      #:endmute\n",
        "      error stop\n",
        "   end if\n",
        "#:endif\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Fypp mute/endmute formatting mismatch");
}

/// Test fypp directives in continuation lines (ported from `test_fypp`)
#[test]
fn test_compat_fypp_continuation() {
    let input = concat!(
        "program try\n",
        "#:def mydef\n",
        "a = &\n",
        "#:if dothat\n",
        "b + &\n",
        "#:else\n",
        "c + &\n",
        "#:endif\n",
        "d\n",
        "#:enddef\n",
        "end program\n"
    );

    let expected = concat!(
        "program try\n",
        "   #:def mydef\n",
        "      a = &\n",
        "#:if dothat\n",
        "         b + &\n",
        "#:else\n",
        "         c + &\n",
        "#:endif\n",
        "         d\n",
        "   #:enddef\n",
        "end program\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Fypp continuation formatting mismatch");
}

/// Test fypp with worktype/type conditionals (ported from `test_fypp`)
/// fprettier applies consistent indentation based on nesting level.
#[test]
fn test_compat_fypp_worktype() {
    let input = concat!(
        "#:if worktype\n",
        "      ${worktype}$, &\n",
        "#:else\n",
        "      ${type}$, &\n",
        "#:endif\n",
        "         DIMENSION(${arr_exp}$), &\n",
        "         POINTER :: work\n"
    );

    // fprettier applies consistent scope-based indentation.
    let expected = concat!(
        "#:if worktype\n",
        "   ${worktype}$, &\n",
        "#:else\n",
        "      ${type}$, &\n",
        "#:endif\n",
        "      DIMENSION(${arr_exp}$), &\n",
        "      POINTER :: work\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Fypp worktype formatting mismatch");
}

/// Test fypp DEBUG_CODE nested definition (ported from `test_fypp`)
#[test]
fn test_compat_fypp_debug_code() {
    let input = concat!(
        "#:def DEBUG_CODE( code)\n",
        "  #:if DEBUG > 0\n",
        "    $:code\n",
        "  #:endif\n",
        "#:enddef DEBUG_CODE\n"
    );

    let expected = concat!(
        "#:def DEBUG_CODE( code)\n",
        "   #:if DEBUG > 0\n",
        "      $:code\n",
        "   #:endif\n",
        "#:enddef DEBUG_CODE\n"
    );

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Fypp DEBUG_CODE formatting mismatch");
}

/// Test swap case for intrinsic function calls (ported from `test_swap_case`)
#[test]
fn test_compat_swap_case_intrinsics() {
    use std::collections::HashMap;

    let tests = [
        (
            "INTEGER,   PARAMETER :: dp1 = SELECTED_REAL_KIND ( 15 , 307)\n",
            "integer, parameter :: dp1 = selected_real_kind(15, 307)\n",
        ),
        // Type suffix 2_int64 -> 2_INT64
        (
            "IF (l.EQ.2) l=MAX  (l64, 2_int64)\n",
            "if (l .eq. 2) l = max(l64, 2_INT64)\n",
        ),
        // USE statement: int64 is not a type suffix here, it's a regular identifier
        (
            "USE ISO_FORTRAN_ENV, ONLY: int64\n",
            "use iso_fortran_env, only: int64\n",
        ),
    ];

    let mut case_dict = HashMap::new();
    case_dict.insert("keywords".to_string(), 1); // lowercase
    case_dict.insert("procedures".to_string(), 1); // lowercase
    case_dict.insert("operators".to_string(), 1); // lowercase
    case_dict.insert("types".to_string(), 2); // uppercase

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        case_dict,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(
            result, expected,
            "Swap case intrinsics mismatch for: {input:?}"
        );
    }
}

/// Test swap case for kind suffixes and literals (ported from `test_swap_case`)
/// Tests that type suffixes (_int64, _real64), kind= values, and exponents
/// are converted to uppercase when types=2.
#[test]
fn test_compat_swap_case_kind_suffixes() {
    use std::collections::HashMap;

    // Convert all type suffixes and exponents to uppercase when types=2
    let tests = [
        (
            "INTEGER(kind=int64), PARAMETER :: l64 = 2_int64\n",
            "integer(kind=INT64), parameter :: l64 = 2_INT64\n",
        ),
        (
            "REAL(kind=real64), PARAMETER :: r64a = 2._real64\n",
            "real(kind=REAL64), parameter :: r64a = 2._REAL64\n",
        ),
        (
            "REAL(kind=real64), PARAMETER :: r64b = 2.0_real64\n",
            "real(kind=REAL64), parameter :: r64b = 2.0_REAL64\n",
        ),
        (
            "REAL(kind=real64), PARAMETER :: r64c = .0_real64\n",
            "real(kind=REAL64), parameter :: r64c = .0_REAL64\n",
        ),
        (
            "REAL(kind=real64), PARAMETER :: r64a = 2.e3_real64\n",
            "real(kind=REAL64), parameter :: r64a = 2.E3_REAL64\n",
        ),
        (
            "REAL(kind=real64), PARAMETER :: r64b = 2.0e3_real64\n",
            "real(kind=REAL64), parameter :: r64b = 2.0E3_REAL64\n",
        ),
        (
            "REAL(kind=real64), PARAMETER :: r64c = .0e3_real64\n",
            "real(kind=REAL64), parameter :: r64c = .0E3_REAL64\n",
        ),
        // Exponents without kind suffix are also converted (e3 -> E3, d3 -> D3)
        (
            "REAL, PARAMETER :: r32 = 2.e3\n",
            "real, parameter :: r32 = 2.E3\n",
        ),
        (
            "REAL, PARAMETER :: r32 = 2.0d3\n",
            "real, parameter :: r32 = 2.0D3\n",
        ),
        (
            "REAL, PARAMETER :: r32 = .2e3\n",
            "real, parameter :: r32 = .2E3\n",
        ),
    ];

    let mut case_dict = HashMap::new();
    case_dict.insert("keywords".to_string(), 1); // lowercase
    case_dict.insert("procedures".to_string(), 1); // lowercase
    case_dict.insert("operators".to_string(), 1); // lowercase
    case_dict.insert("types".to_string(), 2); // uppercase

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        case_dict,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(
            result, expected,
            "Swap case kind suffixes mismatch for: {input:?}"
        );
    }
}

/// Test swap case for CHARACTER with string parameters (ported from `test_swap_case`)
#[test]
fn test_compat_swap_case_character() {
    use std::collections::HashMap;

    let tests = [
        (
            "CHARACTER(LEN=*), PARAMETER :: a = \"INTEGER,   PARAMETER\" // \"b\"\n",
            "character(LEN=*), parameter :: a = \"INTEGER,   PARAMETER\"//\"b\"\n",
        ),
        (
            "CHARACTER(LEN=*), PARAMETER :: a = 'INTEGER,   PARAMETER' // 'b'\n",
            "character(LEN=*), parameter :: a = 'INTEGER,   PARAMETER'//'b'\n",
        ),
    ];

    let mut case_dict = HashMap::new();
    case_dict.insert("keywords".to_string(), 1); // lowercase
    case_dict.insert("procedures".to_string(), 1); // lowercase
    case_dict.insert("operators".to_string(), 1); // lowercase
    case_dict.insert("types".to_string(), 2); // uppercase

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        case_dict,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(
            result, expected,
            "Swap case character mismatch for: {input:?}"
        );
    }
}

/// Test relation replacement edge cases - strings with operators (ported from `test_relation_replacement`)
#[test]
fn test_compat_relation_replacement_strings() {
    // String literal with == inside - should be preserved
    let input = "'==== heading\n";

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        enable_replacements: true,
        c_relations: false,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(
        result, input,
        "String with == should be preserved unchanged"
    );
}

/// Test relation replacement edge cases - format strings (ported from `test_relation_replacement`)
#[test]
fn test_compat_relation_replacement_format_string() {
    let tests = [
        (
            "if (vtk%my_rank .eq. 0) write (vtk%filehandle_par, '(\"<DataArray\"\n",
            "if (vtk%my_rank .eq. 0) write (vtk%filehandle_par, '(\"<DataArray\"\n",
        ),
        ("'(\"</Collection>\",\n", "'(\"</Collection>\",\n"),
    ];

    let config = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        enable_replacements: true,
        c_relations: false,
        ..Default::default()
    };

    for (input, expected) in tests {
        let result = run_format(input, &config);
        assert_eq!(
            result, expected,
            "Format string replacement mismatch for: {input:?}"
        );
    }
}

/// Test relation replacement with continuation lines (ported from `test_relation_replacement`)
#[test]
fn test_compat_relation_replacement_continuation() {
    let input = "if (abc(1) .lt. -bca .or. &\n qwe .gt. ewq) then\n";
    let expected_f = "if (abc(1) .lt. -bca .or. &\n    qwe .gt. ewq) then\n";
    let expected_c = "if (abc(1) < -bca .or. &\n    qwe > ewq) then\n";

    // Fortran-style
    let config_f = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        enable_replacements: true,
        c_relations: false,
        ..Default::default()
    };

    let result_f = run_format(input, &config_f);
    assert_eq!(
        result_f, expected_f,
        "Fortran relation continuation mismatch"
    );

    // C-style
    let config_c = Config {
        impose_whitespace: true,
        impose_indent: true,
        indent: 3,
        whitespace: 2,
        enable_replacements: true,
        c_relations: true,
        ..Default::default()
    };

    let result_c = run_format(input, &config_c);
    assert_eq!(result_c, expected_c, "C relation continuation mismatch");
}

/// Test declaration with whitespace-decl=0 (ported from `test_decl`)
#[test]
fn test_compat_decl_whitespace_zero() {
    use std::collections::HashMap;

    let input = "integer, dimension(:)    ::     a\n";
    let expected = "integer, dimension(:)::a\n";

    let mut whitespace_dict = HashMap::new();
    whitespace_dict.insert("decl".to_string(), false);

    let config = Config {
        impose_whitespace: true,
        impose_indent: false,
        whitespace: 2,
        format_decl: true,
        whitespace_dict,
        ..Default::default()
    };

    let result = run_format(input, &config);
    assert_eq!(result, expected, "Declaration whitespace-decl=0 mismatch");
}
