/// Regex patterns for Fortran syntax
///
/// All patterns are compiled once at startup using `LazyLock`.
///
/// All regexes use case-insensitive + unicode flags
use std::sync::LazyLock;

use regex::{Regex, RegexBuilder};

/// Build a case-insensitive regex from a compile-time constant pattern.
///
/// # Panics
///
/// Panics if the pattern is invalid. This is acceptable because all patterns
/// in this module are compile-time constants that are verified by tests.
/// The panic occurs at first access of the `LazyLock` static.
fn build_re(pattern: &str) -> Regex {
    RegexBuilder::new(pattern)
        .case_insensitive(true)
        .unicode(true)
        .build()
        .unwrap_or_else(|_| panic!("Invalid regex pattern: {pattern}"))
}

// Anchor patterns
const EOL_STR: &str = r"\s*;?\s*$"; // End of line
const SOL_STR: &str = r"^\s*"; // Start of line

// ===== STRUCTURE KEYWORDS =====

// IF/THEN/ELSE/ENDIF
pub static IF_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r"{SOL_STR}(\w+\s*:)?\s*IF\s*\(.*\)\s*THEN{EOL_STR}"
    ))
});
pub static ELSE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}ELSE(\s*IF\s*\(.*\)\s*THEN)?{EOL_STR}")));
pub static ENDIF_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*IF(\s+\w+)?{EOL_STR}")));

// DO/ENDDO
pub static DO_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}(\w+\s*:)?\s*DO({EOL_STR}|\s+\w)")));
pub static ENDDO_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*DO(\s+\w+)?{EOL_STR}")));

// SELECT CASE/RANK/TYPE
pub static SELCASE_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r"{SOL_STR}(\w+\s*:)?\s*SELECT\s*(CASE|RANK|TYPE)\s*\(.*\){EOL_STR}"
    ))
});
pub static CASE_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r"{SOL_STR}((CASE|RANK|TYPE\s+IS|CLASS\s+IS)\s*(\(.*\)|DEFAULT)|CLASS\s+DEFAULT){EOL_STR}"
    ))
});
pub static ENDSEL_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*SELECT(\s+\w+)?{EOL_STR}")));

// SUBROUTINE
pub static SUBR_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r#"^([^"']* )?SUBROUTINE\s+\w+\s*(\(.*\))?{EOL_STR}"#
    ))
});
pub static ENDSUBR_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*SUBROUTINE(\s+\w+)?{EOL_STR}")));

// FUNCTION
pub static FCT_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r#"^([^"']* )?FUNCTION\s+\w+\s*(\(.*\))?(\s*RESULT\s*\(\w+\))?{EOL_STR}"#
    ))
});
pub static ENDFCT_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*FUNCTION(\s+\w+)?{EOL_STR}")));

// MODULE
pub static MOD_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}MODULE\s+\w+{EOL_STR}")));
pub static ENDMOD_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*MODULE(\s+\w+)?{EOL_STR}")));

// SUBMODULE
pub static SMOD_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}SUBMODULE\s*\(\w+\)\s+\w+{EOL_STR}")));
pub static ENDSMOD_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*SUBMODULE(\s+\w+)?{EOL_STR}")));

// PROGRAM
pub static PROG_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}PROGRAM\s+\w+{EOL_STR}")));
pub static ENDPROG_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*PROGRAM(\s+\w+)?{EOL_STR}")));

// TYPE
pub static TYPE_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r"{SOL_STR}TYPE(\s*,\s*(BIND\s*\(\s*C\s*\)|EXTENDS\s*\(.*\)|ABSTRACT|PUBLIC|PRIVATE))*(\s*,\s*)?(\s*::\s*|\s+)\w+{EOL_STR}"
    ))
});
pub static ENDTYPE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*TYPE(\s+\w+)?{EOL_STR}")));

// INTERFACE
pub static INTERFACE_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r"{SOL_STR}(ABSTRACT\s+)?INTERFACE(\s+\w+|\s+OPERATOR\s*\(.*\)|\s+ASSIGNMENT\s*\(.*\))?{EOL_STR}"
    ))
});
pub static ENDINTERFACE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*INTERFACE(\s+\w+)?{EOL_STR}")));

// ASSOCIATE
pub static ASSOCIATE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}ASSOCIATE\s*\(.*\){EOL_STR}")));
pub static ENDASSOCIATE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*ASSOCIATE{EOL_STR}")));

// ENUM
pub static ENUM_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r"{SOL_STR}ENUM(\s*,\s*BIND\s*\(\s*C\s*\))?{EOL_STR}"
    ))
});
pub static ENDENUM_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*ENUM{EOL_STR}")));

// BLOCK
pub static BLK_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}(\w+\s*:)?\s*BLOCK{EOL_STR}")));
pub static ENDBLK_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*BLOCK(\s+\w+)?{EOL_STR}")));

// WHERE
pub static WHERE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}(\w+\s*:\s*)?WHERE\s*\(.*\){EOL_STR}")));
pub static ELSEWHERE_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r"{SOL_STR}ELSE\s*WHERE(\s*\(.*\))?(\s+\w+)?{EOL_STR}"
    ))
});
pub static ENDWHERE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*WHERE(\s+\w+)?{EOL_STR}")));

// FORALL
pub static FORALL_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}(\w+\s*:\s*)?FORALL\s*\(.*\){EOL_STR}")));
pub static ENDFORALL_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END\s*FORALL(\s+\w+)?{EOL_STR}")));

// Generic END (matches any scope)
pub static ENDANY_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}END{EOL_STR}")));

// CONTAINS
pub static CONTAINS_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}CONTAINS{EOL_STR}")));

// ===== DELIMITERS =====

pub static DEL_OPEN_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"(\(\/?|\[)"));
pub static DEL_CLOSE_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"(\/?\)|\])"));

// ===== KEYWORD-PARENTHESIS SPACING =====

// Intrinsic statements that should have space before parentheses
pub static INTR_STMTS_PAR_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(
        r"\b(ALLOCATE|DEALLOCATE|OPEN|CLOSE|READ|WRITE|FLUSH|ENDFILE|REWIND|BACKSPACE|INQUIRE|FORALL|WHERE|ASSOCIATE|NULLIFY)\s*$",
    )
});

// Keywords before parentheses that need space (using line context)
// Matches: IF, ELSE IF, DO WHILE, CASE, SELECT RANK/TYPE, CLASS IS/DEFAULT, TYPE IS
pub static KEYWORD_PAREN_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(
        r"^\s*(?:(?:\w+\s*:)?(?:ELSE)?\s*IF|(?:\w+\s*:)?\s*DO\s+WHILE|(?:SELECT)?\s*CASE|(?:SELECT)?\s*RANK|SELECT\s*TYPE|CLASS\s*DEFAULT|(?:TYPE|CLASS)\s+IS)\s*$",
    )
});

// ===== LINE CONTINUATION =====

pub static LINEBREAK_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"(&)[\s]*(?:!.*)?$"));

// ===== DECLARATIONS AND STATEMENTS =====

// Variable declarations (integer, real, character, etc.)
pub static VAR_DECL_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(
        r"^ *(?:integer(?: *\* *[0-9]+)?|logical|character(?: *\* *[0-9]+)?|real(?: *\* *[0-9]+)?|complex(?: *\* *[0-9]+)?|type) *(?:\((?:[^()]+|\((?:[^()]+|\([^()]*\))*\))*\))? *(?:(?: *, *[a-zA-Z_0-9]+(?: *\((?:[^()]+|\((?:[^()]+|\([^()]*\))*\))*\))?)+)? *(?:::)?(?:[^\n]+)\n?",
    )
});

// USE statement
pub static USE_RE: LazyLock<Regex> = LazyLock::new(|| build_re(&format!(r"{SOL_STR}USE(\s+|,)")));

// PUBLIC declaration
pub static PUBLIC_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}PUBLIC\s*::")));

// PRIVATE declaration
pub static PRIVATE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}PRIVATE\s*::")));

// ===== END KEYWORD SEPARATION =====

// Pattern to match compound END keywords (e.g., ENDIF, ENDDO) for separation
pub static END_RE: LazyLock<Regex> = LazyLock::new(|| {
    build_re(&format!(
        r"{SOL_STR}(END)\s*(IF|DO|SELECT|ASSOCIATE|BLOCK|SUBROUTINE|FUNCTION|MODULE|SUBMODULE|TYPE|PROGRAM|INTERFACE|ENUM|WHERE|FORALL)"
    ))
});

// ===== OPERATORS =====

// Relational operators
pub static REL_OP_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(r"\.(?:EQ|NE|LT|LE|GT|GE)\.|(?:==|/=|<=|<|>=|>)"));

// Logical operators
pub static LOG_OP_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"\.(?:AND|OR|EQV|NEQV|NOT)\."));

// Plus/minus (excluding scientific notation like 1.0E-5)
pub static PLUSMINUS_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"(?<=[^ \d])(\+|-)"));

// Multiply/divide (excluding ** and //)
pub static MULTDIV_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"(?<=[^\*/])(\*|/)(?![/\*])"));

// ===== FYPP PREPROCESSOR =====

// FYPP DEF
pub static FYPP_DEF_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:DEF\s+")));
pub static FYPP_ENDDEF_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:ENDDEF")));

// FYPP IF/ELIF/ELSE/ENDIF
pub static FYPP_IF_RE: LazyLock<Regex> = LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:IF\s+")));
pub static FYPP_ELIF_ELSE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:(ELIF\s+|ELSE)")));
pub static FYPP_ENDIF_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:ENDIF")));

// FYPP FOR
pub static FYPP_FOR_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:FOR\s+")));
pub static FYPP_ENDFOR_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:ENDFOR")));

// FYPP BLOCK
pub static FYPP_BLOCK_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:BLOCK\s+")));
pub static FYPP_ENDBLOCK_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:ENDBLOCK")));

// FYPP CALL
pub static FYPP_CALL_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:CALL\s+")));
pub static FYPP_ENDCALL_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:ENDCALL")));

// FYPP MUTE
pub static FYPP_MUTE_RE: LazyLock<Regex> = LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:MUTE")));
pub static FYPP_ENDMUTE_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(&format!(r"{SOL_STR}#:ENDMUTE")));

// ===== OMP (OpenMP) DIRECTIVES =====

// OMP conditional Fortran statement: "!$ " (code only active with OpenMP)
pub static OMP_COND_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"^\s*(!\$ )"));

// OMP directive: "!$OMP" (OpenMP directive)
pub static OMP_DIR_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"^\s*(!\$OMP)"));

// ===== STATEMENT LABELS =====

// Statement label: numeric prefix like "100 " at start of line
// Matches digits followed by space, with non-whitespace content somewhere after
pub static STATEMENT_LABEL_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"^\s*(\d+\s).*\S"));

// NO_ALIGN_RE: Matches lines starting with & followed by non-whitespace content
// When any line in a multi-line statement matches this, use manual alignment instead of auto
pub static NO_ALIGN_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"^\s*&\s*\S+"));

// PRE_AMPERSAND_RE: Matches and captures leading & and any trailing whitespace
// Used by remove_pre_ampersands to extract the ampersand portion
pub static PRE_AMPERSAND_RE: LazyLock<Regex> = LazyLock::new(|| build_re(r"^\s*(&\s*)"));

// TRAILING_AMPERSAND_RE: Matches trailing & with optional whitespace/comment
// Used to capture whitespace before trailing ampersand for ampersand_sep
pub static TRAILING_AMPERSAND_RE: LazyLock<Regex> =
    LazyLock::new(|| build_re(r"(\s*)&\s*(?:!.*)?$"));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_if_regex() {
        assert!(IF_RE.is_match("if (x > 0) then"));
        assert!(IF_RE.is_match("  IF (x > 0) THEN"));
        assert!(IF_RE.is_match("label: if (x) then"));
        assert!(!IF_RE.is_match("if (x > 0)")); // Missing THEN
    }

    #[test]
    fn test_do_regex() {
        assert!(DO_RE.is_match("do"));
        assert!(DO_RE.is_match("DO i = 1, 10"));
        assert!(DO_RE.is_match("loop: do"));
    }

    #[test]
    fn test_subroutine_regex() {
        assert!(SUBR_RE.is_match("subroutine foo()"));
        assert!(SUBR_RE.is_match("SUBROUTINE bar"));
        assert!(SUBR_RE.is_match("recursive subroutine baz(x, y)"));
    }

    #[test]
    fn test_function_regex() {
        assert!(FCT_RE.is_match("function foo()"));
        assert!(FCT_RE.is_match("INTEGER FUNCTION bar(x)"));
        assert!(FCT_RE.is_match("function baz() result(r)"));
    }

    #[test]
    fn test_module_regex() {
        assert!(MOD_RE.is_match("module my_module"));
        assert!(MOD_RE.is_match("MODULE MyModule"));
    }

    #[test]
    fn test_case_insensitive() {
        assert!(IF_RE.is_match("IF (x) THEN"));
        assert!(IF_RE.is_match("if (x) then"));
        assert!(IF_RE.is_match("If (X) Then"));
    }

    #[test]
    fn test_relational_operators() {
        assert!(REL_OP_RE.is_match("x .EQ. y"));
        assert!(REL_OP_RE.is_match("x == y"));
        assert!(REL_OP_RE.is_match("x /= y"));
        assert!(REL_OP_RE.is_match("x < y"));
    }

    #[test]
    fn test_logical_operators() {
        assert!(LOG_OP_RE.is_match("x .AND. y"));
        assert!(LOG_OP_RE.is_match("x .OR. y"));
        assert!(LOG_OP_RE.is_match(".NOT. x"));
    }

    #[test]
    fn test_omp_conditional() {
        // OMP conditional: "!$ " (with space)
        assert!(OMP_COND_RE.is_match("!$ x = 5"));
        assert!(OMP_COND_RE.is_match("  !$ x = 5")); // Leading whitespace ok
        assert!(!OMP_COND_RE.is_match("!$OMP")); // This is a directive, not conditional
        assert!(!OMP_COND_RE.is_match("! $ x")); // Space after ! is not OMP
    }

    #[test]
    fn test_omp_directive() {
        // OMP directive: "!$OMP" (no space before OMP)
        assert!(OMP_DIR_RE.is_match("!$OMP PARALLEL"));
        assert!(OMP_DIR_RE.is_match("  !$OMP DO"));
        assert!(!OMP_DIR_RE.is_match("!$ x = 5")); // This is conditional, not directive
    }

    #[test]
    fn test_statement_label() {
        // Statement labels: numeric prefix at start of line followed by content
        assert!(STATEMENT_LABEL_RE.is_match("100 CONTINUE"));
        assert!(STATEMENT_LABEL_RE.is_match("  200 FORMAT(A)"));
        assert!(STATEMENT_LABEL_RE.is_match("999 x = 5"));
        // Double space between label and statement should also match
        assert!(STATEMENT_LABEL_RE.is_match("1003  FORMAT(A)"));
        // Should not match if it's just a number with no content after
        assert!(!STATEMENT_LABEL_RE.is_match("100 "));
        assert!(!STATEMENT_LABEL_RE.is_match("100"));
        // Should capture the label with trailing space (only one space captured)
        let caps = STATEMENT_LABEL_RE.captures("100 CONTINUE").unwrap();
        assert_eq!(caps.get(1).unwrap().as_str(), "100 ");
        // For double space, only one space is captured in the label
        let caps2 = STATEMENT_LABEL_RE.captures("1003  FORMAT(A)").unwrap();
        assert_eq!(caps2.get(1).unwrap().as_str(), "1003 ");
    }

    #[test]
    fn test_select_case_regex() {
        // Without label
        assert!(SELCASE_RE.is_match("select case(i)"));
        assert!(SELCASE_RE.is_match("SELECT CASE (i)"));
        assert!(SELCASE_RE.is_match("  select case (x + 1)"));
        // With label
        assert!(SELCASE_RE.is_match("label: select case(i)"));
        assert!(SELCASE_RE.is_match("casetest: select case(i)"));
        assert!(SELCASE_RE.is_match("  mycase: SELECT CASE (x)"));
        // SELECT RANK and SELECT TYPE
        assert!(SELCASE_RE.is_match("select rank(x)"));
        assert!(SELCASE_RE.is_match("select type(x)"));
        assert!(SELCASE_RE.is_match("label: select rank(x)"));
    }

    #[test]
    fn test_end_select_regex() {
        // Without name
        assert!(ENDSEL_RE.is_match("end select"));
        assert!(ENDSEL_RE.is_match("END SELECT"));
        assert!(ENDSEL_RE.is_match("endselect"));
        // With name
        assert!(ENDSEL_RE.is_match("end select casetest"));
        assert!(ENDSEL_RE.is_match("END SELECT mycase"));
    }
}
