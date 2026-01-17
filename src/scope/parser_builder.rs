/// Scope parser builder - creates regex-based parsers for scope tracking
use regex::Regex;

use crate::parser::patterns::{
    ASSOCIATE_RE, BLK_RE, CASE_RE, CONTAINS_RE, DO_RE, ELSEWHERE_RE, ELSE_RE, ENDANY_RE,
    ENDASSOCIATE_RE, ENDBLK_RE, ENDDO_RE, ENDENUM_RE, ENDFCT_RE, ENDFORALL_RE, ENDIF_RE,
    ENDINTERFACE_RE, ENDMOD_RE, ENDPROG_RE, ENDSEL_RE, ENDSMOD_RE, ENDSUBR_RE, ENDTYPE_RE,
    ENDWHERE_RE, ENUM_RE, FCT_RE, FORALL_RE, FYPP_BLOCK_RE, FYPP_CALL_RE, FYPP_DEF_RE,
    FYPP_ELIF_ELSE_RE, FYPP_ENDBLOCK_RE, FYPP_ENDCALL_RE, FYPP_ENDDEF_RE, FYPP_ENDFOR_RE,
    FYPP_ENDIF_RE, FYPP_ENDMUTE_RE, FYPP_FOR_RE, FYPP_IF_RE, FYPP_MUTE_RE, IF_RE, INTERFACE_RE,
    MOD_RE, PROG_RE, SELCASE_RE, SMOD_RE, SUBR_RE, TYPE_RE, WHERE_RE,
};

/// Wrapper around a regex for scope parsing
#[derive(Clone)]
pub struct ParserRe {
    re: &'static Regex,
    pub spec: bool, // Whether scope must match exactly on close
}

impl ParserRe {
    #[must_use]
    pub fn new(re: &'static Regex, spec: bool) -> Self {
        Self { re, spec }
    }

    #[must_use]
    pub fn is_match(&self, line: &str) -> bool {
        self.re.is_match(line)
    }
}

/// Total number of scope type slots (0-21)
const NUM_SCOPE_TYPES: usize = 22;

/// Scope parser containing regex matchers for opening/continue/closing
#[derive(Clone)]
pub struct ScopeParser {
    /// Regexes for opening new scopes
    pub opening: Vec<Option<ParserRe>>,
    /// Regexes for continuing scopes (e.g., ELSE, CASE)
    pub continue_: Vec<Option<ParserRe>>,
    /// Regexes for closing scopes
    pub closing: Vec<Option<ParserRe>>,
}

/// Build scope parser based on configuration
///
/// # Arguments
/// * `indent_fypp` - Whether to indent fypp preprocessor blocks
/// * `indent_mod` - Whether to indent module/program/submodule
///
/// Returns a `ScopeParser` with 22 slots (0-21) for all scope types
#[must_use]
pub fn build_scope_parser(indent_fypp: bool, indent_mod: bool) -> ScopeParser {
    let mut opening = vec![None; NUM_SCOPE_TYPES];
    let mut continue_ = vec![None; NUM_SCOPE_TYPES];
    let mut closing = vec![None; NUM_SCOPE_TYPES];

    // Core Fortran scopes (0-12)

    // 0: IF/THEN/ELSE/ENDIF
    opening[0] = Some(ParserRe::new(&IF_RE, true));
    continue_[0] = Some(ParserRe::new(&ELSE_RE, true));
    closing[0] = Some(ParserRe::new(&ENDIF_RE, true));

    // 1: DO/ENDDO
    opening[1] = Some(ParserRe::new(&DO_RE, true));
    closing[1] = Some(ParserRe::new(&ENDDO_RE, true));

    // 2: SELECT CASE/RANK/TYPE
    opening[2] = Some(ParserRe::new(&SELCASE_RE, true));
    continue_[2] = Some(ParserRe::new(&CASE_RE, true));
    closing[2] = Some(ParserRe::new(&ENDSEL_RE, true));

    // 3: SUBROUTINE
    opening[3] = Some(ParserRe::new(&SUBR_RE, true));
    continue_[3] = Some(ParserRe::new(&CONTAINS_RE, true)); // CONTAINS reduces indent
    closing[3] = Some(ParserRe::new(&ENDSUBR_RE, true));

    // 4: FUNCTION
    opening[4] = Some(ParserRe::new(&FCT_RE, true));
    continue_[4] = Some(ParserRe::new(&CONTAINS_RE, true)); // CONTAINS reduces indent
    closing[4] = Some(ParserRe::new(&ENDFCT_RE, true));

    // 5: INTERFACE
    opening[5] = Some(ParserRe::new(&INTERFACE_RE, true));
    closing[5] = Some(ParserRe::new(&ENDINTERFACE_RE, true));

    // 6: TYPE
    opening[6] = Some(ParserRe::new(&TYPE_RE, true));
    continue_[6] = Some(ParserRe::new(&CONTAINS_RE, true)); // CONTAINS reduces indent
    closing[6] = Some(ParserRe::new(&ENDTYPE_RE, true));

    // 7: ENUM
    opening[7] = Some(ParserRe::new(&ENUM_RE, true));
    closing[7] = Some(ParserRe::new(&ENDENUM_RE, true));

    // 8: ASSOCIATE
    opening[8] = Some(ParserRe::new(&ASSOCIATE_RE, true));
    closing[8] = Some(ParserRe::new(&ENDASSOCIATE_RE, true));

    // 9: ENDANY (generic END - matches any scope, spec=false)
    closing[9] = Some(ParserRe::new(&ENDANY_RE, false));

    // 10: BLOCK
    opening[10] = Some(ParserRe::new(&BLK_RE, true));
    closing[10] = Some(ParserRe::new(&ENDBLK_RE, true));

    // 11: WHERE
    opening[11] = Some(ParserRe::new(&WHERE_RE, true));
    continue_[11] = Some(ParserRe::new(&ELSEWHERE_RE, true));
    closing[11] = Some(ParserRe::new(&ENDWHERE_RE, true));

    // 12: FORALL
    opening[12] = Some(ParserRe::new(&FORALL_RE, true));
    closing[12] = Some(ParserRe::new(&ENDFORALL_RE, true));

    // Module scopes (13-15) - only if indent_mod is true
    if indent_mod {
        // 13: MODULE
        opening[13] = Some(ParserRe::new(&MOD_RE, true));
        continue_[13] = Some(ParserRe::new(&CONTAINS_RE, true)); // CONTAINS reduces indent
        closing[13] = Some(ParserRe::new(&ENDMOD_RE, true));

        // 14: SUBMODULE
        opening[14] = Some(ParserRe::new(&SMOD_RE, true));
        continue_[14] = Some(ParserRe::new(&CONTAINS_RE, true)); // CONTAINS reduces indent
        closing[14] = Some(ParserRe::new(&ENDSMOD_RE, true));

        // 15: PROGRAM
        opening[15] = Some(ParserRe::new(&PROG_RE, true));
        continue_[15] = Some(ParserRe::new(&CONTAINS_RE, true)); // CONTAINS reduces indent
        closing[15] = Some(ParserRe::new(&ENDPROG_RE, true));
    }

    // Fypp preprocessor scopes (16-21) - only if indent_fypp is true
    if indent_fypp {
        // 16: FYPP DEF
        opening[16] = Some(ParserRe::new(&FYPP_DEF_RE, true));
        closing[16] = Some(ParserRe::new(&FYPP_ENDDEF_RE, true));

        // 17: FYPP IF
        opening[17] = Some(ParserRe::new(&FYPP_IF_RE, true));
        continue_[17] = Some(ParserRe::new(&FYPP_ELIF_ELSE_RE, true));
        closing[17] = Some(ParserRe::new(&FYPP_ENDIF_RE, true));

        // 18: FYPP FOR
        opening[18] = Some(ParserRe::new(&FYPP_FOR_RE, true));
        closing[18] = Some(ParserRe::new(&FYPP_ENDFOR_RE, true));

        // 19: FYPP BLOCK
        opening[19] = Some(ParserRe::new(&FYPP_BLOCK_RE, true));
        closing[19] = Some(ParserRe::new(&FYPP_ENDBLOCK_RE, true));

        // 20: FYPP CALL
        opening[20] = Some(ParserRe::new(&FYPP_CALL_RE, true));
        closing[20] = Some(ParserRe::new(&FYPP_ENDCALL_RE, true));

        // 21: FYPP MUTE
        opening[21] = Some(ParserRe::new(&FYPP_MUTE_RE, true));
        closing[21] = Some(ParserRe::new(&FYPP_ENDMUTE_RE, true));
    }

    ScopeParser {
        opening,
        continue_,
        closing,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_parser() {
        let parser = build_scope_parser(false, false);

        // Should have IF parser
        assert!(parser.opening[0].is_some());
        assert!(parser.continue_[0].is_some());
        assert!(parser.closing[0].is_some());

        // Should NOT have fypp parsers
        assert!(parser.opening[16].is_none());
    }

    #[test]
    fn test_fypp_parser() {
        let parser = build_scope_parser(true, false);

        // Should have fypp IF parser
        assert!(parser.opening[17].is_some());
        assert!(parser.closing[17].is_some());
    }

    #[test]
    fn test_module_parser() {
        let parser = build_scope_parser(false, true);

        // Should have MODULE parser
        assert!(parser.opening[13].is_some());
        assert!(parser.closing[13].is_some());
    }

    #[test]
    fn test_if_matching() {
        let parser = build_scope_parser(false, false);

        assert!(parser.opening[0]
            .as_ref()
            .unwrap()
            .is_match("if (x > 0) then"));
        assert!(parser.continue_[0].as_ref().unwrap().is_match("else"));
        assert!(parser.closing[0].as_ref().unwrap().is_match("end if"));
    }
}
