//! Fortran scope tracking and indentation control.
//!
//! Every construct that opens an indented scope is one row of [`SCOPES`]:
//! block constructs (IF/THEN, DO, SELECT CASE, WHERE, FORALL), program units
//! (MODULE, PROGRAM, SUBMODULE), subprograms (SUBROUTINE, FUNCTION,
//! INTERFACE), type definitions (TYPE, ENUM, ASSOCIATE, BLOCK) and fypp
//! preprocessor directives (`#:if`, `#:for`, `#:def`, ...).
//!
//! A scope is identified by its index into that table, which is what
//! [`F90Indenter`](crate::format::F90Indenter) pushes on its stack.

use std::sync::LazyLock;

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

/// A lazily compiled pattern, as stored in [`crate::parser::patterns`]
type Re = LazyLock<Regex>;

/// Which configuration flag decides whether a scope is tracked
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Group {
    /// Always tracked
    Core,
    /// Tracked only when `indent_mod` is set
    Module,
    /// Tracked only when `indent_fypp` is set
    Fypp,
}

/// One Fortran construct that opens an indented scope
pub struct Scope {
    pub group: Group,
    /// Statement that opens the scope
    pub open: Option<&'static Re>,
    /// Statement that continues it at the parent indent (ELSE, CASE, CONTAINS)
    pub cont: Option<&'static Re>,
    /// Statement that closes it
    pub close: Option<&'static Re>,
    /// Whether a close must match this scope exactly; a bare END matches any
    pub spec: bool,
    /// WHERE/FORALL, which open a block only when nothing follows the `)`
    pub conditional: bool,
}

impl Scope {
    /// A scope whose close must match it exactly and always opens a block
    const fn new(
        group: Group,
        open: Option<&'static Re>,
        cont: Option<&'static Re>,
        close: Option<&'static Re>,
    ) -> Self {
        Self {
            group,
            open,
            cont,
            close,
            spec: true,
            conditional: false,
        }
    }

    /// Whether this is a fypp preprocessor scope
    #[must_use]
    pub fn is_fypp(&self) -> bool {
        self.group == Group::Fypp
    }
}

/// Every tracked scope. Indices are stable: they are the scope identity used
/// by the indenter's stack, so rows may be appended but not reordered.
pub static SCOPES: &[Scope] = {
    use Group::{Core, Fypp, Module};
    &[
        // 0: IF/THEN/ELSE/ENDIF
        Scope::new(Core, Some(&IF_RE), Some(&ELSE_RE), Some(&ENDIF_RE)),
        // 1: DO/ENDDO
        Scope::new(Core, Some(&DO_RE), None, Some(&ENDDO_RE)),
        // 2: SELECT CASE/RANK/TYPE
        Scope::new(Core, Some(&SELCASE_RE), Some(&CASE_RE), Some(&ENDSEL_RE)),
        // 3-4: SUBROUTINE / FUNCTION (CONTAINS reduces indent)
        Scope::new(Core, Some(&SUBR_RE), Some(&CONTAINS_RE), Some(&ENDSUBR_RE)),
        Scope::new(Core, Some(&FCT_RE), Some(&CONTAINS_RE), Some(&ENDFCT_RE)),
        // 5: INTERFACE
        Scope::new(Core, Some(&INTERFACE_RE), None, Some(&ENDINTERFACE_RE)),
        // 6: TYPE
        Scope::new(Core, Some(&TYPE_RE), Some(&CONTAINS_RE), Some(&ENDTYPE_RE)),
        // 7-8: ENUM / ASSOCIATE
        Scope::new(Core, Some(&ENUM_RE), None, Some(&ENDENUM_RE)),
        Scope::new(Core, Some(&ASSOCIATE_RE), None, Some(&ENDASSOCIATE_RE)),
        // 9: generic END, closes whatever is innermost (spec = false)
        Scope {
            spec: false,
            ..Scope::new(Core, None, None, Some(&ENDANY_RE))
        },
        // 10: BLOCK
        Scope::new(Core, Some(&BLK_RE), None, Some(&ENDBLK_RE)),
        // 11-12: WHERE / FORALL, block only when nothing follows the `)`
        Scope {
            conditional: true,
            ..Scope::new(
                Core,
                Some(&WHERE_RE),
                Some(&ELSEWHERE_RE),
                Some(&ENDWHERE_RE),
            )
        },
        Scope {
            conditional: true,
            ..Scope::new(Core, Some(&FORALL_RE), None, Some(&ENDFORALL_RE))
        },
        // 13-15: MODULE / SUBMODULE / PROGRAM (CONTAINS reduces indent)
        Scope::new(Module, Some(&MOD_RE), Some(&CONTAINS_RE), Some(&ENDMOD_RE)),
        Scope::new(
            Module,
            Some(&SMOD_RE),
            Some(&CONTAINS_RE),
            Some(&ENDSMOD_RE),
        ),
        Scope::new(
            Module,
            Some(&PROG_RE),
            Some(&CONTAINS_RE),
            Some(&ENDPROG_RE),
        ),
        // 16-21: fypp DEF / IF / FOR / BLOCK / CALL / MUTE
        Scope::new(Fypp, Some(&FYPP_DEF_RE), None, Some(&FYPP_ENDDEF_RE)),
        Scope::new(
            Fypp,
            Some(&FYPP_IF_RE),
            Some(&FYPP_ELIF_ELSE_RE),
            Some(&FYPP_ENDIF_RE),
        ),
        Scope::new(Fypp, Some(&FYPP_FOR_RE), None, Some(&FYPP_ENDFOR_RE)),
        Scope::new(Fypp, Some(&FYPP_BLOCK_RE), None, Some(&FYPP_ENDBLOCK_RE)),
        Scope::new(Fypp, Some(&FYPP_CALL_RE), None, Some(&FYPP_ENDCALL_RE)),
        Scope::new(Fypp, Some(&FYPP_MUTE_RE), None, Some(&FYPP_ENDMUTE_RE)),
    ]
};

/// Selects which rows of [`SCOPES`] are tracked for a given configuration
#[derive(Debug, Clone, Copy)]
pub struct ScopeParser {
    indent_fypp: bool,
    indent_mod: bool,
}

impl ScopeParser {
    /// Whether the scope at `idx` is tracked under this configuration
    fn enabled(self, idx: usize) -> bool {
        match SCOPES.get(idx).map(|s| s.group) {
            Some(Group::Core) => true,
            Some(Group::Module) => self.indent_mod,
            Some(Group::Fypp) => self.indent_fypp,
            None => false,
        }
    }

    /// The tracked scopes, paired with their index (their identity)
    pub fn iter(self) -> impl Iterator<Item = (usize, &'static Scope)> {
        SCOPES
            .iter()
            .enumerate()
            .filter(move |(idx, _)| self.enabled(*idx))
    }

    /// The scope at `idx`, if it is tracked under this configuration
    #[must_use]
    pub fn get(self, idx: usize) -> Option<&'static Scope> {
        self.enabled(idx).then(|| &SCOPES[idx])
    }
}

/// Build a scope parser for the given configuration
///
/// # Arguments
/// * `indent_fypp` - Whether to indent fypp preprocessor blocks
/// * `indent_mod` - Whether to indent module/program/submodule
#[must_use]
pub fn build_scope_parser(indent_fypp: bool, indent_mod: bool) -> ScopeParser {
    ScopeParser {
        indent_fypp,
        indent_mod,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_core_always_tracked() {
        let parser = build_scope_parser(false, false);
        let (idx, scope) = parser.iter().next().unwrap();
        assert_eq!(idx, 0);
        assert!(scope.open.unwrap().is_match("if (x > 0) then"));
        assert!(scope.cont.unwrap().is_match("else"));
        assert!(scope.close.unwrap().is_match("end if"));
    }

    #[test]
    fn test_optional_groups_gated_by_flags() {
        let bare = build_scope_parser(false, false);
        assert!(bare.get(13).is_none(), "MODULE needs indent_mod");
        assert!(bare.get(17).is_none(), "fypp IF needs indent_fypp");

        assert!(build_scope_parser(false, true).get(13).is_some());
        assert!(build_scope_parser(true, false).get(17).is_some());
    }

    #[test]
    fn test_scope_attributes() {
        // A bare END closes any scope; everything else must match exactly
        assert!(!SCOPES[9].spec);
        assert!(SCOPES.iter().enumerate().all(|(i, s)| i == 9 || s.spec));
        // Only WHERE and FORALL are conditional blocks
        let conditional: Vec<usize> = SCOPES
            .iter()
            .enumerate()
            .filter(|(_, s)| s.conditional)
            .map(|(i, _)| i)
            .collect();
        assert_eq!(conditional, vec![11, 12]);
        // fypp scopes are the last six rows
        assert!((16..22).all(|i| SCOPES[i].is_fypp()));
        assert!(!SCOPES[15].is_fypp());
    }
}
