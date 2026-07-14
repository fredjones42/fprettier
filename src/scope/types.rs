/// Fortran scope types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum ScopeType {
    // Core Fortran scopes (0-12)
    If = 0,
    Do = 1,
    Select = 2, // SELECT CASE/RANK/TYPE
    Subroutine = 3,
    Function = 4,
    Interface = 5,
    Type = 6,
    Enum = 7,
    Associate = 8,
    EndAny = 9, // Generic END
    Block = 10,
    Where = 11,
    Forall = 12,

    // Module scopes (13-15, optional based on indent_mod flag)
    Module = 13,
    Submodule = 14,
    Program = 15,

    // Fypp preprocessor scopes (16-21, optional based on fypp flag)
    FyppDef = 16,
    FyppIf = 17,
    FyppFor = 18,
    FyppBlock = 19,
    FyppCall = 20,
    FyppMute = 21,
}

impl ScopeType {
    /// Get the scope type as a usize index
    #[must_use]
    pub fn as_index(self) -> usize {
        self as usize
    }

    /// Create from index (used when popping from stack)
    #[must_use]
    pub fn from_index(idx: usize) -> Option<Self> {
        use ScopeType::{
            Associate, Block, Do, EndAny, Enum, Forall, Function, FyppBlock, FyppCall, FyppDef,
            FyppFor, FyppIf, FyppMute, If, Interface, Module, Program, Select, Submodule,
            Subroutine, Type, Where,
        };
        const ALL: [ScopeType; 22] = [
            If, Do, Select, Subroutine, Function, Interface, Type, Enum, Associate, EndAny, Block,
            Where, Forall, Module, Submodule, Program, FyppDef, FyppIf, FyppFor, FyppBlock,
            FyppCall, FyppMute,
        ];
        ALL.get(idx).copied()
    }

    /// Check if this is a fypp preprocessor scope
    #[must_use]
    pub fn is_fypp_scope(self) -> bool {
        matches!(
            self,
            ScopeType::FyppDef
                | ScopeType::FyppIf
                | ScopeType::FyppFor
                | ScopeType::FyppBlock
                | ScopeType::FyppCall
                | ScopeType::FyppMute
        )
    }

    /// Check if a scope index represents a fypp scope
    ///
    /// This is useful when working with raw indices instead of `ScopeType` values.
    /// Fypp scopes are indices 16-21 (`FyppDef` through `FyppMute`).
    #[must_use]
    pub fn is_fypp_scope_index(idx: usize) -> bool {
        idx >= ScopeType::FyppDef.as_index() && idx <= ScopeType::FyppMute.as_index()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_indices() {
        assert_eq!(ScopeType::If.as_index(), 0);
        assert_eq!(ScopeType::Do.as_index(), 1);
        assert_eq!(ScopeType::Module.as_index(), 13);
        assert_eq!(ScopeType::FyppIf.as_index(), 17);
    }

    #[test]
    fn test_from_index() {
        assert_eq!(ScopeType::from_index(0), Some(ScopeType::If));
        assert_eq!(ScopeType::from_index(3), Some(ScopeType::Subroutine));
        assert_eq!(ScopeType::from_index(99), None);
    }

    #[test]
    fn test_is_fypp_scope() {
        assert!(ScopeType::FyppIf.is_fypp_scope());
        assert!(!ScopeType::If.is_fypp_scope());
    }

    #[test]
    fn test_is_fypp_scope_index() {
        // Fypp scopes are indices 16-21
        assert!(!ScopeType::is_fypp_scope_index(15)); // Program
        assert!(ScopeType::is_fypp_scope_index(16)); // FyppDef
        assert!(ScopeType::is_fypp_scope_index(17)); // FyppIf
        assert!(ScopeType::is_fypp_scope_index(21)); // FyppMute
        assert!(!ScopeType::is_fypp_scope_index(22)); // Out of range
        assert!(!ScopeType::is_fypp_scope_index(0)); // If
    }
}
