/// Scope types for Fortran constructs
use std::fmt;

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
        match idx {
            0 => Some(ScopeType::If),
            1 => Some(ScopeType::Do),
            2 => Some(ScopeType::Select),
            3 => Some(ScopeType::Subroutine),
            4 => Some(ScopeType::Function),
            5 => Some(ScopeType::Interface),
            6 => Some(ScopeType::Type),
            7 => Some(ScopeType::Enum),
            8 => Some(ScopeType::Associate),
            9 => Some(ScopeType::EndAny),
            10 => Some(ScopeType::Block),
            11 => Some(ScopeType::Where),
            12 => Some(ScopeType::Forall),
            13 => Some(ScopeType::Module),
            14 => Some(ScopeType::Submodule),
            15 => Some(ScopeType::Program),
            16 => Some(ScopeType::FyppDef),
            17 => Some(ScopeType::FyppIf),
            18 => Some(ScopeType::FyppFor),
            19 => Some(ScopeType::FyppBlock),
            20 => Some(ScopeType::FyppCall),
            21 => Some(ScopeType::FyppMute),
            _ => None,
        }
    }

    /// Check if this is a module-type scope
    #[must_use]
    pub fn is_module_scope(self) -> bool {
        matches!(
            self,
            ScopeType::Module | ScopeType::Submodule | ScopeType::Program
        )
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

impl fmt::Display for ScopeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            ScopeType::If => "IF",
            ScopeType::Do => "DO",
            ScopeType::Select => "SELECT",
            ScopeType::Subroutine => "SUBROUTINE",
            ScopeType::Function => "FUNCTION",
            ScopeType::Interface => "INTERFACE",
            ScopeType::Type => "TYPE",
            ScopeType::Enum => "ENUM",
            ScopeType::Associate => "ASSOCIATE",
            ScopeType::EndAny => "END",
            ScopeType::Block => "BLOCK",
            ScopeType::Where => "WHERE",
            ScopeType::Forall => "FORALL",
            ScopeType::Module => "MODULE",
            ScopeType::Submodule => "SUBMODULE",
            ScopeType::Program => "PROGRAM",
            ScopeType::FyppDef => "FYPP_DEF",
            ScopeType::FyppIf => "FYPP_IF",
            ScopeType::FyppFor => "FYPP_FOR",
            ScopeType::FyppBlock => "FYPP_BLOCK",
            ScopeType::FyppCall => "FYPP_CALL",
            ScopeType::FyppMute => "FYPP_MUTE",
        };
        write!(f, "{name}")
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
    fn test_is_module_scope() {
        assert!(ScopeType::Module.is_module_scope());
        assert!(ScopeType::Program.is_module_scope());
        assert!(!ScopeType::If.is_module_scope());
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

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", ScopeType::If), "IF");
        assert_eq!(format!("{}", ScopeType::Subroutine), "SUBROUTINE");
    }
}
