//! Configuration management for fprettier.
//!
//! This module provides the [`Config`] struct which controls all formatting behavior.
//! Configuration can be loaded from:
//! - TOML files (`fprettier.toml`)
//! - CLI arguments (which override file settings)
//! - In-file directives (`! fprettier: --indent 4`)
//!
//! Config files are auto-discovered by searching parent directories from the file
//! being formatted up to the filesystem root, plus the user's home directory.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use anyhow::Context;
use serde::Deserialize;
use toml::{Table, Value};

use crate::format::case_convert::CaseMode;

/// Config file name to search for
const CONFIG_FILE_NAME: &str = "fprettier.toml";

/// Longest line free source form permits (F2023 6.3.2.1). Asking for more
/// would have fprettier write lines no conforming processor need accept.
pub const MAX_LINE_LENGTH: usize = 10_000;

/// Longest statement free source form permits, counting every continuation
/// line but not the `&`s (F2023 6.3.2.6).
pub const MAX_STATEMENT_LENGTH: usize = 1_000_000;

/// Indices into the array [`Config::get_whitespace_flags`] returns.
/// [`WS_FLAGS`] pairs each with the name it goes by elsewhere.
/// Spacing around comma/semicolon
pub const WS_COMMA: usize = 0;
/// Spacing around assignments (=, =>)
pub const WS_ASSIGNMENT: usize = 1;
/// Spacing around relational operators (<, >, ==, /=, etc.)
pub const WS_RELATIONAL: usize = 2;
/// Spacing around logical operators (.and., .or., etc.)
pub const WS_LOGICAL: usize = 3;
/// Spacing around plus/minus
pub const WS_PLUSMINUS: usize = 4;
/// Spacing around multiply/divide
pub const WS_MULTDIV: usize = 5;
/// Spacing around print/read statements
pub const WS_PRINT: usize = 6;
/// Spacing around select type components
pub const WS_TYPE: usize = 7;
/// Spacing around intrinsics
pub const WS_INTRINSICS: usize = 8;
/// Spacing around declarations (::)
pub const WS_DECL: usize = 9;
/// Spacing around string concatenation (//)
pub const WS_CONCAT: usize = 10;

/// Every fine-grained whitespace option, in the order `--help` lists them:
/// its `whitespace_dict` key, the flag-array index it sets, and what it
/// controls. The CLI flag is the key with a `whitespace-` prefix.
///
/// One row per option, so a flag, its config-file key and the behavior it
/// reaches cannot drift apart.
pub const WS_FLAGS: [(&str, usize, &str); 11] = [
    (
        "comma",
        WS_COMMA,
        "Enable/disable spacing after commas and semicolons",
    ),
    (
        "assignments",
        WS_ASSIGNMENT,
        "Enable/disable spacing around assignment operators (=, =>)",
    ),
    (
        "decl",
        WS_DECL,
        "Enable/disable spacing around declaration operator (::)",
    ),
    (
        "relational",
        WS_RELATIONAL,
        "Enable/disable spacing around relational operators (<, >, ==, /=, .eq., etc.)",
    ),
    (
        "logical",
        WS_LOGICAL,
        "Enable/disable spacing around logical operators (.and., .or., etc.)",
    ),
    (
        "plusminus",
        WS_PLUSMINUS,
        "Enable/disable spacing around plus/minus operators",
    ),
    (
        "multdiv",
        WS_MULTDIV,
        "Enable/disable spacing around multiply/divide operators",
    ),
    (
        "print",
        WS_PRINT,
        "Enable/disable spacing in print/read statements",
    ),
    (
        "type",
        WS_TYPE,
        "Enable/disable spacing around type selector (%)",
    ),
    (
        "intrinsics",
        WS_INTRINSICS,
        "Enable/disable spacing before intrinsic function parentheses",
    ),
    (
        "concat",
        WS_CONCAT,
        "Enable/disable spacing around string concatenation operator (//)",
    ),
];

/// Main configuration struct for fprettier
///
/// Deserialized straight from a config file's TOML table. Every field falls
/// back to its [`Default`] value, so a file need only name what it changes.
#[derive(Debug, Clone, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct Config {
    /// Number of spaces per indent level (default: 4)
    pub indent: usize,

    /// Maximum line length (default: 132)
    pub line_length: usize,

    /// Whitespace formatting level (0-4, default: 2)
    /// 0: no whitespace formatting
    /// 1: minimal (comma, assignments, relational, logical, print, intrinsics, decl)
    /// 2: standard (adds plusminus)
    /// 3: aggressive (adds multdiv)
    /// 4: maximum (adds type, concat)
    pub whitespace: u8,

    /// Fine-grained whitespace control
    pub whitespace_dict: BTreeMap<String, bool>,

    /// Impose indentation (default: true)
    pub impose_indent: bool,

    /// Impose whitespace formatting (default: true)
    pub impose_whitespace: bool,

    /// Strict indentation checking (default: false)
    pub strict_indent: bool,

    /// Indent fypp preprocessor directives (default: true)
    pub indent_fypp: bool,

    /// Indent module/program/submodule blocks (default: true)
    pub indent_mod: bool,

    /// Normalize comment spacing (use consistent spacing before inline comments, default: false)
    pub normalize_comment_spacing: bool,

    /// Format declaration statements (default: false)
    pub format_decl: bool,

    /// Case conversion dictionary
    pub case_dict: BTreeMap<String, CaseMode>,

    /// Number of spaces before comments (default: 1)
    pub comment_spacing: usize,

    /// Enable relational operator replacement (default: false)
    pub enable_replacements: bool,

    /// Use C-style relational operators when `enable_replacements` is true (default: false)
    /// If false, uses Fortran-style (.lt., .le., .gt., .ge., .eq., .ne.)
    /// If true, uses C-style (<, <=, >, >=, ==, /=)
    pub c_relations: bool,

    /// Sort `use` statements alphabetically within a group (default: false)
    pub sort_use: bool,

    /// Sort the names in a `use ... only:` list alphabetically (default: false)
    pub sort_use_only: bool,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            indent: 4,
            line_length: 132,
            whitespace: 2,
            whitespace_dict: BTreeMap::new(),
            impose_indent: true,
            impose_whitespace: true,
            strict_indent: false,
            indent_fypp: true,
            indent_mod: true,
            normalize_comment_spacing: false,
            format_decl: false,
            case_dict: BTreeMap::new(),
            comment_spacing: 1,
            enable_replacements: false,
            c_relations: false,
            sort_use: false,
            sort_use_only: false,
        }
    }
}

impl Config {
    /// Validate configuration values, returning an error message if invalid.
    ///
    /// Only `indent` and `line_length` are checked. Out-of-range values
    /// elsewhere degrade harmlessly: `whitespace > 4` falls through to the
    /// level-2 defaults, and a `line_length` under 40 short-circuits in
    /// `auto_split_line`.
    #[must_use]
    pub fn validate(&self) -> Option<String> {
        if self.indent == 0 {
            return Some("indent must be at least 1".to_string());
        }
        if self.line_length > MAX_LINE_LENGTH {
            return Some(format!("line length must be at most {MAX_LINE_LENGTH}"));
        }
        None
    }

    /// Load configuration from a TOML file
    pub fn from_toml_file(path: &Path) -> anyhow::Result<Self> {
        let contents = std::fs::read_to_string(path)?;
        Ok(toml::from_str(&contents)?)
    }

    /// Discover config files from parent directories of a given path
    ///
    /// Searches from the file's directory up to the root, then adds home directory config.
    /// Returns list of config file paths in order of priority (least specific first).
    #[must_use]
    pub fn discover_config_files(start_path: &Path) -> Vec<PathBuf> {
        let mut config_files = Vec::new();

        // Add home directory config first (lowest priority)
        if let Some(home) = std::env::home_dir() {
            let home_config = home.join(CONFIG_FILE_NAME);
            if home_config.is_file() {
                config_files.push(home_config);
            }
        }

        // Start from the file's parent directory (or the path itself if it's a directory)
        let start_dir = if start_path.is_file() {
            start_path.parent().map(Path::to_path_buf)
        } else if start_path.is_dir() {
            Some(start_path.to_path_buf())
        } else {
            // Path doesn't exist, use current directory
            std::env::current_dir().ok()
        };

        // Collect config files from parent directories (from root to current)
        if let Some(dir) = start_dir {
            let mut ancestors: Vec<PathBuf> = dir.ancestors().map(Path::to_path_buf).collect();
            // Reverse so we go from root to current (less specific to more specific)
            ancestors.reverse();

            for ancestor in ancestors {
                let config_path = ancestor.join(CONFIG_FILE_NAME);
                if config_path.is_file() && !config_files.contains(&config_path) {
                    config_files.push(config_path);
                }
            }
        }

        config_files
    }

    /// Load and merge configuration from discovered config files
    ///
    /// Later files override earlier ones (only explicitly set values).
    /// Returns default config if no files found. A config file that exists but
    /// cannot be read or parsed is an error: carrying on with the defaults
    /// would silently reformat the tree with settings nobody asked for.
    pub fn from_discovered_files(start_path: &Path) -> anyhow::Result<Self> {
        let mut merged = Table::new();
        let mut config = Self::default();
        for path in &Self::discover_config_files(start_path) {
            let contents = std::fs::read_to_string(path)
                .with_context(|| format!("failed to read {}", path.display()))?;
            let table: Table = toml::from_str(&contents)
                .with_context(|| format!("failed to parse {}", path.display()))?;
            merge_tables(&mut merged, table);
            // Deserialized once per file rather than once at the end, so an
            // unknown key or a bad value is reported against the file that
            // introduced it. Everything merged so far already deserialized.
            config = Value::Table(merged.clone())
                .try_into()
                .with_context(|| format!("failed to parse {}", path.display()))?;
        }
        Ok(config)
    }

    /// Get the whitespace array based on whitespace level and dictionary overrides
    ///
    /// Index it with the `WS_*` constants; [`WS_FLAGS`] names each position.
    #[must_use]
    pub fn get_whitespace_flags(&self) -> [bool; 11] {
        // Base array for each whitespace level
        let mut whitespace_flags = match self.whitespace {
            0 => [false; 11],
            1 => [
                true, true, true, true, false, false, true, false, true, true, false,
            ],
            3 => [
                true, true, true, true, true, true, true, false, true, true, false,
            ],
            4 => [true; 11],
            // 2 is the default
            _ => [
                true, true, true, true, true, false, true, false, true, true, false,
            ],
        };

        // Override with whitespace_dict settings
        for (key, idx, _) in WS_FLAGS {
            if let Some(&value) = self.whitespace_dict.get(key) {
                whitespace_flags[idx] = value;
            }
        }

        whitespace_flags
    }
}

/// Overlay `overlay` onto `base`, recursing into nested tables so that a
/// closer config file adds to `whitespace_dict`/`case_dict` rather than
/// replacing whatever a parent directory put there.
fn merge_tables(base: &mut Table, overlay: Table) {
    for (key, value) in overlay {
        match (base.get_mut(&key), value) {
            (Some(Value::Table(existing)), Value::Table(new)) => merge_tables(existing, new),
            (_, value) => {
                base.insert(key, value);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert_eq!(config.indent, 4);
        assert_eq!(config.line_length, 132);
        assert_eq!(config.whitespace, 2);
        assert!(config.impose_indent);
        assert!(config.impose_whitespace);
    }

    #[test]
    fn test_whitespace_flags_array_level_0() {
        let config = Config {
            whitespace: 0,
            ..Default::default()
        };
        let whitespace_flags = config.get_whitespace_flags();
        assert_eq!(whitespace_flags, [false; 11]);
    }

    #[test]
    fn test_whitespace_flags_array_level_2() {
        let config = Config {
            whitespace: 2,
            ..Default::default()
        };
        let whitespace_flags = config.get_whitespace_flags();
        assert_eq!(
            whitespace_flags,
            [true, true, true, true, true, false, true, false, true, true, false]
        );
    }

    #[test]
    fn test_whitespace_flags_array_level_4() {
        let config = Config {
            whitespace: 4,
            ..Default::default()
        };
        let whitespace_flags = config.get_whitespace_flags();
        assert_eq!(whitespace_flags, [true; 11]);
    }

    #[test]
    fn test_whitespace_dict_override() {
        let mut dict = BTreeMap::new();
        dict.insert("plusminus".to_string(), false);
        dict.insert("multdiv".to_string(), true);

        let config = Config {
            whitespace: 2,
            whitespace_dict: dict,
            ..Default::default()
        };

        let whitespace_flags = config.get_whitespace_flags();
        // Level 2 normally has: [T T T T T F T F T T F]
        // Override plusminus (idx 4) to false, multdiv (idx 5) to true
        assert_eq!(
            whitespace_flags,
            [true, true, true, true, false, true, true, false, true, true, false]
        );
    }

    /// Merge TOML sources the way `from_discovered_files` does: least
    /// specific first, then deserialize the result.
    fn merge_all(sources: &[&str]) -> Config {
        let mut merged = Table::new();
        for src in sources {
            merge_tables(&mut merged, toml::from_str(src).unwrap());
        }
        Value::Table(merged).try_into().unwrap()
    }

    #[test]
    fn test_unset_keys_keep_their_defaults() {
        let config = merge_all(&["indent = 2\nline_length = 80\n"]);
        assert_eq!(config.indent, 2);
        assert_eq!(config.line_length, 80);
        // Everything the file did not name
        assert_eq!(config.whitespace, 2);
        assert!(config.impose_indent);
    }

    #[test]
    fn test_later_file_preserves_keys_it_does_not_set() {
        let config = merge_all(&["indent = 2\n", "line_length = 80\n"]);
        // The closer file said nothing about indent, so the outer one stands
        assert_eq!(config.indent, 2);
        assert_eq!(config.line_length, 80);
    }

    #[test]
    fn test_dicts_merge_key_by_key() {
        let config = merge_all(&[
            "[whitespace_dict]\ncomma = true\nconcat = false\n",
            "[whitespace_dict]\nconcat = true\nmultdiv = true\n",
        ]);
        // Untouched by the closer file
        assert_eq!(config.whitespace_dict.get("comma"), Some(&true));
        // Overridden, and added
        assert_eq!(config.whitespace_dict.get("concat"), Some(&true));
        assert_eq!(config.whitespace_dict.get("multdiv"), Some(&true));
    }

    #[test]
    fn test_unknown_key_is_rejected() {
        let table: Table = toml::from_str("indnet = 2\n").unwrap();
        assert!(Value::Table(table).try_into::<Config>().is_err());
    }

    #[test]
    fn test_discover_config_files_nonexistent_path() {
        // Discovery from a path that doesn't exist should return empty or use current dir
        let path = PathBuf::from("/nonexistent/path/file.f90");
        let files = Config::discover_config_files(&path);
        // Should not panic, just return empty or configs from current directory
        assert!(files.is_empty() || !files.is_empty()); // Just checking it doesn't panic
    }

    #[test]
    fn test_from_discovered_files_returns_default_when_empty() {
        // When no config files exist, should return default config
        let path = PathBuf::from("/nonexistent/unique/path/file.f90");
        let config = Config::from_discovered_files(&path).unwrap();
        // Should be default values
        assert_eq!(config.indent, 4);
        assert_eq!(config.line_length, 132);
        assert_eq!(config.whitespace, 2);
    }

    #[test]
    fn test_validate_default_config() {
        let config = Config::default();
        assert!(
            config.validate().is_none(),
            "Default config should be valid"
        );
    }

    #[test]
    fn test_validate_indent_zero() {
        let config = Config {
            indent: 0,
            ..Default::default()
        };
        assert!(config.validate().is_some());
        assert!(config.validate().unwrap().contains("indent"));
    }

    #[test]
    fn test_validate_line_length() {
        let at_limit = Config {
            line_length: MAX_LINE_LENGTH,
            ..Default::default()
        };
        assert!(at_limit.validate().is_none());

        let over = Config {
            line_length: MAX_LINE_LENGTH + 1,
            ..Default::default()
        };
        assert!(over.validate().unwrap().contains("line length"));
    }
}
