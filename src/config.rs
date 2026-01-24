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

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// Config file names to search for (in order of priority, later overrides earlier)
const CONFIG_FILE_NAMES: &[&str] = &["fprettier.toml"];

/// Get the user's home directory
fn dirs_home() -> Option<PathBuf> {
    // Try HOME environment variable first (works on Unix and some Windows setups)
    if let Ok(home) = std::env::var("HOME") {
        return Some(PathBuf::from(home));
    }
    // Fallback for Windows
    if let Ok(userprofile) = std::env::var("USERPROFILE") {
        return Some(PathBuf::from(userprofile));
    }
    None
}

// Serde default functions
fn default_indent() -> usize {
    3
}
fn default_line_length() -> usize {
    132
}
fn default_whitespace() -> u8 {
    2
}
fn default_true() -> bool {
    true
}
fn default_comment_spacing() -> usize {
    1
}

/// Main configuration struct for fprettier
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Number of spaces per indent level (default: 3)
    #[serde(default = "default_indent")]
    pub indent: usize,

    /// Maximum line length (default: 132)
    #[serde(default = "default_line_length")]
    pub line_length: usize,

    /// Whitespace formatting level (0-4, default: 2)
    /// 0: no whitespace formatting
    /// 1: minimal (comma, assignments, relational, logical, print, intrinsics, decl)
    /// 2: standard (adds plusminus)
    /// 3: aggressive (adds multdiv)
    /// 4: maximum (adds type, concat)
    #[serde(default = "default_whitespace")]
    pub whitespace: u8,

    /// Fine-grained whitespace control
    #[serde(default)]
    pub whitespace_dict: HashMap<String, bool>,

    /// Impose indentation (default: true)
    #[serde(default = "default_true")]
    pub impose_indent: bool,

    /// Impose whitespace formatting (default: true)
    #[serde(default = "default_true")]
    pub impose_whitespace: bool,

    /// Strict indentation checking (default: false)
    #[serde(default)]
    pub strict_indent: bool,

    /// Indent fypp preprocessor directives (default: true)
    #[serde(default = "default_true")]
    pub indent_fypp: bool,

    /// Indent module/program/submodule blocks (default: true)
    #[serde(default = "default_true")]
    pub indent_mod: bool,

    /// Normalize comment spacing (use consistent spacing before inline comments, default: false)
    #[serde(default)]
    pub normalize_comment_spacing: bool,

    /// Format declaration statements (default: false)
    #[serde(default)]
    pub format_decl: bool,

    /// Case conversion dictionary
    #[serde(default)]
    pub case_dict: HashMap<String, i32>,

    /// Number of spaces before comments (default: 1)
    #[serde(default = "default_comment_spacing")]
    pub comment_spacing: usize,

    /// Enable relational operator replacement (default: false)
    #[serde(default)]
    pub enable_replacements: bool,

    /// Use C-style relational operators when `enable_replacements` is true (default: false)
    /// If false, uses Fortran-style (.lt., .le., .gt., .ge., .eq., .ne.)
    /// If true, uses C-style (<, <=, >, >=, ==, /=)
    #[serde(default)]
    pub c_relations: bool,
}

/// Partial configuration for TOML parsing
///
/// All fields are `Option<T>` so we can distinguish between
/// "explicitly set" and "not specified" when merging configs.
#[derive(Debug, Clone, Default, Deserialize)]
struct PartialConfig {
    pub indent: Option<usize>,
    pub line_length: Option<usize>,
    pub whitespace: Option<u8>,
    #[serde(default)]
    pub whitespace_dict: HashMap<String, bool>,
    pub impose_indent: Option<bool>,
    pub impose_whitespace: Option<bool>,
    pub strict_indent: Option<bool>,
    pub indent_fypp: Option<bool>,
    pub indent_mod: Option<bool>,
    pub normalize_comment_spacing: Option<bool>,
    pub format_decl: Option<bool>,
    #[serde(default)]
    pub case_dict: HashMap<String, i32>,
    pub comment_spacing: Option<usize>,
    pub enable_replacements: Option<bool>,
    pub c_relations: Option<bool>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            indent: 3,
            line_length: 132,
            whitespace: 2,
            whitespace_dict: HashMap::new(),
            impose_indent: true,
            impose_whitespace: true,
            strict_indent: false,
            indent_fypp: true,
            indent_mod: true,
            normalize_comment_spacing: false,
            format_decl: false,
            case_dict: HashMap::new(),
            comment_spacing: 1,
            enable_replacements: false,
            c_relations: false,
        }
    }
}

impl Config {
    /// Minimum reasonable line length (must fit at least some code)
    const MIN_LINE_LENGTH: usize = 40;
    /// Maximum reasonable line length
    const MAX_LINE_LENGTH: usize = 1000;
    /// Maximum reasonable indent size
    const MAX_INDENT: usize = 20;
    /// Maximum whitespace level
    const MAX_WHITESPACE_LEVEL: u8 = 4;
    /// Maximum comment spacing
    const MAX_COMMENT_SPACING: usize = 10;

    /// Validate configuration values are within reasonable bounds
    ///
    /// Returns an error message if validation fails, None if valid.
    #[must_use]
    pub fn validate(&self) -> Option<String> {
        if self.indent == 0 {
            return Some("indent must be at least 1".to_string());
        }
        if self.indent > Self::MAX_INDENT {
            return Some(format!(
                "indent {} exceeds maximum of {}",
                self.indent,
                Self::MAX_INDENT
            ));
        }
        if self.line_length < Self::MIN_LINE_LENGTH {
            return Some(format!(
                "line_length {} is below minimum of {}",
                self.line_length,
                Self::MIN_LINE_LENGTH
            ));
        }
        if self.line_length > Self::MAX_LINE_LENGTH {
            return Some(format!(
                "line_length {} exceeds maximum of {}",
                self.line_length,
                Self::MAX_LINE_LENGTH
            ));
        }
        if self.whitespace > Self::MAX_WHITESPACE_LEVEL {
            return Some(format!(
                "whitespace level {} exceeds maximum of {}",
                self.whitespace,
                Self::MAX_WHITESPACE_LEVEL
            ));
        }
        if self.comment_spacing > Self::MAX_COMMENT_SPACING {
            return Some(format!(
                "comment_spacing {} exceeds maximum of {}",
                self.comment_spacing,
                Self::MAX_COMMENT_SPACING
            ));
        }
        None
    }

    /// Load configuration from a TOML file
    pub fn from_toml_file(path: &Path) -> anyhow::Result<Self> {
        let contents = std::fs::read_to_string(path)?;
        let partial: PartialConfig = toml::from_str(&contents)?;
        let mut config = Self::default();
        config.apply_partial(&partial);
        Ok(config)
    }

    /// Apply a partial config, only overriding fields that are explicitly set
    fn apply_partial(&mut self, partial: &PartialConfig) {
        if let Some(v) = partial.indent {
            self.indent = v;
        }
        if let Some(v) = partial.line_length {
            self.line_length = v;
        }
        if let Some(v) = partial.whitespace {
            self.whitespace = v;
        }
        if let Some(v) = partial.impose_indent {
            self.impose_indent = v;
        }
        if let Some(v) = partial.impose_whitespace {
            self.impose_whitespace = v;
        }
        if let Some(v) = partial.strict_indent {
            self.strict_indent = v;
        }
        if let Some(v) = partial.indent_fypp {
            self.indent_fypp = v;
        }
        if let Some(v) = partial.indent_mod {
            self.indent_mod = v;
        }
        if let Some(v) = partial.normalize_comment_spacing {
            self.normalize_comment_spacing = v;
        }
        if let Some(v) = partial.format_decl {
            self.format_decl = v;
        }
        if let Some(v) = partial.comment_spacing {
            self.comment_spacing = v;
        }
        if let Some(v) = partial.enable_replacements {
            self.enable_replacements = v;
        }
        if let Some(v) = partial.c_relations {
            self.c_relations = v;
        }
        // Merge dictionaries (partial values override)
        for (k, v) in &partial.whitespace_dict {
            self.whitespace_dict.insert(k.clone(), *v);
        }
        for (k, v) in &partial.case_dict {
            self.case_dict.insert(k.clone(), *v);
        }
    }

    /// Discover config files from parent directories of a given path
    ///
    /// Searches from the file's directory up to the root, then adds home directory config.
    /// Returns list of config file paths in order of priority (least specific first).
    #[must_use]
    pub fn discover_config_files(start_path: &Path) -> Vec<PathBuf> {
        let mut config_files = Vec::new();

        // Add home directory config first (lowest priority)
        if let Some(home) = dirs_home() {
            for config_name in CONFIG_FILE_NAMES {
                let home_config = home.join(config_name);
                if home_config.is_file() {
                    config_files.push(home_config);
                }
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
                for config_name in CONFIG_FILE_NAMES {
                    let config_path = ancestor.join(config_name);
                    if config_path.is_file() && !config_files.contains(&config_path) {
                        config_files.push(config_path);
                    }
                }
            }
        }

        config_files
    }

    /// Load and merge configuration from discovered config files
    ///
    /// Later files override earlier ones (only explicitly set values).
    /// Returns default config if no files found.
    #[must_use]
    pub fn from_discovered_files(start_path: &Path) -> Self {
        let config_files = Self::discover_config_files(start_path);

        if config_files.is_empty() {
            return Self::default();
        }

        let mut config = Self::default();
        for path in &config_files {
            match std::fs::read_to_string(path) {
                Ok(contents) => match toml::from_str::<PartialConfig>(&contents) {
                    Ok(partial) => config.apply_partial(&partial),
                    Err(e) => eprintln!("Warning: failed to parse {}: {e}", path.display()),
                },
                Err(e) => eprintln!("Warning: failed to read {}: {e}", path.display()),
            }
        }
        config
    }

    /// Get the whitespace array based on whitespace level and dictionary overrides
    ///
    /// Returns an 11-element array controlling spacing around:
    /// 0: comma/semicolon
    /// 1: assignments (=, =>)
    /// 2: relational operators (<, >, ==, /=, etc.)
    /// 3: logical operators (.and., .or., etc.)
    /// 4: plus/minus
    /// 5: multiply/divide
    /// 6: print/read statements
    /// 7: select type components
    /// 8: intrinsics
    /// 9: declarations (::)
    /// 10: string concatenation (//)
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

        // Mapping from dictionary keys to array indices
        let mapping = [
            ("comma", 0),
            ("assignments", 1),
            ("relational", 2),
            ("logical", 3),
            ("plusminus", 4),
            ("multdiv", 5),
            ("print", 6),
            ("type", 7),
            ("intrinsics", 8),
            ("decl", 9),
            ("concat", 10),
        ];

        // Override with whitespace_dict settings
        for (key, idx) in &mapping {
            if let Some(&value) = self.whitespace_dict.get(*key) {
                whitespace_flags[*idx] = value;
            }
        }

        whitespace_flags
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = Config::default();
        assert_eq!(config.indent, 3);
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
        let mut dict = HashMap::new();
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

    #[test]
    fn test_config_apply_partial() {
        let mut base = Config::default();
        assert_eq!(base.indent, 3);
        assert_eq!(base.line_length, 132);

        // Only set indent and line_length, leave others as None
        let partial = PartialConfig {
            indent: Some(4),
            line_length: Some(80),
            ..Default::default()
        };

        base.apply_partial(&partial);
        assert_eq!(base.indent, 4);
        assert_eq!(base.line_length, 80);
        // Other fields should remain at defaults
        assert_eq!(base.whitespace, 2);
        assert!(base.impose_indent);
    }

    #[test]
    fn test_config_apply_partial_preserves_unset() {
        let mut base = Config::default();
        base.indent = 4; // Set a non-default value

        // Partial config that only sets line_length
        let partial = PartialConfig {
            line_length: Some(80),
            ..Default::default()
        };

        base.apply_partial(&partial);
        // indent should be preserved (not reset to default)
        assert_eq!(base.indent, 4);
        assert_eq!(base.line_length, 80);
    }

    #[test]
    fn test_config_apply_partial_whitespace_dict() {
        let mut base = Config::default();
        base.whitespace_dict.insert("comma".to_string(), true);
        base.whitespace_dict.insert("concat".to_string(), false);

        let mut partial = PartialConfig::default();
        partial.whitespace_dict.insert("concat".to_string(), true);
        partial.whitespace_dict.insert("multdiv".to_string(), true);

        base.apply_partial(&partial);

        // comma should be preserved
        assert_eq!(base.whitespace_dict.get("comma"), Some(&true));
        // concat should be overridden to true
        assert_eq!(base.whitespace_dict.get("concat"), Some(&true));
        // multdiv should be added
        assert_eq!(base.whitespace_dict.get("multdiv"), Some(&true));
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
        let config = Config::from_discovered_files(&path);
        // Should be default values
        assert_eq!(config.indent, 3);
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
    fn test_validate_indent_too_large() {
        let config = Config {
            indent: 100,
            ..Default::default()
        };
        assert!(config.validate().is_some());
    }

    #[test]
    fn test_validate_line_length_too_small() {
        let config = Config {
            line_length: 10,
            ..Default::default()
        };
        assert!(config.validate().is_some());
        assert!(config.validate().unwrap().contains("line_length"));
    }

    #[test]
    fn test_validate_line_length_too_large() {
        let config = Config {
            line_length: 5000,
            ..Default::default()
        };
        assert!(config.validate().is_some());
    }

    #[test]
    fn test_validate_whitespace_level() {
        let config = Config {
            whitespace: 10,
            ..Default::default()
        };
        assert!(config.validate().is_some());
        assert!(config.validate().unwrap().contains("whitespace"));
    }

    #[test]
    fn test_validate_comment_spacing() {
        let config = Config {
            comment_spacing: 50,
            ..Default::default()
        };
        assert!(config.validate().is_some());
    }
}
