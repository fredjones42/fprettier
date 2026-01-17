//! Command-line interface for fprettier.
//!
//! Defines CLI arguments using clap builder API

use std::path::PathBuf;

use clap::{Arg, ArgAction, Command};

/// CLI arguments parsed from command line
#[derive(Debug, Clone)]
pub struct CliArgs {
    /// Files or directories to format
    pub inputs: Vec<PathBuf>,

    /// Number of spaces per indent level
    pub indent: Option<usize>,

    /// Maximum line length
    pub line_length: Option<usize>,

    /// Whitespace formatting level (0-4)
    pub whitespace: Option<u8>,

    /// Fine-grained whitespace: comma/semicolon spacing
    pub whitespace_comma: Option<bool>,

    /// Fine-grained whitespace: assignment operator spacing (=, =>)
    pub whitespace_assignment: Option<bool>,

    /// Fine-grained whitespace: declaration spacing (::)
    pub whitespace_decl: Option<bool>,

    /// Fine-grained whitespace: relational operator spacing (<, >, ==, etc.)
    pub whitespace_relational: Option<bool>,

    /// Fine-grained whitespace: logical operator spacing (.and., .or., etc.)
    pub whitespace_logical: Option<bool>,

    /// Fine-grained whitespace: plus/minus spacing
    pub whitespace_plusminus: Option<bool>,

    /// Fine-grained whitespace: multiply/divide spacing
    pub whitespace_multdiv: Option<bool>,

    /// Fine-grained whitespace: print/read statement spacing
    pub whitespace_print: Option<bool>,

    /// Fine-grained whitespace: type selector (%) spacing
    pub whitespace_type: Option<bool>,

    /// Fine-grained whitespace: intrinsic function spacing
    pub whitespace_intrinsics: Option<bool>,

    /// Fine-grained whitespace: string concatenation (//) spacing
    pub whitespace_concat: Option<bool>,

    /// Disable indentation
    pub no_indent: bool,

    /// Disable whitespace formatting
    pub no_whitespace: bool,

    /// Strict indentation checking
    pub strict_indent: bool,

    /// Don't indent fypp preprocessor
    pub no_indent_fypp: bool,

    /// Don't indent module/program blocks
    pub no_indent_mod: bool,

    /// Normalize comment spacing (use consistent spacing before inline comments)
    pub normalize_comment_spacing: bool,

    /// Format declaration statements
    pub format_decl: bool,

    /// Enable relational operator replacement (.lt. <-> <, etc.)
    pub enable_replacements: bool,

    /// Use C-style relational operators (<, <=, etc.) instead of Fortran-style (.lt., .le., etc.)
    pub c_relations: bool,

    /// Number of spaces before inline comments
    pub comment_spacing: Option<usize>,

    /// Output to stdout instead of in-place
    pub stdout: bool,

    /// Show diff without modifying files
    pub diff: bool,

    /// Config file path
    pub config: Option<PathBuf>,

    /// Recursive directory processing
    pub recursive: bool,

    /// Silent mode (no output)
    pub silent: bool,

    /// Case conversion settings [keywords, procedures, operators, constants]
    /// Each value: 0=no change, 1=lowercase, 2=uppercase
    pub case: Option<[i32; 4]>,

    /// Number of parallel jobs (0 = auto, 1 = sequential)
    pub jobs: Option<usize>,

    /// Exclude patterns for files/directories (glob patterns)
    pub exclude: Vec<String>,

    /// Custom Fortran file extensions (in addition to defaults)
    pub fortran_extensions: Vec<String>,

    /// Exclude files with more than this many lines
    pub exclude_max_lines: Option<usize>,

    /// Enable debug output
    pub debug: bool,
}

/// Build the clap Command for parsing CLI arguments
#[must_use]
pub fn build_cli() -> Command {
    Command::new("fprettier")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Fred Jones")
        .about("Auto-formatter for modern Fortran code (Fortran 90+)")
        .arg(
            Arg::new("inputs")
                .help("Files or directories to format")
                .value_name("FILE")
                .num_args(1..)
                .required(false)
                .value_parser(clap::value_parser!(PathBuf)),
        )
        .arg(
            Arg::new("indent")
                .short('i')
                .long("indent")
                .help("Number of spaces per indent level [default: 3]")
                .value_name("NUM")
                .value_parser(clap::value_parser!(usize)),
        )
        .arg(
            Arg::new("line-length")
                .short('l')
                .long("line-length")
                .help("Maximum line length [default: 132]")
                .value_name("NUM")
                .value_parser(clap::value_parser!(usize)),
        )
        .arg(
            Arg::new("whitespace")
                .short('w')
                .long("whitespace")
                .help("Whitespace level: 0=minimal, 1=+operators, 2=+plusminus, 3=+multdiv, 4=all [default: 2]")
                .value_name("NUM")
                .value_parser(clap::value_parser!(u8)),
        )
        // Fine-grained whitespace options
        .arg(
            Arg::new("whitespace-comma")
                .long("whitespace-comma")
                .help("Enable/disable spacing after commas and semicolons")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-assignment")
                .long("whitespace-assignment")
                .help("Enable/disable spacing around assignment operators (=, =>)")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-decl")
                .long("whitespace-decl")
                .help("Enable/disable spacing around declaration operator (::)")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-relational")
                .long("whitespace-relational")
                .help("Enable/disable spacing around relational operators (<, >, ==, /=, .eq., etc.)")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-logical")
                .long("whitespace-logical")
                .help("Enable/disable spacing around logical operators (.and., .or., etc.)")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-plusminus")
                .long("whitespace-plusminus")
                .help("Enable/disable spacing around plus/minus operators")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-multdiv")
                .long("whitespace-multdiv")
                .help("Enable/disable spacing around multiply/divide operators")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-print")
                .long("whitespace-print")
                .help("Enable/disable spacing in print/read statements")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-type")
                .long("whitespace-type")
                .help("Enable/disable spacing around type selector (%)")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-intrinsics")
                .long("whitespace-intrinsics")
                .help("Enable/disable spacing before intrinsic function parentheses")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("whitespace-concat")
                .long("whitespace-concat")
                .help("Enable/disable spacing around string concatenation operator (//)")
                .value_name("BOOL")
                .num_args(0..=1)
                .require_equals(true)
                .default_missing_value("true")
                .value_parser(clap::value_parser!(bool)),
        )
        .arg(
            Arg::new("no-indent")
                .long("no-indent")
                .help("Disable indentation")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-whitespace")
                .long("no-whitespace")
                .help("Disable whitespace formatting")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("strict-indent")
                .long("strict-indent")
                .help("Strict indentation checking")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-indent-fypp")
                .long("no-indent-fypp")
                .help("Don't indent fypp preprocessor directives")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-indent-mod")
                .long("no-indent-mod")
                .help("Don't indent module/program/submodule blocks")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("normalize-comment-spacing")
                .long("normalize-comment-spacing")
                .help("Normalize spacing before inline comments")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("format-decl")
                .long("format-decl")
                .help("Format declaration statements")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("enable-replacements")
                .long("enable-replacements")
                .help("Replace relational operators between Fortran (.lt., .eq., etc.) and C-style (<, ==, etc.)")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("c-relations")
                .long("c-relations")
                .help("Use C-style relational operators (<, <=, >, >=, ==, /=) when --enable-replacements is set")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("comment-spacing")
                .long("comment-spacing")
                .help("Number of spaces before inline comments [default: 1]")
                .value_name("NUM")
                .value_parser(clap::value_parser!(usize)),
        )
        .arg(
            Arg::new("stdout")
                .short('s')
                .long("stdout")
                .help("Output to stdout instead of modifying files in-place")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("diff")
                .short('d')
                .long("diff")
                .help("Show diff without modifying files")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("config")
                .short('c')
                .long("config")
                .help("Path to configuration file (overrides auto-discovery)")
                .value_name("FILE")
                .value_parser(clap::value_parser!(PathBuf)),
        )
        .arg(
            Arg::new("recursive")
                .short('r')
                .long("recursive")
                .help("Recursively format directories")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("exclude")
                .short('e')
                .long("exclude")
                .help("Exclude files/directories matching pattern (glob syntax, can be repeated)")
                .value_name("PATTERN")
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("fortran")
                .short('f')
                .long("fortran")
                .help("Additional Fortran file extension (can be repeated, e.g., -f f03 -f F03)")
                .value_name("EXT")
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("exclude-max-lines")
                .short('m')
                .long("exclude-max-lines")
                .help("Exclude files with more than this many lines")
                .value_name("NUM")
                .value_parser(clap::value_parser!(usize)),
        )
        .arg(
            Arg::new("debug")
                .short('D')
                .long("debug")
                .help("Enable debug output (shows config, scope changes, warnings)")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("silent")
                .short('S')
                .long("silent")
                .help("Silent mode (no output, for editor integration)")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("case")
                .long("case")
                .help("Enable case formatting: 4 values for keywords, procedures, operators, constants (0=none, 1=lower, 2=upper)")
                .value_name("NUM")
                .num_args(4)
                .value_parser(clap::value_parser!(i32)),
        )
        .arg(
            Arg::new("jobs")
                .short('j')
                .long("jobs")
                .help("Number of parallel jobs (0=auto, 1=sequential)")
                .value_name("NUM")
                .value_parser(clap::value_parser!(usize)),
        )
}

/// Parse CLI arguments from command line
#[must_use]
pub fn parse_args() -> CliArgs {
    args_from_matches(&build_cli().get_matches())
}

/// Parse CLI arguments from an iterator (for testing)
#[must_use]
pub fn parse_args_from<I, T>(args: I) -> CliArgs
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
{
    args_from_matches(&build_cli().get_matches_from(args))
}

/// Convert clap `ArgMatches` to `CliArgs`
fn args_from_matches(matches: &clap::ArgMatches) -> CliArgs {
    let case = matches.get_many::<i32>("case").map(|vals| {
        let v: Vec<i32> = vals.copied().collect();
        [v[0], v[1], v[2], v[3]]
    });

    CliArgs {
        inputs: matches
            .get_many::<PathBuf>("inputs")
            .map(|vals| vals.cloned().collect())
            .unwrap_or_default(),
        indent: matches.get_one::<usize>("indent").copied(),
        line_length: matches.get_one::<usize>("line-length").copied(),
        whitespace: matches.get_one::<u8>("whitespace").copied(),
        whitespace_comma: matches.get_one::<bool>("whitespace-comma").copied(),
        whitespace_assignment: matches.get_one::<bool>("whitespace-assignment").copied(),
        whitespace_decl: matches.get_one::<bool>("whitespace-decl").copied(),
        whitespace_relational: matches.get_one::<bool>("whitespace-relational").copied(),
        whitespace_logical: matches.get_one::<bool>("whitespace-logical").copied(),
        whitespace_plusminus: matches.get_one::<bool>("whitespace-plusminus").copied(),
        whitespace_multdiv: matches.get_one::<bool>("whitespace-multdiv").copied(),
        whitespace_print: matches.get_one::<bool>("whitespace-print").copied(),
        whitespace_type: matches.get_one::<bool>("whitespace-type").copied(),
        whitespace_intrinsics: matches.get_one::<bool>("whitespace-intrinsics").copied(),
        whitespace_concat: matches.get_one::<bool>("whitespace-concat").copied(),
        no_indent: matches.get_flag("no-indent"),
        no_whitespace: matches.get_flag("no-whitespace"),
        strict_indent: matches.get_flag("strict-indent"),
        no_indent_fypp: matches.get_flag("no-indent-fypp"),
        no_indent_mod: matches.get_flag("no-indent-mod"),
        normalize_comment_spacing: matches.get_flag("normalize-comment-spacing"),
        format_decl: matches.get_flag("format-decl"),
        enable_replacements: matches.get_flag("enable-replacements"),
        c_relations: matches.get_flag("c-relations"),
        comment_spacing: matches.get_one::<usize>("comment-spacing").copied(),
        stdout: matches.get_flag("stdout"),
        diff: matches.get_flag("diff"),
        config: matches.get_one::<PathBuf>("config").cloned(),
        recursive: matches.get_flag("recursive"),
        exclude: matches
            .get_many::<String>("exclude")
            .map(|vals| vals.cloned().collect())
            .unwrap_or_default(),
        fortran_extensions: matches
            .get_many::<String>("fortran")
            .map(|vals| vals.cloned().collect())
            .unwrap_or_default(),
        exclude_max_lines: matches.get_one::<usize>("exclude-max-lines").copied(),
        debug: matches.get_flag("debug"),
        silent: matches.get_flag("silent"),
        case,
        jobs: matches.get_one::<usize>("jobs").copied(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cli_builds() {
        let cmd = build_cli();
        // Just verify it builds without panic
        assert_eq!(cmd.get_name(), "fprettier");
    }

    #[test]
    fn test_cli_defaults() {
        let cmd = build_cli();
        let matches = cmd.try_get_matches_from(vec!["fprettier"]).unwrap();

        assert!(matches.get_many::<PathBuf>("inputs").is_none());
        assert!(!matches.get_flag("no-indent"));
        assert!(!matches.get_flag("stdout"));
    }

    #[test]
    fn test_whitespace_comma_flag() {
        // Test --whitespace-comma (no value = true)
        let args = parse_args_from(vec!["fprettier", "--whitespace-comma", "file.f90"]);
        assert_eq!(args.whitespace_comma, Some(true));
    }

    #[test]
    fn test_whitespace_comma_explicit_true() {
        // Test --whitespace-comma=true
        let args = parse_args_from(vec!["fprettier", "--whitespace-comma=true", "file.f90"]);
        assert_eq!(args.whitespace_comma, Some(true));
    }

    #[test]
    fn test_whitespace_comma_explicit_false() {
        // Test --whitespace-comma=false
        let args = parse_args_from(vec!["fprettier", "--whitespace-comma=false", "file.f90"]);
        assert_eq!(args.whitespace_comma, Some(false));
    }

    #[test]
    fn test_whitespace_options_not_set() {
        // Test that options are None when not specified
        let args = parse_args_from(vec!["fprettier", "file.f90"]);
        assert_eq!(args.whitespace_comma, None);
        assert_eq!(args.whitespace_assignment, None);
        assert_eq!(args.whitespace_decl, None);
        assert_eq!(args.whitespace_relational, None);
        assert_eq!(args.whitespace_logical, None);
        assert_eq!(args.whitespace_plusminus, None);
        assert_eq!(args.whitespace_multdiv, None);
        assert_eq!(args.whitespace_print, None);
        assert_eq!(args.whitespace_type, None);
        assert_eq!(args.whitespace_intrinsics, None);
        assert_eq!(args.whitespace_concat, None);
    }

    #[test]
    fn test_multiple_whitespace_options() {
        // Test multiple options together
        let args = parse_args_from(vec![
            "fprettier",
            "--whitespace-comma",
            "--whitespace-concat=false",
            "--whitespace-type=true",
            "file.f90",
        ]);
        assert_eq!(args.whitespace_comma, Some(true));
        assert_eq!(args.whitespace_concat, Some(false));
        assert_eq!(args.whitespace_type, Some(true));
        // Others should be None
        assert_eq!(args.whitespace_assignment, None);
    }

    #[test]
    fn test_all_whitespace_options() {
        // Test all 11 options
        let args = parse_args_from(vec![
            "fprettier",
            "--whitespace-comma=true",
            "--whitespace-assignment=false",
            "--whitespace-decl=true",
            "--whitespace-relational=false",
            "--whitespace-logical=true",
            "--whitespace-plusminus=false",
            "--whitespace-multdiv=true",
            "--whitespace-print=false",
            "--whitespace-type=true",
            "--whitespace-intrinsics=false",
            "--whitespace-concat=true",
            "file.f90",
        ]);
        assert_eq!(args.whitespace_comma, Some(true));
        assert_eq!(args.whitespace_assignment, Some(false));
        assert_eq!(args.whitespace_decl, Some(true));
        assert_eq!(args.whitespace_relational, Some(false));
        assert_eq!(args.whitespace_logical, Some(true));
        assert_eq!(args.whitespace_plusminus, Some(false));
        assert_eq!(args.whitespace_multdiv, Some(true));
        assert_eq!(args.whitespace_print, Some(false));
        assert_eq!(args.whitespace_type, Some(true));
        assert_eq!(args.whitespace_intrinsics, Some(false));
        assert_eq!(args.whitespace_concat, Some(true));
    }

    #[test]
    fn test_comment_spacing() {
        let args = parse_args_from(vec!["fprettier", "--comment-spacing", "3", "file.f90"]);
        assert_eq!(args.comment_spacing, Some(3));
    }

    #[test]
    fn test_comment_spacing_not_set() {
        let args = parse_args_from(vec!["fprettier", "file.f90"]);
        assert_eq!(args.comment_spacing, None);
    }

    #[test]
    fn test_exclude_single() {
        let args = parse_args_from(vec!["fprettier", "-r", "-e", "*.mod", "src/"]);
        assert_eq!(args.exclude, vec!["*.mod"]);
    }

    #[test]
    fn test_exclude_multiple() {
        let args = parse_args_from(vec![
            "fprettier",
            "-r",
            "-e",
            "*.mod",
            "--exclude",
            "build*",
            "-e",
            "test_*",
            "src/",
        ]);
        assert_eq!(args.exclude, vec!["*.mod", "build*", "test_*"]);
    }

    #[test]
    fn test_exclude_empty() {
        let args = parse_args_from(vec!["fprettier", "file.f90"]);
        assert!(args.exclude.is_empty());
    }

    #[test]
    fn test_fortran_single_extension() {
        let args = parse_args_from(vec!["fprettier", "-r", "-f", "f2003", "src/"]);
        assert_eq!(args.fortran_extensions, vec!["f2003"]);
    }

    #[test]
    fn test_fortran_multiple_extensions() {
        let args = parse_args_from(vec![
            "fprettier",
            "-r",
            "-f",
            "f2003",
            "--fortran",
            "F2003",
            "-f",
            "f2008",
            "src/",
        ]);
        assert_eq!(args.fortran_extensions, vec!["f2003", "F2003", "f2008"]);
    }

    #[test]
    fn test_fortran_extensions_empty() {
        let args = parse_args_from(vec!["fprettier", "file.f90"]);
        assert!(args.fortran_extensions.is_empty());
    }

    #[test]
    fn test_exclude_max_lines() {
        let args = parse_args_from(vec!["fprettier", "--exclude-max-lines", "1000", "file.f90"]);
        assert_eq!(args.exclude_max_lines, Some(1000));
    }

    #[test]
    fn test_exclude_max_lines_short_flag() {
        let args = parse_args_from(vec!["fprettier", "-m", "500", "file.f90"]);
        assert_eq!(args.exclude_max_lines, Some(500));
    }

    #[test]
    fn test_exclude_max_lines_not_set() {
        let args = parse_args_from(vec!["fprettier", "file.f90"]);
        assert_eq!(args.exclude_max_lines, None);
    }

    #[test]
    fn test_debug_flag() {
        let args = parse_args_from(vec!["fprettier", "-D", "file.f90"]);
        assert!(args.debug);
    }

    #[test]
    fn test_debug_long_flag() {
        let args = parse_args_from(vec!["fprettier", "--debug", "file.f90"]);
        assert!(args.debug);
    }

    #[test]
    fn test_debug_not_set() {
        let args = parse_args_from(vec!["fprettier", "file.f90"]);
        assert!(!args.debug);
    }
}
