//! fprettier - Auto-formatter for modern Fortran source code

use mimalloc::MiMalloc;

/// Benchmarked 2026-08-10 on a 3000-file / 18 MB tree (8 cores), against an
/// otherwise identical build using the system allocator. mimalloc wins by
/// 5.5-9.5% (best-of-7 wall clock) at every thread count:
///
/// | -j | mimalloc | system |
/// |----|----------|--------|
/// |  1 |  3432 ms | 3791 ms|
/// |  2 |  2157 ms | 2303 ms|
/// |  4 |  1335 ms | 1412 ms|
/// |  8 |   746 ms |  799 ms|
///
/// Note the win is *largest single-threaded* and does not grow with thread
/// count, so this is not about heap lock contention between rayon workers
/// (as 4066bee claimed) - formatting just churns many small short-lived
/// Strings, and mimalloc is faster per allocation. Don't drop this without
/// re-running the comparison.
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufReader, Cursor, IsTerminal, Read, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::atomic::{AtomicUsize, Ordering};

use anyhow::Result;
use fprettier::format::case_convert::CASE_KEYS;
use fprettier::process::format_file;
use fprettier::{build_cli, find_directive, parse_args, CliArgs, Config, DirectiveOverrides};
use glob::Pattern;
use rayon::prelude::*;
use similar::TextDiff;
use walkdir::WalkDir;

/// Fortran file extensions to process, matched case-insensitively
const FORTRAN_EXTENSIONS: &[&str] = &["f90", "f95", "f03", "f08", "f18", "f", "for", "ftn", "fpp"];

/// Default maximum file size in bytes (100 MB)
/// Files larger than this are skipped to prevent memory exhaustion
const DEFAULT_MAX_FILE_SIZE: u64 = 100 * 1024 * 1024;

fn main() -> Result<ExitCode> {
    // Parse CLI arguments
    let args = parse_args();

    // Check if we should read from stdin
    let use_stdin =
        args.inputs.is_empty() || (args.inputs.len() == 1 && args.inputs[0].as_os_str() == "-");

    // If no inputs and running interactively, print help; otherwise read from stdin
    if args.inputs.is_empty() && io::stdin().is_terminal() {
        build_cli().print_help()?;
        return Ok(ExitCode::SUCCESS);
    }

    if use_stdin {
        // Process stdin - use current directory for config discovery
        let config = build_config(&args, None)?;
        return process_stdin(&config, &args);
    }

    // Build base configuration for parallel processing
    // For explicit config files, we use one config for all files
    // For auto-discovery, each file may have its own config
    let use_per_file_config = args.config.is_none();
    let base_config = if use_per_file_config {
        None
    } else {
        Some(build_config(&args, None)?)
    };

    // Configure thread pool if --jobs specified
    if let Some(jobs) = args.jobs {
        if jobs > 0 {
            if let Err(e) = rayon::ThreadPoolBuilder::new()
                .num_threads(jobs)
                .build_global()
            {
                eprintln!("Warning: failed to configure thread pool: {e}");
            }
        }
    }

    // An input path that does not exist is an error, not an empty run: it is
    // almost always a typo, and silently exiting 0 hides it from CI.
    let mut missing = 0;
    for path in args.inputs.iter().filter(|path| !path.exists()) {
        eprintln!("Error: no such file or directory: {}", path.display());
        missing += 1;
    }

    // Collect all files to process
    let files = collect_files(&args);

    if files.is_empty() {
        if !args.silent && missing == 0 {
            eprintln!("No Fortran files found to format.");
        }
        return Ok(exit_code(missing > 0));
    }

    // Sequential processing keeps stdout/diff/check output deterministic
    let use_sequential = args.stdout || args.diff || args.check || args.jobs == Some(1);

    // Pre-warm regex patterns on the main thread to avoid contention
    // during parallel processing. There are ~100 LazyLock<Regex> patterns
    // across the codebase; formatting a minimal program initializes them all.
    // Measured 2026-08-10: worth 10-40 ms, i.e. ~1-3% of a 3000-file run.
    // It is a fixed one-time cost, so the percentage shrinks as trees grow.
    if !use_sequential {
        let warmup = b"program x\nend program x\n";
        let _ = format_file(
            BufReader::new(Cursor::new(warmup.as_slice())),
            &mut Vec::new(),
            base_config.as_ref().unwrap_or(&Config::default()),
        );
    }

    let (changed, errors) = process_files(&files, base_config.as_ref(), &args, use_sequential);

    Ok(exit_code(
        errors + missing > 0 || (args.check && changed > 0),
    ))
}

/// `ExitCode::FAILURE` when something went wrong, `SUCCESS` otherwise
fn exit_code(failed: bool) -> ExitCode {
    if failed {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

/// Build configuration from CLI args and optional config file
///
/// If `for_path` is provided and no explicit config file is specified,
/// uses auto-discovery to find config files in parent directories.
fn build_config(args: &CliArgs, for_path: Option<&Path>) -> Result<Config> {
    let mut config = if let Some(config_path) = &args.config {
        // Explicit config file specified
        if args.debug {
            eprintln!(
                "[DEBUG] Using explicit config file: {}",
                config_path.display()
            );
        }
        Config::from_toml_file(config_path)?
    } else {
        // Auto-discover config files from parent directories of the target
        // path, or of the current directory when no path is given
        let start = for_path.map_or_else(
            || std::env::current_dir().unwrap_or_default(),
            Path::to_path_buf,
        );
        if args.debug {
            let discovered = Config::discover_config_files(&start);
            if discovered.is_empty() {
                eprintln!(
                    "[DEBUG] No config files discovered for: {}",
                    start.display()
                );
            } else {
                eprintln!("[DEBUG] Discovered config files for {}:", start.display());
                for f in &discovered {
                    eprintln!("[DEBUG]   - {}", f.display());
                }
            }
        }
        Config::from_discovered_files(&start)?
    };

    // Override with CLI arguments
    if let Some(indent) = args.indent {
        config.indent = indent;
    }
    if let Some(line_length) = args.line_length {
        config.line_length = line_length;
    }
    if let Some(whitespace) = args.whitespace {
        config.whitespace = whitespace;
    }

    // Apply fine-grained whitespace overrides
    config
        .whitespace_dict
        .extend(args.whitespace_overrides.iter().cloned());

    // Every boolean flag is one-way: a --no-* flag can only clear a setting the
    // config file turned on, and the rest can only turn one on.
    config.impose_indent &= !args.no_indent;
    config.impose_whitespace &= !args.no_whitespace;
    config.indent_fypp &= !args.no_indent_fypp;
    config.indent_mod &= !args.no_indent_mod;
    config.strict_indent |= args.strict_indent;
    config.normalize_comment_spacing |= args.normalize_comment_spacing;
    config.format_decl |= args.format_decl;
    config.sort_use |= args.sort_use;
    config.sort_use_only |= args.sort_use_only;
    config.enable_replacements |= args.enable_replacements;
    config.c_relations |= args.c_relations;
    if let Some(spacing) = args.comment_spacing {
        config.comment_spacing = spacing;
    }
    if let Some(case) = &args.case {
        // zip stops at the shorter side: --case sets the first four keys and
        // leaves `types` to --case-types (or to the config file)
        for (key, mode) in CASE_KEYS.iter().zip(case) {
            config.case_dict.insert((*key).to_string(), *mode);
        }
    }
    if let Some(types) = args.case_types {
        config.case_dict.insert("types".to_string(), types);
    }

    // Print final config in debug mode
    if args.debug {
        eprintln!("[DEBUG] Configuration: {config:#?}");
        eprintln!(
            "[DEBUG] whitespace_flags array: {:?}",
            config.get_whitespace_flags()
        );
    }

    // Validate configuration
    if let Some(error) = config.validate() {
        anyhow::bail!("invalid configuration: {error}");
    }

    Ok(config)
}

/// Collect all files to process, handling directories and recursive flag
fn collect_files(args: &CliArgs) -> Vec<PathBuf> {
    // Compile exclude patterns
    let exclude_patterns: Vec<Pattern> = args
        .exclude
        .iter()
        .filter_map(|p| Pattern::new(p).ok())
        .collect();

    // Get custom Fortran extensions
    let custom_extensions = &args.fortran_extensions;

    // A directory entry is formatted when it is a Fortran file and not excluded
    let wanted = |path: &Path| {
        path.is_file()
            && is_fortran_file(path, custom_extensions)
            && !is_excluded(path, &exclude_patterns)
    };

    let mut files = Vec::new();

    for input in &args.inputs {
        if input.is_file() {
            if !is_excluded(input, &exclude_patterns) {
                files.push(input.clone());
            }
        } else if input.is_dir() {
            // Note: WalkDir detects symlink loops when follow_links(true) and
            // returns errors for them. We skip errors via filter_map(ok).
            // max_depth prevents runaway traversal in pathological directory
            // structures, and pins the non-recursive case to direct children.
            let max_depth = if args.recursive { 256 } else { 1 };
            for entry in WalkDir::new(input)
                .follow_links(true)
                .max_depth(max_depth)
                .into_iter()
                .filter_map(std::result::Result::ok)
            {
                if wanted(entry.path()) {
                    files.push(entry.path().to_path_buf());
                }
            }
        }
    }

    files
}

/// Check if a path matches any exclusion pattern
fn is_excluded(path: &Path, patterns: &[Pattern]) -> bool {
    if patterns.is_empty() {
        return false;
    }

    let path_str = path.to_string_lossy();

    for pattern in patterns {
        // Match against full path
        if pattern.matches(&path_str) {
            return true;
        }

        // Match against file name only
        if let Some(file_name) = path.file_name() {
            if pattern.matches(&file_name.to_string_lossy()) {
                return true;
            }
        }

        // Match against each path component (for directory patterns)
        for component in path.components() {
            if let std::path::Component::Normal(c) = component {
                if pattern.matches(&c.to_string_lossy()) {
                    return true;
                }
            }
        }
    }

    false
}

/// Count the lines in a byte buffer, for the `--exclude-max-lines` check
#[allow(clippy::naive_bytecount)] // one comparison per byte; not worth a crate
fn count_lines(contents: &[u8]) -> usize {
    contents.iter().filter(|&&b| b == b'\n').count()
}

/// Check if a file has a Fortran extension
/// Checks against both default extensions and any custom extensions provided
fn is_fortran_file(path: &Path, custom_extensions: &[String]) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| {
            FORTRAN_EXTENSIONS
                .iter()
                .any(|e| ext.eq_ignore_ascii_case(e))
                || custom_extensions
                    .iter()
                    .any(|c| ext == c.strip_prefix('.').unwrap_or(c))
        })
}

/// Process every file, returning (files that changed or would change, files
/// that errored).
///
/// `sequential` keeps stdout deterministic, which `--stdout`, `--diff` and
/// `--check` all need; otherwise the files are spread across the rayon pool.
fn process_files(
    files: &[PathBuf],
    base_config: Option<&Config>,
    args: &CliArgs,
    sequential: bool,
) -> (usize, usize) {
    // Pre-compute per-directory configs to avoid redundant filesystem walks.
    // Config discovery walks all ancestor directories checking for
    // fprettier.toml; caching by parent dir eliminates ~10 stat() calls per
    // file and removes filesystem contention between threads.
    let dir_configs: HashMap<PathBuf, Config> = if base_config.is_none() {
        let mut dirs: Vec<PathBuf> = files
            .iter()
            .filter_map(|f| f.parent().map(Path::to_path_buf))
            .collect();
        dirs.sort();
        dirs.dedup();
        dirs.into_iter()
            .filter_map(|dir| build_config(args, Some(&dir)).ok().map(|c| (dir, c)))
            .collect()
    } else {
        HashMap::new()
    };

    let changed = AtomicUsize::new(0);
    let errors = AtomicUsize::new(0);
    let run = |path: &PathBuf| {
        let dir = path.parent().unwrap_or(Path::new("."));
        let result = match base_config.or_else(|| dir_configs.get(dir)) {
            Some(config) => process_single_file(path, config, args),
            // Nothing cached for this directory means building its config
            // failed above; building it again reports why, against the file
            None => build_config(args, Some(path))
                .and_then(|config| process_single_file(path, &config, args)),
        };
        match result {
            Ok(true) => {
                changed.fetch_add(1, Ordering::Relaxed);
            }
            Ok(false) => {}
            Err(e) => {
                errors.fetch_add(1, Ordering::Relaxed);
                eprintln!("Error formatting {}: {e:#}", path.display());
            }
        }
    };

    if sequential {
        files.iter().for_each(run);
    } else {
        files.par_iter().for_each(run);
    }

    let errors = errors.load(Ordering::Relaxed);
    // The sequential path is driven by --stdout/--diff/--check, whose own
    // output is the report; a summary line on top of it is just noise.
    if !sequential && !args.silent {
        let success = files.len() - errors;
        if errors == 0 {
            eprintln!("Formatted {success} files successfully.");
        } else {
            eprintln!("Formatted {success} files, {errors} errors.");
        }
    }

    (changed.load(Ordering::Relaxed), errors)
}

/// Apply directive overrides parsed from a file to a configuration
fn apply_directive_overrides(
    config: &mut Config,
    overrides: &DirectiveOverrides,
    debug: bool,
    source_name: &str,
) -> Result<()> {
    if debug {
        // The whole struct at once: an Option field prints its own name and
        // whether it was set, which is what five per-field lines used to say
        eprintln!("[DEBUG] Found file directive in {source_name}: {overrides:?}");
    }

    if let Some(indent) = overrides.indent {
        config.indent = indent;
    }
    if let Some(line_length) = overrides.line_length {
        config.line_length = line_length;
    }
    if let Some(whitespace) = overrides.whitespace {
        config.whitespace = whitespace;
    }
    if let Some(impose_indent) = overrides.impose_indent {
        config.impose_indent = impose_indent;
    }
    if let Some(impose_whitespace) = overrides.impose_whitespace {
        config.impose_whitespace = impose_whitespace;
    }
    config.case_dict.extend(overrides.get_case_dict());

    // The directive can set the same values the CLI can, so it needs the same
    // check the CLI got in build_config
    match config.validate() {
        Some(error) => anyhow::bail!("invalid directive in {source_name}: {error}"),
        None => Ok(()),
    }
}

/// Process a single file. Returns whether the file changed (or would change).
fn process_single_file(path: &PathBuf, config: &Config, args: &CliArgs) -> Result<bool> {
    // Check file size BEFORE reading to prevent memory exhaustion
    let metadata = std::fs::metadata(path)?;
    let file_size = metadata.len();
    if file_size > DEFAULT_MAX_FILE_SIZE {
        if !args.silent {
            let size_mb = file_size / (1024 * 1024);
            let limit_mb = DEFAULT_MAX_FILE_SIZE / (1024 * 1024);
            eprintln!(
                "Skipping {} ({} MB exceeds limit of {} MB)",
                path.display(),
                size_mb,
                limit_mb
            );
        }
        return Ok(false);
    }

    // Read input file into memory
    let mut file_contents = Vec::new();
    File::open(path)?.read_to_end(&mut file_contents)?;

    // Check line count limit if specified
    if let Some(max_lines) = args.exclude_max_lines {
        let line_count = count_lines(&file_contents);
        if line_count > max_lines {
            if !args.silent {
                eprintln!(
                    "Skipping {} ({} lines exceeds limit of {})",
                    path.display(),
                    line_count,
                    max_lines
                );
            }
            return Ok(false);
        }
    }

    if !args.silent && !args.stdout && args.debug {
        eprintln!("Formatting: {}", path.display());
    }

    // Check for in-file directives; only clone config if overrides are found.
    // Most files have no directives, so this avoids cloning the Config (with its
    // HashMaps) for every file in the parallel loop.
    let source_name = path.to_str().unwrap_or("unknown");
    let mut file_config;
    let effective_config =
        if let Some(overrides) = find_directive(&mut BufReader::new(Cursor::new(&file_contents))) {
            file_config = config.clone();
            apply_directive_overrides(&mut file_config, &overrides, args.debug, source_name)?;
            &file_config
        } else {
            config
        };

    // Format the file
    let reader = BufReader::new(Cursor::new(&file_contents));
    let mut output = Vec::with_capacity(file_contents.len());
    format_file(reader, &mut output, effective_config)?;

    // Output results
    let changed = output != file_contents;
    if args.diff || args.check {
        if changed {
            if args.check {
                write_stdout(format!("Would reformat: {}\n", path.display()).as_bytes())?;
            }
            if args.diff {
                print_diff(&file_contents, &output, source_name)?;
            }
        }
    } else if args.stdout {
        write_stdout(&output)?;
    } else if changed {
        // Write back to file only if content changed
        write_in_place(path, &output)?;
    }

    Ok(changed)
}

/// Replace a file's contents atomically: write a sibling temporary file and
/// rename it over the target, so an interrupted run cannot leave the source
/// truncated. Symlinks are resolved first, so the link keeps pointing at the
/// file we rewrite instead of being replaced by it.
fn write_in_place(path: &Path, contents: &[u8]) -> Result<()> {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    let target = std::fs::canonicalize(path)?;
    let name = target
        .file_name()
        .map_or_else(|| "file".into(), std::ffi::OsStr::to_os_string);
    let temp = target.with_file_name(format!(
        ".{}.fprettier{}-{}",
        name.to_string_lossy(),
        std::process::id(),
        COUNTER.fetch_add(1, Ordering::Relaxed)
    ));

    let write_and_rename = || -> std::io::Result<()> {
        std::fs::write(&temp, contents)?;
        // rename() does not carry the target's mode over, so copy it across
        std::fs::set_permissions(&temp, std::fs::metadata(&target)?.permissions())?;
        std::fs::rename(&temp, &target)
    };
    write_and_rename().inspect_err(|_| {
        let _ = std::fs::remove_file(&temp);
    })?;

    Ok(())
}

/// Write to stdout, exiting quietly when the reader has closed the pipe.
///
/// Every stdout write goes through here. `--check` listings, `--diff` output
/// and `--stdout` contents are all routinely piped into `head` or `less`,
/// which close the pipe early. Rust ignores SIGPIPE, so the write comes back
/// as `BrokenPipe` and the `print!`/`println!` macros panic on it. A quiet
/// exit is the conventional CLI behavior; under a normal shell the pipeline's
/// status comes from the reader anyway.
fn write_stdout(bytes: &[u8]) -> Result<()> {
    match io::stdout().write_all(bytes) {
        Err(e) if e.kind() == io::ErrorKind::BrokenPipe => std::process::exit(0),
        result => Ok(result?),
    }
}

/// Print a unified diff between original and formatted contents
fn print_diff(original: &[u8], formatted: &[u8], name: &str) -> Result<()> {
    let old = String::from_utf8_lossy(original);
    let new = String::from_utf8_lossy(formatted);
    let diff = TextDiff::from_lines(old.as_ref(), new.as_ref());
    write_stdout(
        diff.unified_diff()
            .header(name, name)
            .to_string()
            .as_bytes(),
    )
}

/// Process input from stdin, output to stdout
fn process_stdin(config: &Config, args: &CliArgs) -> Result<ExitCode> {
    // Read all input from stdin
    let mut stdin_contents = Vec::new();
    io::stdin().read_to_end(&mut stdin_contents)?;

    // Check size after reading to prevent processing extremely large input
    #[allow(clippy::cast_possible_truncation)]
    let stdin_size = stdin_contents.len() as u64;
    if stdin_size > DEFAULT_MAX_FILE_SIZE {
        anyhow::bail!(
            "stdin input too large ({} MB exceeds limit of {} MB)",
            stdin_size / (1024 * 1024),
            DEFAULT_MAX_FILE_SIZE / (1024 * 1024)
        );
    }

    // Make a copy of config that can be overridden by directives
    let mut file_config = config.clone();
    if let Some(overrides) = find_directive(&mut BufReader::new(Cursor::new(&stdin_contents))) {
        apply_directive_overrides(&mut file_config, &overrides, args.debug, "stdin")?;
    }

    // Format the input
    let reader = BufReader::new(Cursor::new(&stdin_contents));
    let mut output = Vec::new();
    format_file(reader, &mut output, &file_config)?;

    if args.diff || args.check {
        let changed = output != stdin_contents;
        if changed {
            if args.check {
                write_stdout(b"Would reformat: stdin\n")?;
            }
            if args.diff {
                print_diff(&stdin_contents, &output, "stdin")?;
            }
        }
        return Ok(if args.check && changed {
            ExitCode::FAILURE
        } else {
            ExitCode::SUCCESS
        });
    }

    // Always output to stdout when reading from stdin
    write_stdout(&output)?;

    if !args.silent {
        eprintln!("Formatted stdin successfully.");
    }

    Ok(ExitCode::SUCCESS)
}
