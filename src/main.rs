//! fprettier - Auto-formatter for modern Fortran source code

#![warn(clippy::all)]
#![warn(clippy::pedantic)]

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufReader, Cursor, IsTerminal, Read, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::atomic::{AtomicUsize, Ordering};

use anyhow::Result;
use fprettier::process::format_file;
use fprettier::{build_cli, find_directive, parse_args, CliArgs, Config};
use glob::Pattern;
use rayon::prelude::*;
use similar::TextDiff;
use walkdir::WalkDir;

/// Fortran file extensions to process
const FORTRAN_EXTENSIONS: &[&str] = &[
    "f90", "f95", "f03", "f08", "f18", "f", "for", "ftn", "fpp", "F90", "F95", "F03", "F08", "F18",
    "F", "FOR", "FTN", "FPP",
];

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

    // Collect all files to process
    let files = collect_files(&args);

    if files.is_empty() {
        if !args.silent {
            eprintln!("No Fortran files found to format.");
        }
        return Ok(ExitCode::SUCCESS);
    }

    // Sequential processing keeps stdout/diff/check output deterministic
    let use_sequential = args.stdout || args.diff || args.check || args.jobs == Some(1);

    // Pre-warm regex patterns on the main thread to avoid contention
    // during parallel processing. There are ~100 LazyLock<Regex> patterns
    // across the codebase; formatting a minimal program initializes them all.
    if !use_sequential {
        let warmup = b"program x\nend program x\n";
        let _ = format_file(
            BufReader::new(Cursor::new(warmup.as_slice())),
            &mut Vec::new(),
            base_config.as_ref().unwrap_or(&Config::default()),
            "warmup",
        );
    }

    // Process files
    let (changed, errors) = if use_sequential {
        process_files_sequential(&files, base_config.as_ref(), &args)
    } else {
        // Parallel processing for in-place formatting
        process_files_parallel(&files, base_config.as_ref(), &args);
        (0, 0)
    };

    if args.check && (changed > 0 || errors > 0) {
        return Ok(ExitCode::FAILURE);
    }
    Ok(ExitCode::SUCCESS)
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
        Config::from_discovered_files(&start)
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
    for (key, val) in &args.whitespace_overrides {
        config.whitespace_dict.insert(key.clone(), *val);
    }

    if args.no_indent {
        config.impose_indent = false;
    }
    if args.no_whitespace {
        config.impose_whitespace = false;
    }
    if args.strict_indent {
        config.strict_indent = true;
    }
    if args.no_indent_fypp {
        config.indent_fypp = false;
    }
    if args.no_indent_mod {
        config.indent_mod = false;
    }
    if args.normalize_comment_spacing {
        config.normalize_comment_spacing = true;
    }
    if args.format_decl {
        config.format_decl = true;
    }
    if args.enable_replacements {
        config.enable_replacements = true;
    }
    if args.c_relations {
        config.c_relations = true;
    }
    if let Some(spacing) = args.comment_spacing {
        config.comment_spacing = spacing;
    }
    if let Some(case) = args.case {
        config.case_dict.insert("keywords".to_string(), case[0]);
        config.case_dict.insert("procedures".to_string(), case[1]);
        config.case_dict.insert("operators".to_string(), case[2]);
        config.case_dict.insert("constants".to_string(), case[3]);
    }

    // Print final config in debug mode
    if args.debug {
        print_config_debug(&config);
    }

    // Validate configuration
    if let Some(error) = config.validate() {
        anyhow::bail!("invalid configuration: {error}");
    }

    Ok(config)
}

/// Print configuration values in debug mode
fn print_config_debug(config: &Config) {
    eprintln!("[DEBUG] Configuration: {config:#?}");
    let whitespace_flags = config.get_whitespace_flags();
    eprintln!("[DEBUG] whitespace_flags array: {whitespace_flags:?}");
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
            if args.recursive {
                // Recursive directory traversal
                // Note: WalkDir detects symlink loops when follow_links(true) and
                // returns errors for them. We skip errors via filter_map(ok).
                // max_depth prevents runaway traversal in pathological directory structures.
                for entry in WalkDir::new(input)
                    .follow_links(true)
                    .max_depth(256)
                    .into_iter()
                    .filter_map(std::result::Result::ok)
                {
                    if wanted(entry.path()) {
                        files.push(entry.path().to_path_buf());
                    }
                }
            } else {
                // Non-recursive: only direct children
                if let Ok(entries) = std::fs::read_dir(input) {
                    for entry in entries.filter_map(std::result::Result::ok) {
                        let path = entry.path();
                        if wanted(&path) {
                            files.push(path);
                        }
                    }
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

/// Count the number of lines in a byte buffer
#[allow(clippy::naive_bytecount)] // Simple use case, no need for bytecount crate
fn count_lines(contents: &[u8]) -> usize {
    // Count newlines; add 1 if file doesn't end with newline and has content
    let newlines = contents.iter().filter(|&&b| b == b'\n').count();
    if contents.is_empty() {
        0
    } else if contents.last() == Some(&b'\n') {
        newlines
    } else {
        newlines + 1
    }
}

/// Check if a file has a Fortran extension
/// Checks against both default extensions and any custom extensions provided
fn is_fortran_file(path: &Path, custom_extensions: &[String]) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| {
            // Check default extensions
            if FORTRAN_EXTENSIONS.contains(&ext) {
                return true;
            }
            // Check custom extensions (with or without leading dot)
            for custom in custom_extensions {
                let custom_ext = custom.strip_prefix('.').unwrap_or(custom);
                if ext == custom_ext {
                    return true;
                }
            }
            false
        })
}

/// Process files sequentially (for stdout/diff/check output).
/// Returns (files that changed or would change, files that errored).
fn process_files_sequential(
    files: &[PathBuf],
    base_config: Option<&Config>,
    args: &CliArgs,
) -> (usize, usize) {
    let mut changed = 0;
    let mut errors = 0;
    for path in files {
        // Use base config if provided, otherwise discover per-file config
        let file_result = if let Some(config) = base_config {
            process_single_file(path, config, args)
        } else {
            match build_config(args, Some(path)) {
                Ok(config) => process_single_file(path, &config, args),
                Err(e) => Err(e),
            }
        };

        match file_result {
            Ok(true) => changed += 1,
            Ok(false) => {}
            Err(e) => {
                errors += 1;
                eprintln!("Error formatting {}: {}", path.display(), e);
            }
        }
    }
    (changed, errors)
}

/// Process files in parallel using Rayon
fn process_files_parallel(files: &[PathBuf], base_config: Option<&Config>, args: &CliArgs) {
    let success_count = AtomicUsize::new(0);
    let error_count = AtomicUsize::new(0);

    // Pre-compute per-directory configs to avoid redundant filesystem walks
    // during parallel processing. Config discovery walks all ancestor directories
    // checking for fprettier.toml; caching by parent dir eliminates ~10 stat()
    // calls per file and removes filesystem contention between threads.
    let dir_configs: HashMap<PathBuf, Config> = if base_config.is_none() {
        let mut unique_dirs: Vec<PathBuf> = files
            .iter()
            .filter_map(|f| f.parent().map(Path::to_path_buf))
            .collect();
        unique_dirs.sort();
        unique_dirs.dedup();

        unique_dirs
            .into_iter()
            .filter_map(|dir| build_config(args, Some(&dir)).ok().map(|c| (dir, c)))
            .collect()
    } else {
        HashMap::new()
    };

    files.par_iter().for_each(|path| {
        let file_result = if let Some(config) = base_config {
            process_single_file(path, config, args)
        } else {
            let dir = path.parent().unwrap_or(Path::new(".")).to_path_buf();
            match dir_configs.get(&dir) {
                Some(config) => process_single_file(path, config, args),
                None => match build_config(args, Some(path)) {
                    Ok(config) => process_single_file(path, &config, args),
                    Err(e) => Err(e),
                },
            }
        };

        match file_result {
            Ok(_) => {
                success_count.fetch_add(1, Ordering::Relaxed);
            }
            Err(e) => {
                error_count.fetch_add(1, Ordering::Relaxed);
                eprintln!("Error formatting {}: {}", path.display(), e);
            }
        }
    });

    let success = success_count.load(Ordering::Relaxed);
    let errors = error_count.load(Ordering::Relaxed);

    if !args.silent {
        if errors == 0 {
            eprintln!("Formatted {success} files successfully.");
        } else {
            eprintln!("Formatted {success} files, {errors} errors.");
        }
    }
}

/// Apply directive overrides from file contents to a configuration
fn apply_directive_overrides(config: &mut Config, contents: &[u8], debug: bool, source_name: &str) {
    let cursor = Cursor::new(contents);
    if let Some(overrides) = find_directive(&mut BufReader::new(cursor)) {
        if debug {
            eprintln!("[DEBUG] Found file directive in {source_name}");
        }
        if let Some(indent) = overrides.indent {
            if debug {
                eprintln!("[DEBUG]   Directive override: indent = {indent}");
            }
            config.indent = indent;
        }
        if let Some(line_length) = overrides.line_length {
            if debug {
                eprintln!("[DEBUG]   Directive override: line_length = {line_length}");
            }
            config.line_length = line_length;
        }
        if let Some(whitespace) = overrides.whitespace {
            if debug {
                eprintln!("[DEBUG]   Directive override: whitespace = {whitespace}");
            }
            config.whitespace = whitespace;
        }
        if let Some(impose_indent) = overrides.impose_indent {
            if debug {
                eprintln!("[DEBUG]   Directive override: impose_indent = {impose_indent}");
            }
            config.impose_indent = impose_indent;
        }
        if let Some(impose_whitespace) = overrides.impose_whitespace {
            if debug {
                eprintln!("[DEBUG]   Directive override: impose_whitespace = {impose_whitespace}");
            }
            config.impose_whitespace = impose_whitespace;
        }
        for (key, value) in overrides.get_case_dict() {
            if debug {
                eprintln!("[DEBUG]   Directive override: case[{key}] = {value:?}");
            }
            config.case_dict.insert(key, value);
        }
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
    let has_directives = {
        let cursor = Cursor::new(&file_contents);
        find_directive(&mut BufReader::new(cursor)).is_some()
    };
    let mut file_config;
    let effective_config = if has_directives {
        file_config = config.clone();
        apply_directive_overrides(&mut file_config, &file_contents, args.debug, source_name);
        &file_config
    } else {
        config
    };

    // Format the file
    let reader = BufReader::new(Cursor::new(&file_contents));
    let mut output = Vec::with_capacity(file_contents.len());
    format_file(reader, &mut output, effective_config, source_name)?;

    // Output results
    let changed = output != file_contents;
    if args.diff || args.check {
        if changed {
            if args.check {
                println!("Would reformat: {}", path.display());
            }
            if args.diff {
                print_diff(&file_contents, &output, source_name);
            }
        }
    } else if args.stdout {
        io::stdout().write_all(&output)?;
    } else if changed {
        // Write back to file only if content changed
        std::fs::write(path, &output)?;
    }

    Ok(changed)
}

/// Print a unified diff between original and formatted contents
fn print_diff(original: &[u8], formatted: &[u8], name: &str) {
    let old = String::from_utf8_lossy(original);
    let new = String::from_utf8_lossy(formatted);
    let diff = TextDiff::from_lines(old.as_ref(), new.as_ref());
    print!("{}", diff.unified_diff().header(name, name));
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
    apply_directive_overrides(&mut file_config, &stdin_contents, args.debug, "stdin");

    // Format the input
    let reader = BufReader::new(Cursor::new(&stdin_contents));
    let mut output = Vec::new();
    format_file(reader, &mut output, &file_config, "stdin")?;

    if args.diff || args.check {
        let changed = output != stdin_contents;
        if changed {
            if args.check {
                println!("Would reformat: stdin");
            }
            if args.diff {
                print_diff(&stdin_contents, &output, "stdin");
            }
        }
        return Ok(if args.check && changed {
            ExitCode::FAILURE
        } else {
            ExitCode::SUCCESS
        });
    }

    // Always output to stdout when reading from stdin
    io::stdout().write_all(&output)?;

    if !args.silent {
        eprintln!("Formatted stdin successfully.");
    }

    Ok(ExitCode::SUCCESS)
}
