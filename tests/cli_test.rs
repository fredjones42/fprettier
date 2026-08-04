//! End-to-end tests for the --diff and --check CLI modes

use std::path::PathBuf;
use std::process::Command;

const UNFORMATTED: &str = "program p\ninteger::i\ni=1\nend program p\n";

fn bin() -> Command {
    Command::new(env!("CARGO_BIN_EXE_fprettier"))
}

/// Write contents to a unique temp file and return its path
fn write_temp(name: &str, contents: &str) -> PathBuf {
    let path =
        std::env::temp_dir().join(format!("fprettier-cli-{}-{name}.f90", std::process::id()));
    std::fs::write(&path, contents).unwrap();
    path
}

#[test]
fn test_check_reports_unformatted_and_leaves_file_untouched() {
    let path = write_temp("check-dirty", UNFORMATTED);

    let out = bin().arg("--check").arg(&path).output().unwrap();
    assert_eq!(out.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("Would reformat:"), "stdout: {stdout}");
    assert_eq!(std::fs::read_to_string(&path).unwrap(), UNFORMATTED);

    std::fs::remove_file(&path).unwrap();
}

#[test]
fn test_check_passes_on_formatted_file() {
    let path = write_temp("check-clean", UNFORMATTED);

    // Format in-place, then --check must pass with no output
    assert!(bin().arg(&path).output().unwrap().status.success());
    let out = bin().arg("--check").arg(&path).output().unwrap();
    assert_eq!(out.status.code(), Some(0));
    assert!(out.stdout.is_empty());

    std::fs::remove_file(&path).unwrap();
}

#[test]
fn test_diff_prints_unified_diff_and_leaves_file_untouched() {
    let path = write_temp("diff", UNFORMATTED);

    let out = bin().arg("--diff").arg(&path).output().unwrap();
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("-i=1"), "stdout: {stdout}");
    assert!(stdout.contains("+    i = 1"), "stdout: {stdout}");
    assert_eq!(std::fs::read_to_string(&path).unwrap(), UNFORMATTED);

    std::fs::remove_file(&path).unwrap();
}
