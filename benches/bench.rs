use std::fs;
use std::hint::black_box;
use std::path::Path;
use std::process::Command;

use criterion::{Criterion, criterion_group, criterion_main};
use fprettier::Config;
use fprettier::process::format_file;

fn bench_format_file(c: &mut Criterion) {
    let input = include_str!("../examples/in/example.f90");
    let config = Config::default();
    c.bench_function("format_file example.f90", |b| {
        b.iter(|| {
            let mut output = Vec::with_capacity(input.len());
            format_file(black_box(input.as_bytes()), &mut output, &config).unwrap();
            output
        });
    });
}

/// The CLI end to end on a tree of 480 files (1.5 MB): walk, read, format
/// across the rayon pool, write back. The first run formats the copies in
/// place and criterion's warm-up absorbs it, so the samples time a tree that
/// is already formatted, with no writes. An empty `--config` keeps a
/// `fprettier.toml` in a parent or home directory out of the numbers.
fn bench_cli_tree(c: &mut Criterion) {
    let root = std::env::temp_dir().join(format!("fprettier-bench-{}", std::process::id()));
    let tree = root.join("src");
    for dir in 0..10 {
        let dir = tree.join(format!("mod{dir}"));
        fs::create_dir_all(&dir).unwrap();
        for entry in fs::read_dir(Path::new("examples/in")).unwrap() {
            let path = entry.unwrap().path();
            let stem = path.file_stem().unwrap().to_string_lossy();
            for copy in 0..6 {
                fs::copy(&path, dir.join(format!("{stem}_{copy}.f90"))).unwrap();
            }
        }
    }
    let config = root.join("fprettier.toml");
    fs::write(&config, "").unwrap();

    let mut group = c.benchmark_group("cli");
    group.sample_size(20);
    group.bench_function("tree 480 files", |b| {
        b.iter(|| {
            let status = Command::new(env!("CARGO_BIN_EXE_fprettier"))
                .arg("--recursive")
                .arg("--silent")
                .arg("--config")
                .arg(&config)
                .arg(&tree)
                .status()
                .unwrap();
            assert!(status.success());
        });
    });
    group.finish();

    fs::remove_dir_all(&root).unwrap();
}

criterion_group!(benches, bench_format_file, bench_cli_tree);
criterion_main!(benches);
