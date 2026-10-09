use std::hint::black_box;

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

criterion_group!(benches, bench_format_file);
criterion_main!(benches);
