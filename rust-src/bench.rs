mod d1lib;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::fs::read_to_string;

const BENCH_DATA: &[(&str, fn(&str) -> usize, fn(&str) -> usize)] =
    &[("d1", d1lib::part_1, d1lib::part_2)];

pub fn run_benchmarks(c: &mut Criterion) {
    for (day, p1, p2) in BENCH_DATA.iter().copied() {
        let filename = format!("input/{day}.txt");
        let file_contents = read_to_string(filename).unwrap();
        c.bench_function(&format!("{day}p1"), |b| {
            b.iter(|| black_box(p1(&file_contents)))
        });
        c.bench_function(&format!("{day}p2"), |b| {
            b.iter(|| black_box(p2(&file_contents)))
        });
    }
}

criterion_group!(benches, run_benchmarks);
criterion_main!(benches);
