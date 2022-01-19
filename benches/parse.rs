use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark(c: &mut Criterion) {
    c.bench_function("Parse 100 LOC", move |b| {
        b.iter(|| black_box(vec![0u8; 100]))
    });
}

criterion_group!(lexer, benchmark);
criterion_main!(lexer);
