use criterion::{criterion_group, criterion_main, Criterion};

fn benchmark(c: &mut Criterion) {
    c.bench_function("Lex 100 LOC", move |_| {});
}

criterion_group!(lexer, benchmark);
criterion_main!(lexer);