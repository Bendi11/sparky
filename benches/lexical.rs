use spark::lex::Lexer;
use std::io::BufReader;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

const LONG_SOURCE: &[u8] = br#"
fun testing_fun(i32 argc, u8 ptr ptr argv) i32 {
    print("Testing!");
}
"#;

/// Benchmark the lexer's speed on a large input source code string
pub fn lex(bencher: &mut Criterion) {
    bencher.bench_function("lex_long", |bencher| {
        bencher.iter(|| black_box(Lexer::new(&mut BufReader::new(LONG_SOURCE)).last()));
    });
}

criterion_group!(lexical, lex);
criterion_main!(lexical);
