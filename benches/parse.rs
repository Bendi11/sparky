use spark::ast::Body;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

const LONG_SOURCE: &'static str = r#"

;w testing_word () () {
    push pop lol
}
pop drop push pop 
"#;

/// Benchmark the lexer's speed on a large input source code string
pub fn lex(bencher: &mut Criterion) {
    bencher.bench_function("parse_long", |bencher| {
        bencher.iter(|| black_box(LONG_SOURCE.parse::<Body>().unwrap()));
    });
}

criterion_group!(lexical, lex);
criterion_main!(lexical);
