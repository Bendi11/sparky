use inkwell::context::Context;
use spark::parse::SparkParse;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

const LONG_SOURCE: &'static str = r#"

;w testing_word () () {
    push pop lol
}
pop drop push pop 
"#;

/// Benchmark the lexer's speed on a large input source code string
pub fn lex(bencher: &mut Criterion) {
    let ctx = Context::create();
    bencher.bench_function("parse_long", |bencher| {
        bencher.iter(|| black_box(LONG_SOURCE.spark_parse(&ctx).unwrap()));
    });
}

criterion_group!(lexical, lex);
criterion_main!(lexical);
