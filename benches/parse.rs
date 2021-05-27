use spark::{lex::Lexer, parse::Parser};
use std::io::BufReader;
use inkwell::context::Context;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

const LONG_SOURCE: &[u8] = 
br#"
fun testing_fun(i32 argc, u8 ptr ptr argv) i32 {
    print("Testing!");
};
"#;

/// Benchmark the lexer's speed on a large input source code string
pub fn lex(bencher: &mut Criterion) {
    let ctx = Context::create();

        let mut reader = BufReader::new(LONG_SOURCE);
        let mut parser: Parser<'_, Lexer<_>> = Parser::new(&ctx, Lexer::from_reader(&mut reader));
    bencher.bench_function("parse_long", |bencher| {
        bencher.iter(|| black_box(parser.parse()));
    });
}

criterion_group!(lexical, lex);
criterion_main!(lexical);
