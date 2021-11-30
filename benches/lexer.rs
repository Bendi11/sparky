use criterion::{Criterion, black_box, criterion_group, criterion_main};
use spark::parse::lex::Lexer;

const BENCH_SOURCE: [&str ; 17] = [r#"
fun main i32 argc, []*char argv -> i32 {
    let string := "testing\"\n\r "
    mut num := 2412414141414 + 0xFFFABC13
    num := num / 0b10101010 * 0o123
    let result := string[31].field.memberfun 100, 31
    (num)
}
"# ; 17];

fn benchmark(c: &mut Criterion) {
    let src = BENCH_SOURCE.join("");
    c.bench_function("Lex 100 LOC", move |b| {
        let mut lexer = Lexer::new(src.as_str());
        b.iter(|| black_box(lexer.all(|_| true)))
    });
}

criterion_group!(lexer, benchmark);
criterion_main!(lexer);