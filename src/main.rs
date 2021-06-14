pub mod ast;
pub mod code;
pub mod lex;
pub mod types;
use inkwell::context::Context;
pub use types::Type;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parse);

const SRC: &[u8] = br#"
struct test {
    i32 a,
}

fun main() i32 {
    var i32 a = 200;
    var i32 b = a + 200;
    var test abc;
    ret b + 3;
}
"#;

fn main() {
    let mut reader = std::io::BufReader::new(SRC);
    let p = lex::Lexer::from_reader(&mut reader);
    let ctx = Context::create();
    let code = code::Compiler::new(&ctx);
    code.compile(parse::ProgramParser::new().parse(p).unwrap(), "output");
}
