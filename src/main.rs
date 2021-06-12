pub mod lex;
pub mod ast;
pub mod types;
pub use types::Type;

use lalrpop_util::lalrpop_mod;


lalrpop_mod!(pub parse);

const SRC: &[u8] = br#"
fun ext testing2(i32, u8 ptr) struct test_struct;

fun main() i32 {
    struct a b;
    b.testing("test");
}
"#;

fn main() {
    let mut reader = std::io::BufReader::new(SRC);
    let p = lex::Lexer::from_reader(&mut reader);
    println!("{:#?}", parse::ProgramParser::new().parse(p).unwrap());
}
