pub mod lex;
pub mod ast;
pub mod types;
pub use types::Type;

use lalrpop_util::lalrpop_mod;


lalrpop_mod!(pub parse);

const SRC: &[u8] = br#"
fun ext testing2(i32, u8 ptr) i32;

fun const testing(i32 a, u8 ptr, u8 ptr ptr ptr argc) i32 {
    while a > 10 {
        if b && c {
            print("Hi there!");
        };
    };
    i32 a = 100;
    u8 b = (a * 1000);
}
"#;

fn main() {
    let mut reader = std::io::BufReader::new(SRC);
    let p = lex::Lexer::from_reader(&mut reader);
    println!("{:#?}", parse::ProgramParser::new().parse(p).unwrap());
}
