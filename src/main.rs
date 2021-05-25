use spark::{
    ast::{Ast, Body},
    lex::Lexer,
    parse::Parser,
};

pub mod ast;
pub mod lex;
pub mod parse;
pub mod types;

fn main() {
    let p: Body = ";w Testing (i32, u8) (i32) {} testword[a; test] 10 add"
        .parse()
        .unwrap();
    println!(
        "AST: {:#}",
        p.0.iter().fold(String::new(), |acc, ast| acc
            + ast.to_string().as_str()
            + "\n")
    );
}
