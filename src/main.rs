use spark::{ parse::Parser, lex::Lexer, ast::{Ast, Body}};

pub mod ast;
pub mod lex;
pub mod parse;
pub mod types;

fn main() {
    let p: Body = ";fun[] Testing () () {} testword[a; test]".parse().unwrap();
    println!("AST: {:#}", p.0.iter().fold(String::new(), |acc, ast| acc + ast.to_string().as_str() + "\n"));
}
