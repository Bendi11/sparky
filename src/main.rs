use inkwell::context::Context;
use spark::{ast::Body, parse::SparkParse};

pub mod ast;
pub mod code;
pub mod lex;
pub mod parse;
pub mod types;

fn main() {
    let ctx = Context::create();
    let p: Body = ";w Testing (i32, u8) (i32) {} testword[a test] 10 add"
        .spark_parse(&ctx)
        .unwrap();
    println!(
        "AST: {:#}",
        p.iter().fold(String::new(), |acc, ast| acc
            + ast.to_string().as_str()
            + "\n")
    );
}
