use internment::LocalIntern;

pub mod arena;
pub mod ast;
pub mod codegen;
pub mod error;
pub mod parse;
pub mod util;

pub type Symbol = LocalIntern<String>;
