use internment::LocalIntern;

pub mod parse;
pub mod util;
pub mod ast;
pub mod error;
pub mod arena;
pub mod codegen;

pub type Symbol = LocalIntern<String>;
