use super::lex;
use super::Type;
use bitflags::bitflags;

bitflags! {
    pub struct Attributes: u8 {
        const CONST = 0b00000001;
    }
}


/// The `FunProto` struct holds information about a function like its name, return type, argument names, and 
/// argument types
#[derive(Debug)]
pub struct FunProto {
    pub name: String,
    pub ret: Type,
    pub attrs: Attributes,
    pub args: Vec<(Type, Option<String>)>,
}

#[derive(Debug)]
pub enum Ast {
    FunProto(FunProto),

}


