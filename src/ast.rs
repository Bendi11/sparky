use crate::{lex::Op, types};

use super::Type;
use bitflags::bitflags;

bitflags! {
    pub struct Attributes: u8 {
        /// The item's value cannot change
        const CONST = 0b00000001;
        /// The item is defined elsewhere and is only being declared
        const EXT   = 0b00000010;
    }
}


/// The `FunProto` struct holds information about a function like its name, return type, argument names, and 
/// argument types
#[derive(Debug)]
pub struct FunProto {
    /// The name of the function
    pub name: String,
    /// The return type of the function
    pub ret: Type,
    /// The bitflag attributes of this function
    pub attrs: Attributes,
    /// The argument types and optional names that this function takes
    pub args: Vec<(Type, Option<String>)>,
}


/// The `Ast` enum is what is parsed from the lexer's token stream and consumed by the code generator to produce an executable
///
#[derive(Debug)]
pub enum Ast {
    /// A function prototype with all needed information to call the function
    FunProto(FunProto),

    /// A function definition with the function signature and the body of the function 
    FunDef(FunProto, Vec<Ast>),

    /// A constant number literal 
    NumLiteral(Type, String),

    /// A constant string literal
    StrLiteral(String),

    /// A binary operation  applied to two expressions
    Bin(Box<Ast>, Op, Box<Ast>),

    /// A struct definition with the defined type 
    StructDef(types::Container),

    /// A union type definition
    UnionDef(types::Container),

    /// A struct or union field access
    MemberAccess(Box<Ast>, String),

    /// An associated function is being called on a value
    AssocFunAccess(Box<Ast>, String, Vec<Ast>),

    /// A variable declaration with type, name, and attributes of the variable
    VarDecl{
        /// The type of variable being declared
        ty: Type,

        /// The name of the variable
        name: String,

        /// Attributes of the variable declaration
        attrs: Attributes,

    },

    /// A variable access with the name of the variable
    VarAccess(String),
    
    /// A unary operator being applied the the RHS expression
    Unary(Op, Box<Ast>),

    /// A function call with expression arguments
    FunCall(String, Vec<Ast>),

    /// Return statement
    Ret(Box<Ast>),

    /// An if conditional statement
    If {
        /// The condition to check
        cond: Box<Ast>,
        /// Code to execute on true
        true_block: Vec<Ast>,
        /// Code to execute on false, if any
        else_block: Option<Vec<Ast>>,
    },

    /// A while statement for looping
    While {
        /// The condition to check
        cond: Box<Ast>,
        /// The block to loop over
        block: Vec<Ast>,
    },
}


