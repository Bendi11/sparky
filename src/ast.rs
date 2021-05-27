//! The `ast` module provides the all-important
//! [Ast](enum@Ast) enum that represents every possible branch of an abstract syntax tree that has been parsed
//!

use bitflags::bitflags;
use inkwell::{types::BasicTypeEnum, values::BasicValueEnum};

bitflags! {
    /// Attributes that can be given to a function prototype
    pub struct FnAttrs: u8 {
        /// This function is only being declared and not defined
        const EXTERN = 0b00000010;
        /// This function should be inlined
        const INLINE = 0b00000100;
    }
}

bitflags! {
    /// Attributes that can be applied to variables
    pub struct VarAttrs: u8 {
        /// This variable is defined elsewhere
        const EXTERN = 0b00000001;

        /// This variable can not be mutated, use single static assignment for it
        const CONST = 0b000000010;
    }
}

/// The `Body` type represents a list of expressions, like in a function body
pub type Body<'ctx> = Vec<Ast<'ctx>>;

/// The `FnProto` struct holds all information needed to call a function like argument count, argument types, return type, and
/// function name
#[derive(Debug)]
pub struct FnProto<'ctx> {
    /// The name of the function
    pub name: String,
    /// The return type of the function
    pub ret: BasicTypeEnum<'ctx>,

    /// The expected argument types
    pub args: Vec<BasicTypeEnum<'ctx>>,

    /// The attributes that this function prototype has
    pub attrs: FnAttrs,
}

/// The `Ast` enum represents every type of abstract syntax tree node that the [Parser](struct@crate::parse::Parser) produces
#[derive(Debug)]
pub enum Ast<'ctx> {
    /// A function prototype with no body
    FnProto(FnProto<'ctx>),

    /// This is a function definition with a body, it must not be extern
    Fn(FnProto<'ctx>, Body<'ctx>),

    /// This is a binary expression with an operator being applied
    Binary {
        /// The left hand side of the operation
        lhs: Box<Ast<'ctx>>,
        /// The right hand side of the operation
        rhs: Box<Ast<'ctx>>,
        /// The operation to do
        op: String,
    },

    /// This is a variable declation
    VarDecl {
        /// The name of the variable being declared
        name: String,

        /// The attributes of this variable
        attrs: VarAttrs,

        /// The type of the declared variable
        ty: BasicTypeEnum<'ctx>,
    },

    /// This is a variable being accessed
    VarAccess(String),

    /// This is a function being called
    Call {
        /// The name of the function being called
        name: String,

        /// The arguments that were passed to the function
        args: Body<'ctx>,
    },

    /// This is a literal of any type
    Literal(BasicValueEnum<'ctx>),
}
