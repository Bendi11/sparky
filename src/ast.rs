//! The `ast` module provides the all-important
//! [Ast](enum@Ast) enum that represents every possible branch of an abstract syntax tree that has been parsed
//!

use crate::lex::Key;
use std::convert::TryFrom;

use bitflags::bitflags;
use inkwell::{types::BasicTypeEnum, values::IntValue};

bitflags! {
    /// Attributes that can be given to a function prototype
    pub struct FnAttrs: u8 {
        /// This function is only being declared and not defined
        const EXTERN = 0b00000010;
        /// This function should be inlined
        const INLINE = 0b00000100;
    }
}

impl TryFrom<Key> for FnAttrs {
    type Error = ();
    /// Convert a keyword into a function attribute bitflag
    fn try_from(k: Key) -> Result<Self, Self::Error> {
        match k {
            Key::Ext => Ok(Self::EXTERN),
            _ => Err(()),
        }
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

impl TryFrom<Key> for VarAttrs {
    type Error = ();
    /// Convert a keyword into a variable attribute
    fn try_from(k: Key) -> Result<Self, Self::Error> {
        match k {
            Key::Const => Ok(Self::CONST),
            Key::Ext => Ok(Self::EXTERN),
            _ => Err(()),
        }
    }
}

/// The `Body` type represents a list of expressions, like in a function body
pub type Body<'ctx> = Vec<Ast<'ctx>>;

/// The `FnProto` struct holds all information needed to call a function like argument count, argument types, return type, and
/// function name
#[derive(Debug, PartialEq, Eq)]
pub struct FnProto<'ctx> {
    /// The name of the function
    pub name: String,
    /// The return type of the function
    pub ret: BasicTypeEnum<'ctx>,

    /// The expected argument types
    pub args: Vec<BasicTypeEnum<'ctx>>,

    /// A list of argument names that correspond to the argument types
    pub arg_names: Vec<String>,

    /// The attributes that this function prototype has
    pub attrs: FnAttrs,
}

/// The `Ast` enum represents every type of abstract syntax tree node that the [Parser](struct@crate::parse::Parser) produces
#[derive(Debug, PartialEq, Eq)]
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

    /// This is a string literal
    StrLiteral(String),

    /// This is a number literal
    NumLiteral(IntValue<'ctx>),

    /// Return an expression
    Ret(Box<Ast<'ctx>>),
}
