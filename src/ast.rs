use std::ops::Deref;

use crate::{
    lex::Op,
    types::{self, Container},
};

use super::Type;
use bitflags::bitflags;
use hashbrown::HashMap;

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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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

    /// A constant struct literal
    StructLiteral {
        /// The struct type name
        name: String,

        /// The given field values
        fields: Vec<(String, Ast)>,
    },

    /// Cast an expression to a type
    Cast(Box<Ast>, Type),

    /// A struct or union field access
    MemberAccess(Box<Ast>, String),

    /// An associated function is being called on a value
    AssocFunAccess(Box<Ast>, String, Vec<Ast>),

    /// A variable declaration with type, name, and attributes of the variable
    VarDecl {
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
    Ret(Box<Option<Ast>>),

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

    /// A new namespace declaration with the body of expressions inside the namespace
    Namespace(String, Vec<Ast>),
}

impl Ast {
    /// Get the type of this expression, if any
    pub fn get_type<'a, U, U1, U2, U3>(
        &'a self,
        funs: &'a HashMap<String, (U, FunProto)>,
        structs: &'a HashMap<String, (U1, Container)>,
        unions: &'a HashMap<String, (U2, Container)>,
        vars: &'a HashMap<String, (U3, Type)>,
    ) -> Option<Type> {
        Some(match self {
            Self::FunCall(name, _) => funs.get(name)?.1.ret.clone(),
            Self::VarDecl {
                name: _,
                ty,
                attrs: _,
            } => ty.clone(),
            Self::Cast(_, ty) => ty.clone(),
            Self::VarAccess(name) => vars.get(name)?.1.clone(),
            Self::StructLiteral { name, fields: _ } => Type::Struct(structs.get(name)?.1.clone()),
            Self::MemberAccess(first, item) => match first.get_type(funs, structs, unions, vars)? {
                Type::Struct(col) | Type::Union(col) => {
                    col.fields.iter().find(|(name, _)| name == item)?.1.clone()
                }
                _ => return None,
            },
            Self::NumLiteral(ty, _) => ty.clone(),
            Self::StrLiteral(_) => Type::Ptr(Box::new(Type::Integer {
                width: 8,
                signed: false,
            })), //char pointer for string literals
            Self::Unary(op, val) => match op {
                Op::Star => val
                    .deref()
                    .get_type(funs, structs, unions, vars)?
                    .deref_type()?,
                Op::And => val
                    .deref()
                    .get_type(funs, structs, unions, vars)?
                    .ptr_type(),
                _ => return None,
            },
            Self::Bin(lhs, _, _) => lhs.get_type(funs, structs, unions, vars)?,
            _ => return None,
        })
    }
}
