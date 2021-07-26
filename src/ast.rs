use std::ops::Deref;

use crate::{
    code::Compiler,
    lex::{Op, Pos},
    types,
};

use super::Type;
use bitflags::bitflags;
use inkwell::types::StructType;
use log::debug;

bitflags! {
    pub struct Attributes: u8 {
        /// The item's value cannot change
        const CONST = 0b00000001;
        /// The item is defined elsewhere and is only being declared
        const EXT   = 0b00000010;
        /// The function is local to its object file
        const STATIC = 0b00000100;
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
#[derive(Debug, Clone)]
pub enum Ast {
    /// A function prototype with all needed information to call the function
    FunProto(FunProto),

    /// A function definition with the function signature and the body of the function
    FunDef(FunProto, Vec<AstPos>),

    /// A constant number literal
    NumLiteral(Type, String),

    /// A constant string literal
    StrLiteral(String),

    /// A binary operation  applied to two expressions
    Bin(Box<AstPos>, Op, Box<AstPos>),

    /// A struct definition with the defined type
    StructDec(types::Container),

    /// A union type definition
    UnionDec(types::Container),

    /// A constant struct literal
    StructLiteral {
        /// The struct type name
        name: String,

        /// The given field values
        fields: Vec<(String, AstPos)>,
    },

    /// Cast an expression to a type
    Cast(Box<AstPos>, Type),

    /// A struct or union field access
    MemberAccess(Box<AstPos>, String),

    /// An associated function is being called on a value
    AssocFunAccess(Box<AstPos>, String, Vec<AstPos>),

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
    Unary(Op, Box<AstPos>),

    /// A function call with expression arguments
    FunCall(String, Vec<AstPos>),

    /// Return statement
    Ret(Box<Option<AstPos>>),

    /// An if conditional statement
    If {
        /// The condition to check
        cond: Box<AstPos>,
        /// Code to execute on true
        true_block: Vec<AstPos>,
        /// Code to execute on false, if any
        else_block: Option<Vec<AstPos>>,
    },

    /// A while statement for looping
    While {
        /// The condition to check
        cond: Box<AstPos>,
        /// The block to loop over
        block: Vec<AstPos>,
    },

    /// A type definition aliasing an identifier to a typename
    TypeDef(String, Type),

    /// A namespace declaration with further statements inside
    Ns(Vec<String>, Vec<AstPos>),
}

/// The `AstPos` struct holds both an abstract syntax tree node and a position in the source file
#[derive(Debug, Clone)]
pub struct AstPos(pub Ast, pub Pos);

impl AstPos {
    #[inline(always)]
    pub const fn ast(&self) -> &Ast {
        &self.0
    }
}

impl Ast {
    /// Get the type of this expression, if any
    pub fn get_type<'a, 'b, 'c>(
        &'a self,
        compiler: &'b mut Compiler<'c>,
    ) -> Option<(Type, Option<StructType<'c>>)> {
        match match self {
            Self::FunCall(name, _) => compiler.get_fun(name)?.1.ret.clone(),
            Self::VarDecl {
                name: _,
                ty,
                attrs: _,
            } => ty.clone(),
            Self::Cast(_, ty) => ty.clone(),
            Self::VarAccess(name) => compiler.vars.get(name)?.1.clone(),
            Self::StructLiteral { name, fields: _ } => {
                Type::Struct(compiler.get_struct(name)?.1.clone())
            },
            Self::MemberAccess(first, item) => match first.0.get_type(compiler) {
                Some((Type::Struct(col), _)) | Some((Type::Union(col), _)) => match col
                    .fields
                    .unwrap()
                    .iter()
                    .find(|(name, _)| name == item) {
                        Some(field) => field,
                        None => {
                            debug!("Failed to get field {} type because the field does not exist (in member access expression)", item);
                            return None
                        }
                    }
                    .1
                    .clone(),
                None => {
                    debug!("Failed to get type of prefix expression in member access expression, prefix is {:?}", first);
                    return None
                }
                Some(other) => {
                    debug!("Failed to get type of prefix expression in member access, type is {:?}, which is not a struct type", other);
                    return None
                }
            },
            Self::NumLiteral(ty, _) => ty.clone(),
            Self::StrLiteral(_) => Type::Ptr(Box::new(Type::Integer {
                width: 8,
                signed: false,
            })), //char pointer for string literals
            Self::Unary(op, val) => match op {
                Op::Star => val.deref().0.get_type(compiler)?.0.deref_type()?,
                Op::And => val.deref().0.get_type(compiler)?.0.ptr_type(),
                _ => return None,
            },
            Self::Bin(lhs, _, _) => return lhs.0.get_type(compiler),
            _ => return None,
        } {
            Type::Unknown(name) => Some(match (compiler.get_struct(&name), compiler.get_union(&name), compiler.get_typedef(&name)) {
                (Some((ty, c)), _, _) => (Type::Struct(c), Some(ty)),
                (_, Some((ty, c)), _) => (Type::Union(c), Some(ty)),
                (_, _, Some(ty)) => (ty, None),
                (None, None, None) => {
                    debug!("Failed to get type of prefix expression in member access because the struct, union or typedef'd type {} does not exist", name);
                    return None
                }
            }),
            other => Some((other, None))
        }
    }
}
