use std::ops::Deref;

use crate::{
    code::{ns::Path, Compiler},
    lex::{Op, Pos},
    types,
};

use super::Type;
use bitflags::bitflags;
use log::debug;
use num_bigint::BigInt;

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

/// The `NumLiteral` struct holds a constant number value in a big integer, plus type information
#[derive(Debug, Clone)]
pub struct NumLiteral {
    /// The number literal value
    pub val: BigInt,

    /// If the number literal is signed
    pub signed: bool,

    /// The bit width of the number literal
    pub width: u8,
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

    /// An assembly function definition
    AsmFunDef(FunProto, String, String),

    /// A constant number literal
    NumLiteral(NumLiteral),

    /// A constant string literal
    StrLiteral(String),

    /// A binary operation  applied to two expressions
    Bin(Box<AstPos>, Op, Box<AstPos>),

    /// A struct definition with the defined type
    StructDec(types::Container),

    /// A union type definition
    UnionDec(types::Container),

    /// A constant value definition
    GlobalDef(Type, String, Option<Box<AstPos>>, Attributes),

    /// A constant struct literal
    StructLiteral {
        /// The struct type name
        name: String,

        /// The given field values
        fields: Vec<(String, AstPos)>,
    },

    /// Cast an expression to a type
    Cast(Box<AstPos>, Type),

    /// A struct or union field access, plus wether to dereference the left hand side
    MemberAccess(Box<AstPos>, String, bool),

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
    Ns(Path, Vec<AstPos>),

    /// Importing a namespace into scope
    Using(Path),

    /// Array member access with operation to get index
    Array(Box<AstPos>, Box<AstPos>),

    /// A switch statement for jump tables, plus a default case
    Switch(Box<AstPos>, Vec<(NumLiteral, Vec<AstPos>)>, Option<Vec<AstPos>>),
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

impl AstPos {
    /// Get the type of this expression, if any
    pub fn get_type(&self, compiler: &'_ Compiler<'_, '_>) -> Option<Type> {
        let ty = match self.ast() {
            Ast::FunCall(name, _) => compiler.get_fun(name)?.1.ret,
            Ast::VarDecl {
                name: _,
                ty,
                attrs: _,
            } => ty.clone(),
            Ast::Cast(_, ty) => ty.clone(),
            Ast::VarAccess(ref name) => compiler.vars.get(name)?.1.clone(),
            Ast::StructLiteral { name, fields: _ } => {
                Type::Struct(compiler.get_struct(name)?.1)
            },
            Ast::MemberAccess(first, item, deref) => match match deref {
                true => first.get_type(compiler).map(|ty| ty.deref_type().unwrap()),
                false => first.get_type(compiler)
            }  {
                Some(Type::Struct(col)) | Some(Type::Union(col)) => match col
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
            Ast::NumLiteral(l) => Type::Integer{width: l.width, signed: l.signed},
            Ast::StrLiteral(_) => Type::Ptr(Box::new(Type::Integer {
                width: 8,
                signed: false,
            })), //char pointer for string literals
            Ast::Unary(op, val) => match op {
                Op::Star => val.deref().get_type(compiler)?.deref_type()?,
                Op::And => val.deref().get_type(compiler)?.ptr_type(),
                _ => return None,
            },
            Ast::Bin(lhs, _, _) => return lhs.get_type(compiler),
            Ast::Array(ast, _) => return match ast.get_type(compiler)? {
                Type::Array(ty, _) => Some(*ty),
                _ => None
            },
            _ => return None,
        };
        //Resolve unknown types
        Self::apply_ptr_ty(&ty, |ty| match ty {
            Type::Unknown(name) => Some(
                match (
                    compiler.get_struct(&name),
                    compiler.get_union(&name),
                    compiler.get_typedef(&name),
                ) {
                    (Some((_, c)), None, None) => Type::Struct(c),
                    (None, Some((_, c)), None) => Type::Union(c),
                    (None, None, Some(ty)) => ty,
                    (_, _, _) => {
                        debug!("Failed to get type of prefix expression in member access because the struct, union or typedef'd type {} does not exist", name);
                        return None;
                    }
                },
            ),
            other => Some(other.clone()),
        })
    }

    fn apply_ptr_ty<F: FnOnce(&Type) -> Option<Type>>(ty: &Type, op: F) -> Option<Type> {
        match ty {
            Type::Ptr(ty) => Self::apply_ptr_ty(ty, op).map(|ty| ty.ptr_type()),
            Type::Array(ty, len) => {
                Self::apply_ptr_ty(ty, op).map(|ty| Type::Array(Box::new(ty), *len))
            }
            other => op(other),
        }
    }

    /// Check if this expression can be evaluated at compile-time
    pub const fn is_constexpr(&self) -> bool {
        match self.ast() {
            Ast::NumLiteral(_) => true,
            _ => false
        }
    }

    /// Get the constant expression value from this AST node
    pub fn get_constexpr_int<'c>(&self, compiler: &'_ Compiler<'c, '_>) -> Option<inkwell::values::IntValue<'c>> {
        match self.ast() {
            Ast::NumLiteral(literal) => {
                Some(compiler.ctx
                    .custom_width_int_type(literal.width as u32)
                    .const_int_arbitrary_precision(literal.val.to_u64_digits().1.as_slice())
                )
            },
            _ => None
        }
    } 
}
