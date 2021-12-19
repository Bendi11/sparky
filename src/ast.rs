//! Abstract syntax tree structures, the first representation of the program made by the compiler

use std::collections::HashMap;

use bitflags::bitflags;
use num_bigint::BigInt;

use string_interner::symbol::SymbolU32 as Symbol;

use crate::{util::loc::Span, parse::token::Op};

bitflags! {
    /// Structure holding flags of a function's prototype
    pub struct FunFlags: u8 {
        const EXTERN = 0b00000001;
    }
}

/// Data structure storing a function prototype
#[derive(Clone, Debug)]
pub struct FunProto {
    /// User-defined name of the function
    pub name: Symbol,
    /// Any flags that the function has
    pub flags: FunFlags,
    /// Argument name and types
    pub args: Vec<(Symbol, UnresolvedType)>,
    /// Return type of the function
    pub return_ty: UnresolvedType,
}


/// A node in an Abstract Syntax Tree
#[derive(Clone, Debug)]
pub enum AstNode<Type = UnresolvedType> 
where Type: Clone + std::fmt::Debug {
    /// A variable access by name
    VarAccess(Symbol),
    /// Member item access with the '.' operator
    MemberAccess(Box<Ast>, Symbol),
    /// An array-like index expression using '[' ']'
    Index {
        object: Box<Ast>,
        index: Box<Ast>,
    },
    /// Function call with argument expressions
    FunCall(Symbol, Vec<Ast>),
    /// If statement / expression
    IfExpr(IfExpr),
    /// A variable declaration using the `let` or `mut` keywords
    VarDeclaration {
        /// The name of the variable being declared
        name: Symbol,
        /// Optionally specified type of the variable
        ty: Option<Type>,
        /// If the variable is mutable
        mutable: bool,
        /// If an assignment was given, assign this expression
        assigned: Option<Box<Ast>>,
    },
    /// A binary expression with LHS, operator, and RHS
    BinExpr(Box<Ast>, Op, Box<Ast>),
    /// A unary expression with only operator and RHS
    UnaryExpr(Op, Box<Ast>),
    /// Phi returning a value from the current block
    PhiExpr(Box<Ast>),
    /// Casting an expression to a type
    CastExpr(Type, Box<Ast>),
    /// A floating or fixed point number literal
    NumberLiteral(NumberLiteral),
    /// A string literal value
    StringLiteral(String),
    /// A literal boolean value
    BooleanLiteral(bool),
    /// Casting an expression to the given type
    Cast(Type, Box<Ast>),
}

#[derive(Clone, Debug)]
pub struct IfExpr {
    /// Conditional expression
    pub cond: Box<Ast>,
    /// The body of the if statement
    pub body: Vec<Ast>,
    /// Either another if statement or a body
    pub else_expr: Option<ElseExpr>,
}

/// Enum representing what can come after an if expression's body
#[derive(Clone, Debug)]
pub enum ElseExpr {
    ElseIf(Box<IfExpr>),
    Else(Vec<Ast>)
}

/// One node in an abstract syntax tree, containing an [AstNode] and additional location information used for
/// error messages later in the compiler
#[derive(Clone, Debug)]
pub struct Ast {
    /// The AST node's data
    pub node: AstNode,
    /// The span of the source string that this AST node occupies
    pub span: Span,
}

/// An enum representing all parseable definitions
#[derive(Clone, Debug)]
pub enum DefData {
    /// A function definition with body and prototype
    FunDef(FunProto, Vec<Ast>),
    /// A function declaration with no body
    FunDec(FunProto),
    /// A type alias binding a name to a type
    AliasDef {
        /// The alias that `aliased` can be accessed by
        name: Symbol,
        /// The aliased type
        aliased: UnresolvedType
    },
    /// A structure type definition
    StructDef {
        /// The name of the structure type
        name: Symbol,
        /// A map of all fields in the type
        fields: HashMap<Symbol, UnresolvedType>
    },
    /// An enumeration definition
    EnumDef {
        /// The name of the enum
        name: Symbol,
        /// All variants of this enum
        variants: Vec<UnresolvedType>,
    }
}
impl DefData {
    /// Get the name of this definition
    pub const fn name(&self) -> Symbol {
        match self {
            Self::FunDef(proto, _) | Self::FunDec(proto) => proto.name,
            Self::StructDef {
                name,
                ..
            } => *name,
            Self::EnumDef{name, ..} => *name,
            Self::AliasDef{name, ..} => *name,
        }
    }
}

/// A structure holding both [DefData] and metadata
/// used for error messages like location in source
#[derive(Clone, Debug)]
pub struct Def {
    pub data: DefData,
    pub span: Span,
}

/// Structure representing a fully parsed module with easy access 
/// to all defined types and functions
#[derive(Clone, Debug)]
pub struct ParsedModule {
    /// A map of names to all definitions in the module
    pub defs: HashMap<Symbol, Def>,
    /// The name of the module
    pub name: Symbol,
}

impl ParsedModule {
    /// Create a new empty module
    pub fn new(name: Symbol) -> Self {
        Self {
            defs: HashMap::new(),
            name,
        }
    }
}

/// A number literal holding either a big integer or
/// floating point value
#[derive(Clone, Debug)]
pub enum NumberLiteral {
    Integer(BigInt),
    Float(f64),
}

/// All types in the [AstNode] enumeration are represented by the `UnresolvedType` type, as
/// user-defined types are resolved when lowering the AST to IR
#[derive(Clone, Debug)]
pub enum UnresolvedType {
    Integer {
        /// How large in bits is the integer type
        width: IntegerWidth,
        /// Is the integer type signed or unsigned
        signed: bool,
    },
    Float {
        /// If this is an f32 or an f64
        doublewide: bool,
    },
    Pointer(Box<UnresolvedType>),
    Array {
        elements: Box<UnresolvedType>,
        len: u64
    },
    /// Unit type with only one value, like void in C or () in rust
    Unit,
    /// User-defined identifier
    UserDefined {
        /// The name of the user-defined type
        name: Symbol,
    },
}

/// Enumeration for all possible integer bit widths in the [UnresolvedType] enum
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum IntegerWidth {
    Eight = 8,
    Sixteen = 16,
    ThirtyTwo = 32,
    SixtyFour = 64
}
