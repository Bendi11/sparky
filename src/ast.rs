//! Abstract syntax tree structures, the first representation of the program made by the compiler

use bitflags::bitflags;
use smallvec::SmallVec;
use string_interner::symbol::SymbolU32 as Symbol;

use crate::util::loc::Span;

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
pub enum AstNode {
    /// A function declaration with no definition
    FunDecl(FunProto),
    /// A function definition with both a prototype and body
    FunDef(FunProto, Vec<Ast>),
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
        ty: Option<UnresolvedType>,
        /// If the variable is mutable
        mutable: bool,
    }
}

#[derive(Clone, Debug)]
pub struct IfExpr {
    /// Conditional expression
    pub cond: Box<Ast>,
    /// The body of the if statement
    pub body: Vec<Ast>,
    /// Either another if statement or a body
    pub else_expr: ElseExpr,
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
        /// Names of any generic type arguments for the type
        generic_args: SmallVec<[Symbol ; 3]>,
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