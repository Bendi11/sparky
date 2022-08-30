//! Abstract syntax tree structures, the first representation of the program made by the compiler

use std::fmt;

use std::{cmp::Eq, hash::Hash};

use bitflags::bitflags;

use crate::Symbol;

use crate::{
    parse::token::Op,
    util::{files::FileId, loc::Span},
};

bitflags! {
    /// Structure holding flags of a function's prototype
    pub struct FunFlags: u8 {
        const EXTERN = 0b00000001;
    }
}

/// Structure containing a list of symbols separated by the colon
/// character, for example std:io:open
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SymbolPath {
    internal: SymbolPathInternal,
}

/// Enumeration allowing both a heap-allocated list of parts for a path
/// or a single stack-allocated [Symbol] for optimization, avoiding a
/// heap allocation for every identifier
///
/// Kept private becase the [Multiple](UnresolvedPathInternal::Multiple) variant
/// must always have at least one symbol and allowing public access risks allowing
/// that invariant
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum SymbolPathInternal {
    /// A path made of a single part
    Single(Symbol),
    /// A path made of multiple parts
    /// **WARNING**
    /// Must have at least one part
    Multiple(Vec<Symbol>),
}

/// An iterator over the items in an [UnresolvedPath]
pub enum PathIter<'a> {
    /// A single element path
    Single(std::iter::Once<Symbol>),
    /// Multiple path parts to iterate over
    Multiple(std::slice::Iter<'a, Symbol>),
}

impl PathIter<'_> {
    /// Return `true` if a call to next() will consume the last element of the path
    pub fn is_final(&self) -> bool {
        match self {
            Self::Single(s) if !s.len() == 1 => true,
            Self::Multiple(iter) if iter.len() == 1 => true,
            _ => false,
        }
    }
}

impl Iterator for PathIter<'_> {
    type Item = Symbol;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Single(once) => once.next(),
            Self::Multiple(parts) => parts.next().map(|x| *x),
        }
    }
}

impl ExactSizeIterator for PathIter<'_> {
    fn len(&self) -> usize {
        match self {
            Self::Single(sym) => sym.len(),
            Self::Multiple(iter) => iter.len(),
        }
    }
}

impl SymbolPath {
    /// Create a new path with only one identifier
    pub fn new(part: Symbol) -> Self {
        Self {
            internal: SymbolPathInternal::Single(part),
        }
    }

    /// Create a new path from a list of parts
    ///
    /// **Note**
    /// The `parts` argument must contain at least one Symbol
    pub fn new_parts(parts: &[Symbol]) -> Self {
        if parts.len() == 1 {
            Self {
                internal: SymbolPathInternal::Single(parts[0]),
            }
        } else {
            Self {
                internal: SymbolPathInternal::Multiple(parts.to_owned()),
            }
        }
    }

    /// Get the length of this path
    pub fn len(&self) -> usize {
        match self.internal {
            SymbolPathInternal::Single(_) => 1,
            SymbolPathInternal::Multiple(ref parts) => parts.len(),
        }
    }

    /// Retrieve the last identifier in the path
    pub fn last(&self) -> Symbol {
        match self.internal {
            SymbolPathInternal::Single(last) => last,
            SymbolPathInternal::Multiple(ref parts) => *parts.last().unwrap(),
        }
    }

    /// Return the first part of this path
    pub fn first(&self) -> Symbol {
        match self.internal {
            SymbolPathInternal::Single(first) => first,
            SymbolPathInternal::Multiple(ref parts) => *parts.first().unwrap(),
        }
    }

    /// Return an iterator over all parts of this path from first to last
    pub fn iter(&self) -> PathIter<'_> {
        match &self.internal {
            SymbolPathInternal::Single(single) => PathIter::Single(std::iter::once(*single)),
            SymbolPathInternal::Multiple(parts) => PathIter::Multiple(parts.into_iter()),
        }
    }
}

impl fmt::Display for SymbolPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.internal {
            SymbolPathInternal::Single(sym) => sym.fmt(f),
            SymbolPathInternal::Multiple(parts) => {
                let mut iter = parts.iter();
                loop {
                    if let Some(part) = iter.next() {
                        part.fmt(f)?;
                        if !(iter.len() == 0) {
                            write!(f, ":")?;
                        }
                    } else {
                        break;
                    }
                }
                Ok(())
            }
        }
    }
}

/// Data structure storing a function prototype
#[derive(Clone,)]
pub struct FunProto {
    /// User-defined name of the function
    pub name: Symbol,
    /// Any flags that the function has
    pub flags: FunFlags,
    /// Function's signature
    pub ty: UnresolvedFunType,
}

/// A let statement that either assigns a value to an expression or
/// creates a new variable
#[derive(Clone, PartialEq, Eq)]
pub struct Let {
    /// If this let expression was declared with the `mut` keyword
    pub mutable: bool,

    /// Type optionally used to aid inference
    pub ty: Option<UnresolvedType>,

    /// The expression that the let statement contains
    pub let_expr: Box<Expr>,

    /// Optional value being assigned to the expression
    pub assigned: Option<Box<Expr>>,
}

/// A match expression that matches an enum expression based on its type
#[derive(Clone, PartialEq, Eq)]
pub struct Match {
    //The expression being matched
    pub matched: Box<Expr>,
    //The possible cases being tested for
    pub cases: Vec<(UnresolvedType, Stmt)>,
}

/// A statement at the top level of a function
#[derive(Clone, PartialEq, Eq)]
pub enum StmtNode {
    /// A conditional statement with else - if chains
    If(If),
    /// A block with no purpose other than defining a new scope
    Block(Vec<Stmt>),
    /// A loop that iterates over the block of statements forever
    Loop(Vec<Stmt>),
    /// Matching an enum based on its type
    Match(Match),
    /// Calling a function by name
    Call(SymbolPath, Vec<Expr>),
    /// Break from something with a value
    Phi(Box<Expr>),
    /// Return a value from the currently defined function
    Return(Box<Expr>),

    /// Assignment or variable declaration
    Let(Let),

    /// Control flow keyword used to break from a loop
    Break,
    /// Control flow keyword used to continue to the next iteration of a loop
    Continue,
}

/// An expression that appears somewhere inside an [Stmt]
#[derive(Clone, PartialEq, Eq)]
pub enum ExprNode {
    /// Variable / function access by name or path
    Access(SymbolPath),
    /// Structure member access by field name
    Member(Box<Expr>, Symbol),
    /// Array-like index expression using '[' ']'
    Index(Box<Expr>, Box<Expr>),
    /// Expression calling a function expression with arguments
    Call(Box<Expr>, Vec<Expr>),
    /// Binary operator applied to two values
    Bin(Box<Expr>, Op, Box<Expr>),
    /// Unary operator with a single operand
    Unary(Op, Box<Expr>),
    /// Casting an expression to a different type explicitly
    Cast(UnresolvedType, Box<Expr>),
    /// A literal (does not mean compile-time constant) value
    Literal(Literal),
    /// A block of statements, must phi a value in all paths to be a validexpression
    Block(Vec<Stmt>),
    /// Looping over the contained block forever
    Loop(Vec<Stmt>),
    /// A match expression, must phi a value to be valid
    Match(Match),
    /// An if expression, must phi a value to be valid
    If(If),
}

/// An enumeration of all parseable literals
#[derive(Clone, PartialEq, Eq)]
pub enum Literal {
    /// Number literal containing optional annotation
    Number(NumberLiteral),
    /// String literal with all escape characters escaped
    String(String),
    /// Boolean literal
    Bool(bool),
    /// An array literal, all expressions must be of the same type
    Array(Vec<Expr>),
    /// A structure literal containing optional type for type checking and field
    /// assignments
    Struct {
        ty: Option<UnresolvedType>,
        fields: Vec<(Symbol, Expr)>,
    },
    /// Unit literal
    Unit,
}

/// An if expression or statement that tests the value of a boolean expression and
/// adjusts control flow accordingly
#[derive(Clone, PartialEq, Eq)]
pub struct If {
    /// Conditional expression
    pub cond: Box<Expr>,
    /// The body of the if statement
    pub body: Vec<Stmt>,
    /// Either another if statement or a body
    pub else_expr: Option<ElseExpr>,
}

/// Enum representing what can come after an if expression's body
#[derive(Clone, PartialEq, Eq)]
pub enum ElseExpr {
    ElseIf(Box<If>),
    Else(Vec<Stmt>),
}

/// One expression in an abstract syntax tree, containing an [ExprNode] and additional location information used for
/// error messages later in the compiler
#[derive(Clone, PartialEq, Eq)]
pub struct Expr {
    /// The AST node's data
    pub node: ExprNode,
    /// The span of the source string that this AST node occupies
    pub span: Span,
}

/// One statement in the abstract syntax tree, the top level syntax for a function body
#[derive(Clone, PartialEq, Eq)]
pub struct Stmt {
    /// The statement's data
    pub node: StmtNode,
    /// The span of the source file that this statement occupies
    pub span: Span,
}

/// A function definition with body consisting of multiple [Stmt]s
#[derive(Clone)]
pub struct FunDef {
    pub proto: FunProto,
    pub body: Vec<Stmt>,
}

/// An enum representing all parseable definitions
#[derive(Clone)]
pub enum DefData {
    /// A function definition with body and prototype
    FunDef(FunDef),
    /// A function declaration with no body
    FunDec(FunProto),
    /// A type alias binding a name to a type
    AliasDef {
        /// The alias that `aliased` can be accessed by
        name: Symbol,
        /// The aliased type
        aliased: UnresolvedType,
    },
    /// An imported module definition
    ImportDef { name: SymbolPath },
    /// A global value
    Global {
        name: SymbolPath,
        comptime: bool,
        val: Option<Expr>,
        ty: Option<UnresolvedType>,
    }
}
impl DefData {
    /// Get the name of this definition
    pub fn name(&self) -> Symbol {
        match self {
            Self::FunDef(FunDef{ proto, ..} ) | Self::FunDec(proto) => proto.name,
            Self::AliasDef { name, .. } => *name,
            Self::ImportDef { name } => name.last(),
            Self::Global { name, .. } => name.last(),
        }
    }
}

/// A structure holding both [DefData] and metadata
/// used for error messages like location in source
#[derive(Clone)]
pub struct Def {
    pub data: DefData,
    /// Span in the file that this def was defined
    pub span: Span,
    /// File that this definition appeared in
    pub file: FileId,
}

/// Structure representing a fully parsed module with easy access
/// to all defined types and functions
#[derive(Clone)]
pub struct ParsedModule {
    /// A map of names to all definitions in the module
    pub defs: Vec<Def>,
    /// All imports for this module
    pub imports: Vec<Symbol>,
    /// The name of the module
    pub name: Symbol,
    /// All children of this module
    pub children: Vec<ParsedModule>,
}

impl ParsedModule {
    /// Create a new empty module
    pub fn new(name: Symbol) -> Self {
        Self {
            defs: Vec::new(),
            name,
            children: Vec::new(),
            imports: vec![],
        }
    }
}

/// A number literal holding either a big integer or
/// floating point value
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberLiteral {
    Integer(BigInt, Option<NumberLiteralAnnotation>),
    Float(f64, Option<NumberLiteralAnnotation>),
}

/// A big integer that can hold any number literal expressed in spark source
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BigInt {
    pub val: u64,
    pub sign: bool,
}

impl std::cmp::Eq for NumberLiteral {}

impl NumberLiteral {
    /// Get user-defined annotated type of this number literal
    pub fn annotation(&self) -> Option<NumberLiteralAnnotation> {
        match self {
            Self::Integer(_, annotation) | Self::Float(_, annotation) => *annotation,
        }
    }
}

/// The type added to the end of a number literal
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NumberLiteralAnnotation {
    F32,
    F64,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}

/// Type representing a function's type in spark
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UnresolvedFunType {
    /// The return type of the function
    pub return_ty: UnresolvedType,
    /// What argument types this function takes
    pub arg_tys: Vec<(UnresolvedType, Option<Symbol>)>,
}

/// All types in the [AstNode] enumeration are represented by the `UnresolvedType` type, as
/// user-defined types are resolved when lowering the AST to IR
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnresolvedType {
    Integer {
        /// How large in bits is the integer type
        width: IntegerWidth,
        /// Is the integer type signed or unsigned
        signed: bool,
    },
    /// True / False type
    Bool,
    /// A function type
    Fun(Box<UnresolvedFunType>),
    Float {
        /// If this is an f32 or an f64
        doublewide: bool,
    },
    /// Pointer to another defined type
    Pointer(Box<UnresolvedType>),
    /// Array with one element type and constant length
    Array {
        elements: Box<UnresolvedType>,
        len: u64,
    },
    /// Unit type with only one value, like void in C or () in rust
    Unit,
    /// A structure with named members
    Struct {
        fields: Vec<(UnresolvedType, Symbol)>,
    },
    /// A tagged union with variant types
    Enum { variants: Vec<UnresolvedType> },
    /// User-defined identifier
    UserDefined {
        /// The name of the user-defined type
        name: SymbolPath,
    },
}

/// Enumeration for all possible integer bit widths in the [UnresolvedType] enum
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntegerWidth {
    Eight = 8,
    Sixteen = 16,
    ThirtyTwo = 32,
    SixtyFour = 64,
    PtrSize = 0,
}
