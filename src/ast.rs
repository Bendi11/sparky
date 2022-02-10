//! Abstract syntax tree structures, the first representation of the program made by the compiler

use std::fmt;

use std::{cmp::Eq, collections::HashMap, hash::Hash};

use bitflags::bitflags;
use num_bigint::BigInt;

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
#[derive(Clone, Debug)]
pub struct FunProto<T: Clone + Hash + Eq> {
    /// User-defined name of the function
    pub name: Symbol,
    /// Any flags that the function has
    pub flags: FunFlags,
    /// Argument name and types
    pub args: Vec<(Symbol, T)>,
    /// Return type of the function
    pub return_ty: T,
}

/// A node in an Abstract Syntax Tree
#[derive(Clone, PartialEq, Eq)]
pub enum AstNode<T = UnresolvedType>
where
    T: Clone + Hash + Eq,
{
    /// A variable / enum / constant / function access by name
    Access(SymbolPath),
    /// Member item access with the '.' operator
    MemberAccess(Box<Ast<T>>, Symbol),
    /// An array-like index expression using '[' ']'
    Index {
        object: Box<Ast<T>>,
        index: Box<Ast<T>>,
    },
    /// Function call with argument expressions
    FunCall(Box<Ast<T>>, Vec<Ast<T>>),
    /// If statement / expression
    IfExpr(IfExpr<T>),
    /// A variable declaration using the `let` or `mut` keywords
    VarDeclaration {
        /// The name of the variable being declared
        name: Symbol,
        /// Optionally specified type of the variable
        ty: Option<T>,
        /// If the variable is mutable
        mutable: bool,
    },
    /// A value is being assigned to another value
    Assignment {
        /// The left hand side of the assignment expression
        lhs: Box<Ast<T>>,
        /// The value being assigned to the left hand side
        rhs: Box<Ast<T>>,
    },
    /// A binary expression with LHS, operator, and RHS
    BinExpr(Box<Ast<T>>, Op, Box<Ast<T>>),
    /// A unary expression with only operator and RHS
    UnaryExpr(Op, Box<Ast<T>>),
    /// Phi returning a value from the current block
    PhiExpr(Box<Ast<T>>),
    /// Returning an optional expression from a function
    Return(Box<Ast<T>>),
    /// Casting an expression to a type
    CastExpr(T, Box<Ast<T>>),
    /// A floating or fixed point number literal
    NumberLiteral(NumberLiteral),
    /// A string literal value
    StringLiteral(String),
    /// A literal boolean value
    BooleanLiteral(bool),
    /// A tuple made up of multiple expressions
    TupleLiteral(Vec<Ast<T>>),
    /// An array literal with list of expressions for array elements
    ArrayLiteral(Vec<Ast<T>>),
    UnitLiteral,
    /// Breaking out of a loop
    Break,
    /// Continuing in a loop
    Continue,
    /// A block of statements
    Block(Vec<Ast<T>>),
    /// A match statement
    Match {
        //The expression being matched
        matched: Box<Ast<T>>,
        //The possible cases being tested for
        cases: Vec<(Ast<T>, Ast<T>)>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfExpr<T: Clone + Hash + Eq> {
    /// Conditional expression
    pub cond: Box<Ast<T>>,
    /// The body of the if statement
    pub body: Vec<Ast<T>>,
    /// Either another if statement or a body
    pub else_expr: Option<ElseExpr<T>>,
}

/// Enum representing what can come after an if expression's body
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ElseExpr<T: Clone + Hash + Eq> {
    ElseIf(Box<IfExpr<T>>),
    Else(Vec<Ast<T>>),
}

/// One node in an abstract syntax tree, containing an [AstNode] and additional location information used for
/// error messages later in the compiler
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ast<T = UnresolvedType>
where
    T: Clone + Hash + Eq,
{
    /// The AST node's data
    pub node: AstNode<T>,
    /// The span of the source string that this AST node occupies
    pub span: Span,
}

/// An enum representing all parseable definitions
#[derive(Clone, Debug)]
pub enum DefData {
    /// A function definition with body and prototype
    FunDef(FunProto<UnresolvedType>, Vec<Ast>),
    /// A function declaration with no body
    FunDec(FunProto<UnresolvedType>),
    /// A type alias binding a name to a type
    AliasDef {
        /// The alias that `aliased` can be accessed by
        name: Symbol,
        /// The aliased type
        aliased: UnresolvedType,
    },
    /// An imported module definition
    ImportDef { name: SymbolPath },
}
impl DefData {
    /// Get the name of this definition
    pub fn name(&self) -> Symbol {
        match self {
            Self::FunDef(proto, _) | Self::FunDec(proto) => proto.name,
            Self::AliasDef { name, .. } => *name,
            Self::ImportDef { name } => name.last(),
        }
    }
}

/// A structure holding both [DefData] and metadata
/// used for error messages like location in source
#[derive(Clone, Debug)]
pub struct Def {
    pub data: DefData,
    pub span: Span,
    pub file: FileId,
}

/// Structure representing a fully parsed module with easy access
/// to all defined types and functions
#[derive(Clone, Debug)]
pub struct ParsedModule {
    /// A map of names to all definitions in the module
    pub defs: HashMap<Symbol, Def>,
    /// All imports for this module
    pub imports: Vec<Symbol>,
    /// The name of the module
    pub name: Symbol,
    /// All children of this module
    pub children: HashMap<Symbol, ParsedModule>,
}

impl ParsedModule {
    /// Create a new empty module
    pub fn new(name: Symbol) -> Self {
        Self {
            defs: HashMap::new(),
            name,
            children: HashMap::new(),
            imports: vec![],
        }
    }
}

/// A number literal holding either a big integer or
/// floating point value
#[derive(Clone, Debug, PartialEq)]
pub enum NumberLiteral {
    Integer(BigInt, Option<NumberLiteralAnnotation>),
    Float(f64, Option<NumberLiteralAnnotation>),
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
    pub arg_tys: Vec<UnresolvedType>,
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
    Pointer(Box<UnresolvedType>),
    Array {
        elements: Box<UnresolvedType>,
        len: u64,
    },
    /// Unit type with only one value, like void in C or () in rust
    Unit,
    /// Tuple made up of multiple arbitrary types
    Tuple {
        /// The element types contained in the tuple
        elements: Vec<UnresolvedType>,
    },
    /// A structure with named members
    Struct {
        fields: Vec<(UnresolvedType, Symbol)>
    },
    /// A tagged union with variant types
    Enum {
        variants: Vec<UnresolvedType>,
    },
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
}

impl<T: std::fmt::Debug + Clone + Hash + Eq> std::fmt::Debug for AstNode<T> {
    fn fmt(&self, w: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Match { matched, cases: _ } => {
                write!(w, "MATCH {:?}", matched.node)?;
                writeln!(w, " {{")?;
                //for (literal, case_expr) in cases.iter() {}
                write!(w, "}}")
            }
            Self::Block(stmts) => {
                writeln!(w, "BLOCK {{")?;
                for stmt in stmts {
                    writeln!(w, "{:?}", stmt.node)?;
                }
                write!(w, "}}")
            }
            Self::ArrayLiteral(parts) => {
                write!(w, "ARRAY [ ")?;
                for part in parts.iter() {
                    write!(w, "{:?}, ", part.node)?;
                }
                write!(w, " ]")
            }
            Self::Break => write!(w, "BREAK"),
            Self::Continue => write!(w, "CONTINUE"),
            Self::NumberLiteral(num) => write!(w, "NUMBER LITERAL {:?}", num),
            Self::StringLiteral(string) => write!(w, "STRING LITERAL {:?}", string),
            Self::TupleLiteral(tuple) => {
                write!(w, "TUPLE LITERAL ( ")?;
                for element in tuple {
                    write!(w, "{:?}, ", element.node)?;
                }
                write!(w, " )")
            }
            Self::UnitLiteral => write!(w, "UNIT LITERAL ()"),
            Self::Return(expr) => {
                write!(w, "RETURN {:?}", expr.node)
            }
            Self::PhiExpr(expr) => {
                write!(w, "PHI {:?}", expr.node)
            }
            Self::CastExpr(cast, casted) => {
                write!(w, "CAST ${:?} {:?}", cast, casted.node)
            }
            Self::BooleanLiteral(boolean) => write!(w, "BOOL {}", boolean),
            Self::Assignment { lhs, rhs } => {
                write!(w, "ASSIGN {:?}", lhs.node)?;
                write!(w, " = {:?}", rhs.node)
            }
            Self::UnaryExpr(op, expr) => {
                write!(w, "UNARY {} {:?}", op, expr.node)
            }
            Self::VarDeclaration { name, ty, mutable } => write!(
                w,
                "VARDEC {} ({:?}) {}",
                if *mutable { "mut" } else { "let" },
                ty,
                name
            ),
            Self::Access(path) => {
                write!(w, "ACCESSS ")?;
                for part in path.iter() {
                    write!(w, "{}:", part)?;
                }
                Ok(())
            }
            Self::FunCall(called, args) => {
                write!(w, "FUNCALL {:?}", called.node)?;

                write!(w, "( ")?;
                for arg in args {
                    write!(w, "{:?}, ", arg.node)?;
                }
                write!(w, " )")
            }
            Self::MemberAccess(lhs, index) => {
                write!(w, "MEMBERACCESS {:?}", lhs.node)?;
                write!(w, ".")?;
                write!(w, "{}", index)
            }
            Self::BinExpr(lhs, op, rhs) => {
                write!(w, "BIN {:?}", lhs.node)?;
                write!(w, " {} ", op)?;
                write!(w, "{:?}", rhs.node)
            }
            Self::IfExpr(ifexpr) => {
                fn display_if<T: Clone + Eq + Hash + std::fmt::Debug>(
                    ifexpr: &IfExpr<T>,
                    w: &mut std::fmt::Formatter<'_>,
                ) -> std::fmt::Result {
                    write!(w, "IF {:?}", ifexpr.cond.node)?;
                    writeln!(w, " {{")?;
                    for expr in ifexpr.body.iter() {
                        writeln!(w, "{:?}", expr.node)?;
                    }
                    write!(w, "}}")?;

                    if let Some(ref else_expr) = ifexpr.else_expr {
                        match else_expr {
                            ElseExpr::ElseIf(another_if) => display_if(&*another_if, w),
                            ElseExpr::Else(stmts) => {
                                for expr in stmts.iter() {
                                    writeln!(w, "{:?}", expr.node)?;
                                }
                                write!(w, "}}")
                            }
                        }?
                    }
                    Ok(())
                }

                display_if(ifexpr, w)
            }
            Self::Index { object, index } => {
                write!(w, "INDEX {:?}", object.node)?;
                write!(w, " [ ")?;
                write!(w, "{:?}", index.node)?;
                write!(w, " ]")
            }
        }
    }
}
