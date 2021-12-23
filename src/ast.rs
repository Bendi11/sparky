//! Abstract syntax tree structures, the first representation of the program made by the compiler

use std::{collections::HashMap, io::Write};

use bitflags::bitflags;
use num_bigint::BigInt;

use string_interner::{symbol::SymbolU32 as Symbol, StringInterner};

use crate::{util::{loc::Span, files::FileId}, parse::token::Op};

bitflags! {
    /// Structure holding flags of a function's prototype
    pub struct FunFlags: u8 {
        const EXTERN = 0b00000001;
    }
}

/// Structure containing a list of symbols separated by the colon
/// character, for example std:io:open 
#[derive(Clone, Debug)]
pub struct SymbolPath {
    internal: SymbolPathInternal 
}

/// Enumeration allowing both a heap-allocated list of parts for a path
/// or a single stack-allocated [Symbol] for optimization, avoiding a 
/// heap allocation for every identifier
///
/// Kept private becase the [Multiple](UnresolvedPathInternal::Multiple) variant 
/// must always have at least one symbol and allowing public access risks allowing
/// that invariant
#[derive(Clone, Debug)]
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
    Multiple(std::slice::Iter<'a, Symbol>)
}

impl Iterator for PathIter<'_> {
    type Item = Symbol;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Single(once) => once.next(),
            Self::Multiple(parts) => parts.next().map(|x| *x)
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
        Self { internal: SymbolPathInternal::Single(part) }
    }
    
    /// Create a new path from a list of parts
    ///
    /// **Note**
    /// The `parts` argument must contain at least one Symbol
    pub fn new_parts(parts: &[Symbol]) -> Self {
        if parts.len() == 1 {
            Self { internal: SymbolPathInternal::Single(parts[0]) }
        } else {
            Self { internal: SymbolPathInternal::Multiple(parts.to_owned()) }
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
            SymbolPathInternal::Multiple(ref parts) => *parts.last().unwrap()
        }
    }
    
    /// Return the first part of this path
    pub fn first(&self) -> Symbol {
        match self.internal {
            SymbolPathInternal::Single(first) => first,
            SymbolPathInternal::Multiple(ref parts) => *parts.first().unwrap()
        }
    }
    
    /// Return an iterator over all parts of this path from first to last
    pub fn iter(&self) -> PathIter<'_> {
        match &self.internal {
            SymbolPathInternal::Single(single) => PathIter::Single(std::iter::once(*single)),
            SymbolPathInternal::Multiple(parts) => PathIter::Multiple(parts.into_iter())
        }
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
where Type: Clone {
    /// A variable / enum / constant / function access by name
    Access(SymbolPath),
    /// Member item access with the '.' operator
    MemberAccess(Box<Ast>, Symbol),
    /// An array-like index expression using '[' ']'
    Index {
        object: Box<Ast>,
        index: Box<Ast>,
    },
    /// Function call with argument expressions
    FunCall(Box<Ast>, Vec<Ast>),
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
    },
    /// A value is being assigned to another value
    Assignment {
        /// The left hand side of the assignment expression
        lhs: Box<Ast>,
        /// The value being assigned to the left hand side
        rhs: Box<Ast>,
    },
    /// A binary expression with LHS, operator, and RHS
    BinExpr(Box<Ast>, Op, Box<Ast>),
    /// A unary expression with only operator and RHS
    UnaryExpr(Op, Box<Ast>),
    /// Phi returning a value from the current block
    PhiExpr(Box<Ast>),
    /// Returning an optional expression from a function
    Return(Box<Ast>),
    /// Casting an expression to a type
    CastExpr(Type, Box<Ast>),
    /// A floating or fixed point number literal
    NumberLiteral(NumberLiteral),
    /// A string literal value
    StringLiteral(String),
    /// A literal boolean value
    BooleanLiteral(bool),
    /// A tuple made up of multiple expressions
    TupleLiteral(Vec<Ast>),
    /// An array literal with list of expressions for array elements
    ArrayLiteral(Vec<Ast>),
    /// Breaking out of a loop
    Break,
    /// Continuing in a loop
    Continue,
    /// An infinite loop with body
    Loop(Vec<Ast>),
    /// A block of statements
    Block(Vec<Ast>),
    /// A match statement
    Match {
        //The expression being matched
        matched: Box<Ast>,
        //The possible cases being tested for
        cases: HashMap<Ast, Ast>,
    }
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
pub struct Ast<Type = UnresolvedType>
where Type: Clone {
    /// The AST node's data
    pub node: AstNode<Type>,
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
    },
    /// An imported module definition
    ImportDef {
        name: SymbolPath
    }
}
impl DefData {
    /// Get the name of this definition
    pub fn name(&self) -> Symbol {
        match self {
            Self::FunDef(proto, _) | Self::FunDec(proto) => proto.name,
            Self::StructDef {
                name,
                ..
            } => *name,
            Self::EnumDef{name, ..} => *name,
            Self::AliasDef{name, ..} => *name,
            Self::ImportDef{name} => name.last(),
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
    /// All imports for this module
    pub imports: Vec<Symbol>,
    /// The name of the module
    pub name: Symbol,
    /// The file that this module was parsed from
    pub file: FileId,
    /// All children of this module
    pub children: HashMap<Symbol, ParsedModule>,
}

impl ParsedModule {
    /// Create a new empty module
    pub fn new(name: Symbol, file: FileId) -> Self {
        Self {
            defs: HashMap::new(),
            name,
            file,
            children: HashMap::new(),
            imports: vec![],
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

/// Type representing a function's type in spark
#[derive(Clone, Debug)]
pub struct UnresolvedFunType {
    /// The return type of the function
    pub return_ty: UnresolvedType,
    /// What argument types this function takes
    pub arg_tys: Vec<UnresolvedType>,
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
        len: u64
    },
    /// Unit type with only one value, like void in C or () in rust
    Unit,
    /// Tuple made up of multiple arbitrary types
    Tuple {
        /// The element types contained in the tuple
        elements: Vec<UnresolvedType>,
    },
    /// User-defined identifier
    UserDefined {
        /// The name of the user-defined type
        name: SymbolPath,
    },
}

/// Enumeration for all possible integer bit widths in the [UnresolvedType] enum
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum IntegerWidth {
    Eight,
    Sixteen,
    ThirtyTwo,
    SixtyFour,
}

impl<T: std::fmt::Debug + Clone> AstNode<T> {
    pub fn display<W: Write>(&self, w: &mut W, interner: &StringInterner, numtabs: u16) -> std::io::Result<()> {
        match self {
            Self::Match {matched, cases} => {
                write!(w, "MATCH ")?;
                matched.node.display(w, interner, numtabs)?;
                writeln!(w, " {{")?;
                for (literal, case_expr) in cases.iter() {

                }
                write!(w, "}}")
            }
            Self::Block(stmts) => {
                writeln!(w, "BLOCK {{")?;
                for stmt in stmts {
                    write!(w, "{}", (0..numtabs+1).into_iter().map(|_|"  ").collect::<Vec<_>>().join(""))?;
                    stmt.node.display(w, interner, numtabs)?;
                    writeln!(w)?;
                }
                write!(w, "}}")
            },
            Self::ArrayLiteral(parts) => {
                write!(w, "ARRAY [ ")?;
                for part in parts.iter() {
                    part.node.display(w, interner, numtabs)?;
                    write!(w, ", ")?;
                }
                write!(w, " ]")
            }
            Self::Loop(body) => {
                writeln!(w, "LOOP {{")?;
                for stmt in body.iter() {
                    write!(w, "{}", (0..numtabs+1).into_iter().map(|_|"  ").collect::<Vec<_>>().join(""))?;
                    stmt.node.display(w, interner, numtabs + 1)?;
                    writeln!(w)?;
                }
                write!(w, "}}")
            }
            Self::Break => write!(w, "BREAK"),
            Self::Continue => write!(w, "CONTINUE"),
            Self::NumberLiteral(num) => write!(w, "NUMBER LITERAL {:?}", num),
            Self::StringLiteral(string) => write!(w, "STRING LITERAL {}", string),
            Self::TupleLiteral(tuple) => {
                write!(w, "TUPLE LITERAL ( ")?;
                for element in tuple {
                    element.node.display(w, interner, numtabs)?;
                    write!(w, ", ")?;
                }
                write!(w, " )")
            },
            Self::Return(expr) => {
                write!(w, "RETURN ")?;
                expr.node.display(w, interner, numtabs)
            },
            Self::PhiExpr(expr) => {
                write!(w, "PHI ")?;
                expr.node.display(w, interner, numtabs)
            },
            Self::CastExpr(cast, casted) => {
                write!(w, "CAST ${:?} ", cast)?;
                casted.node.display(w, interner, numtabs)
            },
            Self::BooleanLiteral(boolean) => write!(w, "BOOL {}", boolean),
            Self::Assignment{lhs, rhs} => {
                write!(w, "ASSIGN ")?;
                lhs.node.display(w, interner, numtabs)?;
                write!(w, " = ")?;
                rhs.node.display(w, interner, numtabs)
            },
            Self::UnaryExpr(op, expr) => {
                write!(w, "UNARY {} ", op)?;
                expr.node.display(w, interner, numtabs)
            },
            Self::VarDeclaration {
                name, ty, mutable
            } => write!(w, "VARDEC {} ({:?}) {}", if *mutable { "mut" } else { "let" }, ty, interner.resolve(*name).unwrap() ),
            Self::Access(path) => {
                write!(w, "ACCESSS ")?;
                for part in path.iter() {
                    write!(w, "{}:", interner.resolve(part).unwrap())?;
                }
                Ok(())
            },
            Self::FunCall(called, args) => {
                write!(w, "FUNCALL ")?;
                called.node.display(w, interner, numtabs)?;

                write!(w, "( ")?;
                for arg in args {
                    arg.node.display(w, interner, numtabs)?;
                    write!(w, ", ")?;
                }
                write!(w, " )")
            },
            Self::MemberAccess(lhs, index) => {
                write!(w, "MEMBERACCESS ")?;
                lhs.node.display(w, interner, numtabs)?;
                write!(w, ".")?;
                write!(w, "{}", interner.resolve(*index).unwrap())
            },
            Self::BinExpr(lhs, op, rhs) => {
                write!(w, "BIN ")?;
                lhs.node.display(w, interner, numtabs)?;
                write!(w, " {} ", op)?;
                rhs.node.display(w, interner, numtabs)
            },
            Self::IfExpr(ifexpr) => {
                fn display_if<W: Write>(ifexpr: &IfExpr, w: &mut W, interner: &StringInterner, numtabs: u16) -> std::io::Result<()> {
                    write!(w, "IF ")?;
                    ifexpr.cond.node.display(w, interner, numtabs)?;
                    writeln!(w, " {{")?;
                    for expr in ifexpr.body.iter() {
                        write!(w, "{}", (0..numtabs + 1).into_iter().map(|_| "  ").collect::<Vec<_>>().join(""))?;
                        expr.node.display(w, interner, numtabs + 1)?;
                        writeln!(w)?;
                    }
                    write!(w, "}}")?;
                    
                    if let Some(ref else_expr) = ifexpr.else_expr {
                        write!(w, "{}", (0..numtabs).into_iter().map(|_| "  ").collect::<Vec<_>>().join(""))?;
                        match else_expr {
                            ElseExpr::ElseIf(another_if) => display_if(&*another_if, w, interner, numtabs),
                            ElseExpr::Else(stmts) => {
                                for expr in stmts.iter() {
                                    write!(w, "{}", (0..numtabs + 1).into_iter().map(|_| "  ").collect::<Vec<_>>().join(""))?;
                                    expr.node.display(w, interner, numtabs + 1)?;
                                    writeln!(w)?;
                                }
                                write!(w, "}}")
                            }
                        }?
                    }
                    Ok(())
                }

                display_if(ifexpr, w, interner, numtabs)
            },
            Self::Index{object, index} => {
                write!(w, "INDEX ")?;
                object.node.display(w, interner, numtabs)?;
                write!(w, " [ ")?;
                index.node.display(w, interner, numtabs)?;
                write!(w, " ]")
            }
        }
    }
}
