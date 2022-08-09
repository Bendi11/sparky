use std::fmt;

use crate::util::loc::Span;

/// The main type used for a token lexed from a source file containing location information
/// and token data
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<'src> {
    /// Segment of source code this token is from
    pub span: Span,
    /// Specific token information
    pub data: TokenData<'src>,
}

impl<'src> Token<'src> {
    /// Create a new token from a span and token data
    pub fn new(span: impl Into<Span>, data: TokenData<'src>) -> Self {
        Self {
            span: span.into(),
            data,
        }
    }
}

/// All possible tokens lexed from a source string
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenData<'src> {
    /// An identifier or keyword string
    Ident(&'src str),
    /// A decimal, octal, hexadecimal, or binary number
    Number(&'src str),
    /// A user-defined string literal not including start and end quotes
    String(&'src str),
    /// A user-defined character literal not including start or end quotes
    Char(&'src str),
    /// Any opening brace character
    OpenBracket(BracketType),
    /// Any closing brace character
    CloseBracket(BracketType),
    /// The , character
    Comma,
    /// The . character
    Period,
    /// ->
    Arrow,
    /// A unary or binary operator token
    Op(Op),
    // :
    Colon,
    // $
    Dollar,
    /// =
    Assign,
    /// #
    Pound,
}

impl fmt::Display for TokenData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(name) => write!(f, "identifier: \"{}\"", name),
            Self::Number(num) => write!(f, "number: {}", num),
            Self::String(literal) => write!(f, "string literal: \"{}\"", literal),
            Self::Char(character) => write!(f, "character literal: '{}'", character),
            Self::OpenBracket(ty) => write!(
                f,
                "'{}'",
                match ty {
                    BracketType::Smooth => '(',
                    BracketType::Curly => '{',
                    BracketType::Square => '[',
                }
            ),
            Self::CloseBracket(ty) => write!(
                f,
                "'{}'",
                match ty {
                    BracketType::Smooth => ')',
                    BracketType::Curly => '}',
                    BracketType::Square => ']',
                }
            ),
            Self::Comma => write!(f, "','"),
            Self::Period => write!(f, "'.'"),
            Self::Arrow => write!(f, "->"),
            Self::Op(op) => write!(f, "'{}'", op),
            Self::Colon => write!(f, "':'"),
            Self::Dollar => write!(f, "'$'"),
            Self::Assign => write!(f, "'='"),
            Self::Pound => write!(f, "'#'"),
        }
    }
}

/// A binary or unary operator
#[derive(Clone, Debug, Copy, PartialEq, Eq,)]
pub enum Op {
    Star,
    Div,
    Mod,

    Add,
    Sub,

    AND,
    OR,
    XOR,
    NOT,

    LogicalAnd,
    LogicalOr,
    LogicalNot,

    Greater,
    GreaterEq,
    Less,
    LessEq,
    Eq,

    ShLeft,
    ShRight,
}

impl Op {
    /// Get the precedence for this operator
    pub const fn precedence(&self) -> usize {
        match self {
            Self::LogicalNot | Self::NOT => 11,
            Self::Star | Self::Div | Self::Mod => 10,
            Self::Add | Self::Sub => 9,
            Self::ShLeft | Self::ShRight => 8,
            Self::Less | Self::LessEq | Self::Greater | Self::GreaterEq => 7,
            Self::Eq => 6,
            Self::AND => 5,
            Self::XOR => 4,
            Self::OR => 3,
            Self::LogicalAnd => 2,
            Self::LogicalOr => 1,
        }
    }
}

impl std::cmp::PartialOrd for Op {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.precedence().partial_cmp(&other.precedence())
    }
}

impl std::cmp::Ord for Op {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.precedence().cmp(&other.precedence())
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Star => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mod => write!(f, "%"),

            Self::AND => write!(f, "&"),
            Self::OR => write!(f, "|"),
            Self::XOR => write!(f, "^"),
            Self::NOT => write!(f, "~"),

            Self::LogicalAnd => write!(f, "&&"),
            Self::LogicalOr => write!(f, "||"),
            Self::LogicalNot => write!(f, "!"),

            Self::Greater => write!(f, ">"),
            Self::GreaterEq => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),
            Self::Eq => write!(f, "=="),

            Self::ShLeft => write!(f, "<<"),
            Self::ShRight => write!(f, ">>"),
        }
    }
}

/// Enumeration representing all accepted bracket characters
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BracketType {
    Curly,
    Smooth,
    Square,
}
