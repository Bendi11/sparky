use std::{io::Write, fmt};

use crate::util::{files::CompiledFile, loc::Span};

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

    /// Display this token data using a source file
    pub fn display(&self, file: &CompiledFile) -> std::io::Result<()> {
        let start_line = *file
            .lines
            .get(self.span.from.line.get() as usize - 1)
            .expect("Invalid span line when displaying token");
        let startpos = start_line + self.span.from.col as usize;

        let end_line = *file
            .lines
            .get(self.span.to.line.get() as usize - 1)
            .expect("Invalid span line when displaying token");
        let endpos = end_line + self.span.to.col as usize;

        let mut stdout = std::io::stdout();
        let mut line = self.span.from.line.get();
        let mut buf = [0u8; 4];

        stdout.write_fmt(format_args!("{}: ", line))?;
        for character in file.text[startpos..=endpos].chars() {
            character.encode_utf8(&mut buf);
            stdout.write(&buf[..character.len_utf8()])?;

            if character == '\n' {
                line += 1;
                stdout.write_fmt(format_args!("{}: ", line))?;
            }
        }

        stdout.flush()
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
    /// :=
    Assign,
}

/// A binary or unary operator
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Op {
    Star,
    Div,
    Add,
    Sub,
    Mod,

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

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Star => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mod => write!(f, "%"),

            Self::AND => write!(f, "&"),
            Self::OR  => write!(f, "|"),
            Self::XOR => write!(f, "^"),
            Self::NOT => write!(f, "~"),

            Self::LogicalAnd => write!(f, "&&"),
            Self::LogicalOr  => write!(f, "||"),
            Self::LogicalNot => write!(f, "!"),

            Self::Greater   => write!(f, ">"),
            Self::GreaterEq => write!(f, ">="),
            Self::Less      => write!(f, "<"),
            Self::LessEq    => write!(f, "<="),
            Self::Eq        => write!(f, "="),

            Self::ShLeft  => write!(f, "<<"),
            Self::ShRight => write!(f, ">>"),
        }
    }
}

/// Enumeration representing all accepted bracket characters
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BracketType {
    Brace,
    Parenthese,
    Bracket,
}
