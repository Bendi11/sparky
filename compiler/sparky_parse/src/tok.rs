use sparky_span::Span;

/// A token with additional data that is common to all tokens
#[derive(Clone, Copy, Debug)]
pub struct Token {
    /// Span that this token occupies in the source file
    pub(crate) span: Span,
    pub(crate) kind: TokenKind,
}

/// Token data without any common data, see [Token]
#[derive(Clone, Copy, Debug)]
pub enum TokenKind {
    /// An identifier - can be a keyword, symbol name- etc.
    Ident(Span),
    /// A number literal with prefix and span of numeral characters
    Number {
        prefix: Option<NumLitPrefix>,
        numerals: Span,
    },
    /// '.' character
    Dot,
    /// ',' character
    Comma,
    /// '->' character
    Arrow,
    /// An opening brace of the three supported kinds
    OpenBrace(BraceKind),
    /// A closing brace
    CloseBrace(BraceKind),
    /// Character literal, span is the span of text inside the literal which may be two characters
    /// if escaped
    CharLiteral(Option<Span>),
    /// String literal, span is the span of text inside the double quotes
    StringLiteral(Option<Span>),
    /// Assignment '='
    Assign,
    /// All arithmetic and logical operators
    Op(OperatorKind),
    
    /// ';' character
    Semicolon,
}

/// All prefixes that can precede a number literal
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumLitPrefix {
    /// 0x
    Hex,
    /// 0b
    Bin,
    /// 0o
    Oct,
}

/// Enumeration over all brace types- (), [], {}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BraceKind {
    /// {}
    Curly,
    /// []
    Square,
    /// ()
    Smooth,
}

/// Enumeration over all supported arithmetic and logical operators
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperatorKind {
    /// +
    Add,
    /// -
    Sub,
    /// /
    Div,
    /// *
    Mul,
    /// %
    Mod,
    
    /// &
    BitAnd,
    /// |
    BitOr,
    /// ^
    BitXor,
    /// ~
    BitNot,
    
    /// &&
    LogicalAnd,
    /// ||
    LogicalOr,
    /// !
    LogicalNot,
    
    /// >>
    ShRight,
    /// <<
    ShLeft,

    /// <
    LessThan,
    /// >
    GreaterThan,
    /// <=
    LessEq,
    /// >=
    GreaterEq,
    /// ==
    Equal,
}

impl NumLitPrefix {
    /// Get the radix of the numbering system denoted by `self`
    pub const fn radix(&self) -> u32 {
        match self {
            Self::Bin => 2,
            Self::Oct => 8,
            Self::Hex => 16,
        }
    }
}
