use crate::util::loc::Span;


/// The main type used for a token lexed from a source file containing location information
/// and token data
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub span: Span,

}

/// All possible tokens lexed from a source string
#[derive(Clone, Debug)]
pub enum TokenData<'src> {
    Ident(&'src str),
    Number(&'src str),
    String(&'src str),
    OpenBracket(BracketType),
    CloseBracket(BracketType),
    Comma,
    Period,

}


/// Enumeration representing all accepted bracket characters
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BracketType {
    Brace,
    Parenthese,
    Bracket
}