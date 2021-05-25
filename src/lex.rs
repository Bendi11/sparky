//! The `lex` module provides the [Lexer](struct@lex::Lexer) struct; an efficient zero copy lexer that
//! performs the first pass over a source file; turning the raw text into a token stream
//!

use std::fmt;
use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

/// The `TokenType` enumerates every type of token that can be lexed from the input source file
/// and is parsed into an AST by the parser
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    /// An opening left brace: any of {, (, or [
    LeftBrace(char),
    /// Any closing right brace like }, ], or )
    RightBrace(char),

    /// Any unknown identifier, the code generator will ensure that this is a valid word at compile time
    Word(String),

    /// A string literal enclosed in double quotes
    StrLiteral(String),
    /// A number literal that is not floating point
    NumLiteral(i64),

    /// The literal comma character ','; used to separate arguments; tuple fields, etc.
    Comma,
    /// Semicolon used to begin declarations
    Semicolon,

    /// An internal token used for error messages when lexing
    Error(String),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LeftBrace(left) => write!(f, "Open Brace {}", left),
            Self::RightBrace(right) => write!(f, "Closing Brace {}", right),
            Self::Word(ident) => write!(f, "Word: {}", ident),
            Self::StrLiteral(s) => write!(f, "String Literal: \"{}\"", s),
            Self::NumLiteral(num) => write!(f, "Number Literal: {}", num),
            Self::Comma => write!(f, "Comma"),
            Self::Semicolon => write!(f, "Semicolon"),
            Self::Error(err) => write!(f, "Error Lexing: {}", err),
        }
    }
}

/// The `Token` struct stores a [TokenType](enum@TokenType) enumeration representing the type of token,
/// and metadata like the line of the token
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    /// The line number that this token was lexed on
    pub line: usize,
    /// The token that was actually lexed
    pub token: TokenType,
}

impl Token {
    /// Create a new `Token` using the given line number and token type
    #[inline]
    pub const fn new(line: usize, token: TokenType) -> Self {
        Self { line, token }
    }

    /// Check if this token is the same as another
    pub fn is(&self, is: TokenType) -> bool {
        std::mem::discriminant(&self.token) == std::mem::discriminant(&is)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Line #{}: {}", self.line, self.token)
    }
}

impl PartialEq<TokenType> for Token {
    //Implicitly compare a Token and a TokenType
    fn eq(&self, tok: &TokenType) -> bool {
        self.token.eq(tok)
    }
}

impl AsRef<TokenType> for Token {
    //Convert a token to a TokenType
    fn as_ref(&self) -> &TokenType {
        &self.token
    }
}

/// The `Lexer` struct lexes input tokens from the provided source string reference and turns it into a stream of tokens
/// that can then be collected
pub struct Lexer<'a> {
    /// The current line number that the lexer is on, this is incremented every time a newline is encountered
    line: usize,
    /// The peekable iterator over the characters of the input string
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    /// Create a new Lexer using the given source string, creating an iterator over the chars of the source string
    #[inline]
    pub fn new(source: &'a str) -> Self {
        Self {
            line: 0,
            chars: source.chars().peekable(),
        }
    }

    /// Parse one identifier or keyword and return the token that will be either a [Key](enum@TokenType::Key) or a
    /// [Ident](enum@TokenType::Ident) variant
    #[inline(always)]
    fn ident(&mut self, first: char) -> Token {
        let mut ident = String::from(first); //Create a string from the first char given
        while match self.chars.peek() {
            //Push alphabetic characters to the string
            Some(c) if c.is_alphanumeric() => {
                ident.push(self.chars.next().unwrap());
                true
            }
            //Push an undersscore
            Some('_') => {
                ident.push(self.chars.next().unwrap());
                true
            }
            Some(_) => false,
            None => false,
        } {}

        Token::new(self.line, TokenType::Word(ident))
    }

    /// Lex a new token from the input stream or EOF if there are no tokens left to lex
    #[inline]
    fn token(&mut self) -> Option<Token> {
        while match self.chars.peek() {
            Some(w) if w.is_whitespace() => {
                //Increment line number if we encounter a newline
                if w == &'\n' {
                    self.line += 1;
                }
                self.chars.next(); //Consume the whitespace character
                true
            }
            Some(_) => false, //Stop looping if there is a non whitespace character
            None => return None, //Stop looping on EOF
        } {}

        let next = self.chars.next()?;
        //We can unwrap the next char because we returned None
        match next {
            '}' | ')' | ']' => Some(Token::new(self.line, TokenType::RightBrace(next))),
            '{' | '(' | '[' => Some(Token::new(self.line, TokenType::LeftBrace(next))),
            ',' => Some(Token::new(self.line, TokenType::Comma)),
            ';' => Some(Token::new(self.line, TokenType::Semicolon)),

            //Lex a number literal from the input
            c if c.is_numeric() => {
                let mut num = String::from(c); //Get a string of the number literal
                while match self.chars.peek() {
                    Some(n) if n.is_numeric() => {
                        num.push(self.chars.next().unwrap()); //Consume the number character
                        true
                    }
                    Some(_) => false, //Stop looping if it isn't numeric
                    None => false,
                } {}
                Some(Token::new(
                    self.line,
                    TokenType::NumLiteral(num.parse::<i64>().unwrap()),
                )) //Return the number literal that was lexed
            }

            '\"' => {
                let mut literal = String::new(); //Make a string for the string literal
                while match self.chars.next() {
                    Some('\"') => false, //Stop looping when we take a double quote from the end
                    Some(c) => {
                        //Increment line number if the char is a newline
                        if c == '\n' {
                            self.line += 1;
                        }
                        literal.push(c); //Push the character to the string
                        true
                    }
                    None => {
                        return Some(Token::new(
                            self.line,
                            TokenType::Error(format!("Expected ")),
                        ))
                    }
                } {}

                Some(Token::new(self.line, TokenType::StrLiteral(literal))) //Return the string literal
            }

            //Parse identifier or keyword
            c => Some(self.ident(c)),
        }
    }

    /// Lex all tokens from the input string until there are none left and return a vector of [Token](struct@Token)s.
    /// Turning the token iterator into a Vec of Tokens is more memory efficient because we can get rid of the large source file
    /// in memory and only work with tokens.
    #[inline(always)]
    pub fn to_vec(self) -> Vec<Token> {
        self.collect::<Vec<Token>>()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    ///Lex tokens from the input stream until there are none left
    fn next(&mut self) -> Option<Self::Item> {
        self.token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    pub fn lex_correct() {
        let lexed = Lexer::new(
            ";fn[si] 
        test()",
        )
        .to_vec();

        assert_eq!(
            lexed,
            vec![
                Token::new(0, TokenType::Semicolon),
                Token::new(0, TokenType::Word("fn".into())),
                Token::new(0, TokenType::LeftBrace('[')),
                Token::new(0, TokenType::Word("si".into())),
                Token::new(0, TokenType::RightBrace(']')),
                Token::new(1, TokenType::Word("test".into())),
                Token::new(1, TokenType::LeftBrace('(')),
                Token::new(1, TokenType::RightBrace(')')),
            ],
            "Lexer fails to lex tokens correctly"
        )
    }
}
