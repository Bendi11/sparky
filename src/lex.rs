//! The `lex` module provides the [Lexer](struct@Lexer) struct; an efficient zero copy lexer that
//! performs the first pass over a source file; turning the raw text into a token stream
//!

use std::convert::TryInto;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::Iterator;
use std::iter::Peekable;
use std::{convert, fmt};

use utf8_chars::BufReadCharsExt;
use utf8_chars::Chars;

use crate::Type;

/// The `Key` enum represents every type of keyword that can be lexed from the character stream.
/// It is contained in the [Key](enum@TokenType::Key) variant of the `Token` enum and is not meant to be constructed directly
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Key {
    /// The `fun` keyword is used to declare functions
    Fun,

    /// The `ext` keyword is used to declare items that are defined in other compilation units
    Ext,

    /// The `const` keyword is used to declare items that are defined as SSA form
    Const,

    /// The `ptr` keyword is used to declare pointer types
    Ptr,

    /// The `ret` keyword means return from the current function
    Ret,

    /// The `struct` keyword is used to define structs or struct types
    Struct,

    /// The `union` keyword is used to define unions or union types
    Union,

    /// The `if` keyword performs basic control flow
    If,

    /// The `else` keyword performs false condition control flow
    Else,

    /// The `while` keyword performs looping based on a condition
    While,

    /// The `let` keyword is used to declare variables
    Let,

    /// The `void` keyword is used to indicate no type
    Void,

    /// The `type` keyword is used to alias types with an identifier
    Type,

    /// The `static` keyword is used to define a function that is local to an object file
    Static,

    /// The `true` keyword is used for the literal true value
    True,

    /// The `false` keyword is used for the literal false value
    False,

    /// The `ns` keyword is used to declare namespaces
    Ns,

    /// The `use` keyword is used to import namespaces
    Use,
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const => write!(f, "const"),
            Self::Ext => write!(f, "ext"),
            Self::Fun => write!(f, "fun"),
            Self::Ptr => write!(f, "ptr"),
            Self::Ret => write!(f, "ret"),
            Self::Union => write!(f, "union"),
            Self::Struct => write!(f, "struct"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::While => write!(f, "while"),
            Self::Let => write!(f, "let"),
            Self::Void => write!(f, "void"),
            Self::Type => write!(f, "type"),
            Self::Static => write!(f, "static"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Ns => write!(f, "ns"),
            Self::Use => write!(f, "use"),
        }
    }
}

impl convert::TryFrom<&str> for Key {
    type Error = ();
    /// Convert a string to a keyword variant
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "const" => Ok(Self::Const),
            "ext" => Ok(Self::Ext),
            "fun" => Ok(Self::Fun),
            "ptr" => Ok(Self::Ptr),
            "ret" => Ok(Self::Ret),
            "union" => Ok(Self::Union),
            "struct" => Ok(Self::Struct),
            "if" => Ok(Self::If),
            "else" => Ok(Self::Else),
            "while" => Ok(Self::While),
            "let" => Ok(Self::Let),
            "void" => Ok(Self::Void),
            "type" => Ok(Self::Type),
            "static" => Ok(Self::Static),
            "true" => Ok(Self::True),
            "false" => Ok(Self::False),
            "ns" => Ok(Self::Ns),
            "use" => Ok(Self::Use),
            _ => Err(()),
        }
    }
}

/// The `Op` enum is held in the [Op](enum@TokenType::Op) variant of the `TokenType` enum and enumerates all possible operator tokens
/// lexed from the input stream
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    /// The equals assignment `=` operator
    Assign,

    /// The addition `+` operator
    Plus,

    /// The subtraction `-` operator
    Minus,

    /// The dereference / multiply `*` operator
    Star,

    /// The division `/` operator
    Divide,

    /// The remainder division `%` operator
    Modulo,

    /// The bitwise AND `&` operator
    And,

    /// The conditional and `&&` operator
    AndAnd,

    /// The bitwise OR '|' operator
    Or,

    /// The conditional or `||` operator
    OrOr,

    /// The bitwise Exclusive OR `^` operator
    Xor,

    /// The conditional equality `==` operator
    Equal,

    /// The bitwise unary NOT `!` operator
    Not,

    /// The conditional inverse equality `!=` operator
    NEqual,

    /// The conditional greater-than `>` operator
    Greater,

    /// The conditional less-than `<` operator
    Less,

    /// The conditional greater than or equal to `>=` operator
    GreaterEq,

    /// The conditional less than or equal to `<=` operator
    LessEq,

    /// Bitwise shift left `<<` operator
    ShLeft,

    /// Bitwise shift right `>>` operator
    ShRight,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Assign => "=",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::Star => "*",
                Self::Divide => "/",
                Self::Modulo => "%",
                Self::And => "&",
                Self::AndAnd => "&&",
                Self::Or => "|",
                Self::OrOr => "||",
                Self::Equal => "==",
                Self::Not => "!",
                Self::NEqual => "!=",
                Self::Less => "<",
                Self::Greater => ">",
                Self::GreaterEq => ">=",
                Self::LessEq => "<=",
                Self::Xor => "^",
                Self::ShLeft => "<<",
                Self::ShRight => ">>",
            }
        )
    }
}

/// The `TokenType` enumerates every type of token that can be lexed from the input source file
/// and is parsed into an AST by the parser
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    /// An opening left brace: any of {, (, or [
    LeftBrace(char),
    /// Any closing right brace like }, ], or )
    RightBrace(char),

    /// Any unknown identifier, this could be a keyword or plain identifier
    Ident(String),

    /// A string literal enclosed in double quotes
    StrLiteral(String),

    /// A number literal that is not floating point
    NumLiteral(String),

    /// The literal comma character ','; used to separate arguments; tuple fields, etc.
    Comma,

    /// Semicolon used to begin declarations
    Semicolon,

    /// The literal '.' character used for member method and variable access
    Dot,

    /// The dereference left and member access operator
    Arrow,

    /// Typename like i32 or u8, does not include attributes like ptr
    IntType(Type),

    /// An internal token used for error messages when lexing
    Error(String),

    /// A unary or binary operator
    Op(Op),

    /// Any keyword
    Key(Key),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LeftBrace(left) => write!(f, "Open Brace {}", left),
            Self::RightBrace(right) => write!(f, "Closing Brace {}", right),
            Self::Ident(ident) => write!(f, "Identifier: {}", ident),
            Self::StrLiteral(s) => write!(f, "String Literal: \"{}\"", s),
            Self::NumLiteral(num) => write!(f, "Number Literal: {}", num),
            Self::Comma => write!(f, "Comma"),
            Self::Semicolon => write!(f, "Semicolon"),
            Self::Error(err) => write!(f, "Error Lexing: {}", err),
            Self::Op(op) => write!(f, "Operator: {}", op),
            Self::Key(key) => write!(f, "Keyword: {}", key),
            Self::IntType(ty) => write!(f, "Typename: {}", ty),
            Self::Dot => write!(f, "Period"),
            Self::Arrow => write!(f, "Arrow"),
        }
    }
}

/// The `Pos` struct holds the line and column of a token in the source file
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pos(u32, u32, String);

impl Pos {
    pub const fn new(line: u32, col: u32, name: String) -> Self {
        Self(line, col, name)
    }

    pub fn line_mut(&mut self) -> &mut u32 {
        &mut self.0
    }

    pub fn col_mut(&mut self) -> &mut u32 {
        &mut self.1
    }

    pub fn file_name(&self) -> &str {
        &self.2
    }

    pub fn file_name_mut(&mut self) -> &mut String {
        &mut self.2
    }

    #[inline(always)]
    pub const fn line(&self) -> u32 {
        self.0
    }

    #[inline(always)]
    pub const fn col(&self) -> u32 {
        self.1
    }
}

use std::ops;
impl ops::Add<(u32, u32)> for Pos {
    type Output = Self;
    fn add(self, rhs: (u32, u32)) -> Self::Output {
        Self(self.0 + rhs.0, self.1 + rhs.1, self.2)
    }
}

impl ops::AddAssign<(u32, u32)> for Pos {
    fn add_assign(&mut self, rhs: (u32, u32)) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}

impl ops::Sub<(u32, u32)> for Pos {
    type Output = Self;
    fn sub(self, rhs: (u32, u32)) -> Self::Output {
        Self(self.0 - rhs.0, self.1 - rhs.1, self.2)
    }
}

/// Compare two `Pos` structs, respecting that a higher line count always means a bigger
/// position
impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering;
        match self.line().partial_cmp(&other.line()) {
            Some(Ordering::Greater) => Some(Ordering::Greater),
            Some(Ordering::Less) => Some(Ordering::Less),
            Some(Ordering::Equal) => self.col().partial_cmp(&other.col()),
            None => None,
        }
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({}:{})", self.2, self.0, self.1)
    }
}

/// The `Token` struct stores a [TokenType](enum@TokenType) enumeration representing the type of token,
/// and metadata like the line of the token
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token(pub Pos, pub TokenType);

impl Token {
    /// Create a new `Token` using the given line number and token type
    #[inline]
    pub fn new(pos: &Pos, token: TokenType) -> Self {
        Self(pos.clone(), token)
    }

    /// Check if this token is the same as another
    pub fn is(&self, is: TokenType) -> bool {
        self.1 == is
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

impl PartialEq<TokenType> for Token {
    //Implicitly compare a Token and a TokenType
    fn eq(&self, tok: &TokenType) -> bool {
        self.1.eq(tok)
    }
}

impl PartialEq<TokenType> for &Token {
    //Implicitly compare a Token and a TokenType
    fn eq(&self, tok: &TokenType) -> bool {
        self.1.eq(tok)
    }
}

impl From<Token> for TokenType {
    /// Convert a token to a TokenType
    fn from(tok: Token) -> Self {
        tok.1
    }
}

/// The `Lexer` struct lexes input tokens from the provided reader and turns it into a stream of tokens
/// that can then be collected
pub struct Lexer<'a, R: BufRead + ?Sized + fmt::Debug> {
    /// The current line number that the lexer is on, this is incremented every time a newline is encountered
    pos: Pos,
    /// The peekable iterator over the characters of the input string
    chars: Peekable<Chars<'a, R>>,
}

impl<'a> Lexer<'a, BufReader<&'a [u8]>> {
    /// Create a new Lexer using the given source string, creating an iterator over the chars of the source string
    #[inline]
    pub fn new(reader: &'a mut BufReader<&'a [u8]>) -> Self {
        Self {
            pos: Pos::new(1, 0, "unnamed_buffer".to_owned()),
            chars: reader.chars().peekable(),
        }
    }
}

impl<'a, R: BufRead + ?Sized + fmt::Debug> Lexer<'a, R> {
    /// Create a lexer from a buffered reader, this is more memory efficient for larger files
    pub fn from_reader(reader: &'a mut R, file_name: String) -> Self {
        Self {
            pos: Pos::new(1, 0, file_name),
            chars: reader.chars().peekable(),
        }
    }

    /// Parse one identifier or keyword and return the token that will be either a [Key](enum@TokenType::Key) or a
    /// [Ident](enum@TokenType::Ident) variant
    #[inline(always)]
    fn ident(&mut self, first: char) -> Token {
        let mut ident = String::from(first); //Create a string from the first char given
        while match self.chars.peek() {
            //Push alphabetic characters to the string
            Some(Ok(c)) if c.is_alphanumeric() || c == &'_' || c == &':' => {
                ident.push(self.next_char().unwrap().unwrap());
                true
            }
            Some(_) => false,
            None => false,
        } {}

        match ident.as_str() {
            "i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "bool" | "u64" | "i64"  => {
                Token::new(&self.pos, TokenType::IntType(Type::int_ty(ident)))
            },
            "usize" | "isize" => {
                Token::new(&self.pos, TokenType::IntType(Type::Integer{signed: &ident[0..1] == "i", width: usize::BITS as u8}))
            }
            ident =>
            //Create either a keyword or an identifier token
            {
                Token::new(
                    &self.pos,
                    match ident.try_into() {
                        Ok(key) => TokenType::Key(key),
                        _ => TokenType::Ident(ident.to_string()),
                    },
                )
            }
        }
    }

    /// Get the next char from our iterator, incrementing the column counter as we go
    #[inline(always)]
    fn next_char(&mut self) -> Option<Result<char, std::io::Error>> {
        match self.chars.next() {
            some @ Some(_) => {
                self.pos += (0, 1);
                some
            }
            None => None,
        }
    }

    /// Lex a new token from the input stream or EOF if there are no tokens left to lex
    #[inline]
    fn token(&mut self) -> Option<Token> {
        while match self.chars.peek() {
            Some(Ok(w)) if w.is_whitespace() => {
                //Increment line number if we encounter a newline
                if w == &'\n' {
                    self.pos += (1, 0);
                    *self.pos.col_mut() = 0;
                }
                self.next_char(); //Consume the whitespace character
                true
            }
            Some(_) => false, //Stop looping if there is a non whitespace character
            None => return None, //Stop looping on EOF
        } {}

        let next = self.next_char().and_then(|o| o.ok())?;
        //We can unwrap the next char because we returned None
        match next {
            '}' | ')' | ']' => Some(Token::new(&self.pos, TokenType::RightBrace(next))),
            '{' | '(' | '[' => Some(Token::new(&self.pos, TokenType::LeftBrace(next))),
            ',' => Some(Token::new(&self.pos, TokenType::Comma)),
            ';' => Some(Token::new(&self.pos, TokenType::Semicolon)),
            '.' => Some(Token::new(&self.pos, TokenType::Dot)),

            //Character integer literal
            '\'' => {
                let first = self.next_char().and_then(|o| o.ok())?; //Get the first character to check for escape sequences
                let character = if first == '\\' {
                    let second = self.next_char().and_then(|o| o.ok())?;
                    Token::new(
                        &self.pos,
                        TokenType::NumLiteral(
                            match second {
                                '\\' => b'\\',
                                'n' => b'\n',
                                't' => b'\t',
                                other => {
                                    return Some(Token::new(
                                        &self.pos,
                                        TokenType::Error(format!(
                                            "Unknown escape sequence \\{}",
                                            other
                                        )),
                                    ))
                                }
                            }
                            .to_string(),
                        ),
                    )
                } else {
                    Token::new(&self.pos, TokenType::NumLiteral((first as u8).to_string()))
                };
                if self.next_char().and_then(|o| o.ok())? != '\'' {
                    Some(Token::new(
                        &self.pos,
                        TokenType::Error(
                            "Character literal missing terminating \' character".to_owned(),
                        ),
                    ))
                } else {
                    Some(character)
                }
            }

            //Lex a number literal from the input
            c if c.is_numeric() => {
                let mut num = String::from(c); //Get a string of the number literal
                while match self.chars.peek() {
                    Some(Ok(n)) if n.is_numeric() => {
                        num.push(self.next_char().unwrap().unwrap()); //Consume the number character
                        true
                    }
                    Some(_) => false, //Stop looping if it isn't numeric
                    None => false,
                } {}
                Some(Token::new(&self.pos, TokenType::NumLiteral(num))) //Return the number literal that was lexed
            }

            '\"' => {
                let mut literal = String::new(); //Make a string for the string literal
                while match self.next_char().and_then(|o| o.ok()) {
                    Some('\"') => false, //Stop looping when we take a double quote from the end
                    //Parse escape sequences
                    Some('\\') => {
                        match self.next_char().and_then(|o| o.ok())? {
                            '\"' => literal.push('\"'),
                            'n' => literal.push('\n'),
                            't' => literal.push('\t'),
                            unknown => {
                                return Some(Token(
                                    self.pos.clone(),
                                    TokenType::Error(format!(
                                        "Unknown escape sequence \\{}",
                                        unknown
                                    )),
                                ))
                            }
                        }
                        true
                    }
                    Some(c) => {
                        //Increment line number if the char is a newline
                        if c == '\n' {
                            self.pos += (1, 0);
                            *self.pos.col_mut() = 0;
                        }
                        literal.push(c); //Push the character to the string
                        true
                    }
                    None => {
                        return Some(Token::new(
                            &self.pos,
                            TokenType::Error("Expected closing brace, got EOF".to_string()),
                        ))
                    }
                } {}

                Some(Token::new(&self.pos, TokenType::StrLiteral(literal))) //Return the string literal
            }
            //This is an operator
            '+' | '-' | '/' | '*' | '%' | '^' | '&' | '|' | '=' | '>' | '<' | '!' => {
                if self.chars.peek().unwrap().as_ref().unwrap().is_numeric() {
                    let mut num = String::from('-');
                    while match self.chars.peek() {
                        Some(Ok(n)) if n.is_numeric() => {
                            num.push(self.next_char().unwrap().unwrap());
                            true
                        }
                        _ => false,
                    } {}

                    return Some(Token::new(&self.pos, TokenType::NumLiteral(num)));
                }
                let peek = self.chars.peek();
                //Check if this is a two character operator
                if let Some(Ok(peek)) = peek {
                    match (next, peek) {
                        ('=', '=') => {
                            self.next_char();
                            return Some(Token(self.pos.clone(), TokenType::Op(Op::Equal)));
                        }
                        ('>', '=') => {
                            self.next_char();
                            return Some(Token(self.pos.clone(), TokenType::Op(Op::GreaterEq)));
                        }
                        ('<', '=') => {
                            self.next_char();
                            return Some(Token(self.pos.clone(), TokenType::Op(Op::LessEq)));
                        }
                        ('&', '&') => {
                            self.next_char();
                            return Some(Token(self.pos.clone(), TokenType::Op(Op::AndAnd)));
                        }
                        ('|', '|') => {
                            self.next_char();
                            return Some(Token(self.pos.clone(), TokenType::Op(Op::OrOr)));
                        }
                        ('!', '=') => {
                            self.next_char();
                            return Some(Token(self.pos.clone(), TokenType::Op(Op::NEqual)));
                        }
                        ('/', '/') => {
                            self.next_char();
                            while !matches!(self.next_char(), Some(Ok('\n')) | None) {} //Skip the comment
                            self.pos += (1, 0);
                            *self.pos.col_mut() = 0;
                            return self.token(); //Get the next token
                        }
                        ('-', '>') => {
                            self.next_char();
                            return Some(Token(self.pos.clone(), TokenType::Arrow));
                        },
                        ('>', '>') => {
                            self.next_char();
                            return Some(Token(self.pos.clone(), TokenType::Op(Op::ShRight)))
                        },
                        ('<', '<') => {
                            self.next_char();
                            return Some(Token(self.pos.clone(), TokenType::Op(Op::ShLeft)))
                        },
                        _ => (),
                    }
                }

                //Return the operator
                Some(Token(
                    self.pos.clone(),
                    TokenType::Op(match next {
                        '+' => Op::Plus,
                        '-' => Op::Minus,
                        '*' => Op::Star,
                        '/' => Op::Divide,
                        '%' => Op::Modulo,
                        '&' => Op::And,
                        '|' => Op::Or,
                        '>' => Op::Greater,
                        '<' => Op::Less,
                        '=' => Op::Assign,
                        '^' => Op::Xor,
                        _ => unreachable!(),
                    }),
                ))
            }

            //Parse identifier or keyword
            c => Some(self.ident(c)),
        }
    }

    /// Lex all tokens from the input string until there are none left and return a vector of [Token](struct@Token)s.
    /// Turning the token iterator into a Vec of Tokens is more memory efficient because we can get rid of the large source file
    /// in memory and only work with tokens.
    #[inline(always)]
    pub fn into_vec(self) -> Vec<Token> {
        self.into_iter().collect::<Vec<Token>>()
    }
}

impl<'a, R: BufRead + ?Sized + fmt::Debug> Iterator for Lexer<'a, R> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.token()
    }
}
