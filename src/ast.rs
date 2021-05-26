//! The `ast` module provides structs describing abstract syntax tree nodes and
//! the all important [Ast](enum@Ast) enum representing every type of AST node
use crate::{
    lex::{Lexer, Token},
    parse::{Attr, ParseErr, Parser},
    types::Value,
};
use std::{iter::FromIterator, str::FromStr};

/// The `Body` struct is not an AST node, but it is used in any AST node that has a list of expressions to execute like a
/// function declaration
#[derive(Clone, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct Body(pub Vec<Ast>);

/// The `WordProto` struct represents a word prototype with all information needed to call the word like expected stack types
#[derive(Clone, Debug, Eq, PartialEq)]
/// and returned stack layout
pub struct WordProto {
    /// The name of the word that will be used to call it
    pub name: String,

    /// The expected input stack layout
    pub input: StackLayout,

    /// The stack layout for the output
    pub output: StackLayout,

    /// Attributes for the function
    pub attrs: HashMap<String, Attr>,
}

/// The `WordDecl` struct represents one word prototype plus a list of instructions that makes up the word body
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WordDecl {
    /// The word prototype that contains all information about how the word should be called
    pub proto: WordProto,

    /// The body of the function containing all statements
    pub body: Body,
}

impl fmt::Display for WordDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Word: {}\nExpected stack layout: {}\nOutput stack layout: {}\n",
            self.proto.name, self.proto.input, self.proto.output
        )
    }
}

/// The `Decl` enumerates all types of declarations possible
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Decl {
    /// A word declaration with name and stack changes
    Word(WordDecl),
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Word(decl) => write!(f, "{}", decl),
        }
    }
}

/// The `Ast` enum represents all possible abstract syntax tree nodes and their children
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ast {
    /// Any declaration
    Decl(Decl),

    /// A word call
    Word {
        path: String,
        attrs: HashMap<String, Attr>,
    },

    /// A literal value is being pushed to the stack
    Literal {
        /// The value being pushed to the stack
        val: Value,
    },
}

use std::fmt;

use hashbrown::HashMap;

use crate::types::StackLayout;
impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Decl(d) => write!(f, "{}", d),
            Self::Word { path, attrs } => {
                write!(f, "Word call: {}\nAttributes: {:#?}", path, attrs)
            }
            Self::Literal { val } => write!(f, "Push {}", val),
        }
    }
}

impl FromIterator<Token> for Body {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
        let parser = Parser {
            tokens: iter.into_iter().peekable(),
        }; //Create a new instance of self
        parser.parse().unwrap()
    }
}

impl FromStr for Body {
    type Err = ParseErr;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let p = Parser {
            tokens: Lexer::new(s).peekable(),
        };
        p.parse()
    }
}
