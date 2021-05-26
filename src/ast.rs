//! The `ast` module provides structs describing abstract syntax tree nodes and
//! the all important [Ast](enum@Ast) enum representing every type of AST node
use crate::{
    parse::Attr,
};

/// The `Body` struct is not an AST node, but it is used in any AST node that has a list of expressions to execute like a
/// function declaration
pub type Body<'a> = Vec<Ast<'a>>;

/// The `WordProto` struct represents a word prototype with all information needed to call the word like expected stack types
#[derive(Clone, Debug, Eq, PartialEq)]
/// and returned stack layout
pub struct WordProto<'a> {
    /// The name of the word that will be used to call it
    pub name: String,

    /// The expected input stack layout
    pub input: StackLayout<'a>,

    /// The stack layout for the output
    pub output: StackLayout<'a>,

    /// Attributes for the function
    pub attrs: Attr,
}

/// The `WordDecl` struct represents one word prototype plus a list of instructions that makes up the word body
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WordDecl<'a> {
    /// The word prototype that contains all information about how the word should be called
    pub proto: WordProto<'a>,

    /// The body of the function containing all statements
    pub body: Body<'a>,
}

impl<'a> fmt::Display for WordDecl<'a> {
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
pub enum Decl<'a> {
    /// A word declaration with name and stack changes
    Word(WordDecl<'a>),
}

impl<'a> fmt::Display for Decl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Word(decl) => write!(f, "{}", decl),
        }
    }
}

/// The `Ast` enum represents all possible abstract syntax tree nodes and their children
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ast<'a> {
    /// Any declaration
    Decl(Decl<'a>),

    /// A word call
    Word {
        path: String,
        attrs: Attr,
    },

    /// A literal value is being pushed to the stack
    Literal {
        /// The value being pushed to the stack
        val: BasicValueEnum<'a>,
    },
}

use std::fmt;

use inkwell::values::BasicValueEnum;

use crate::types::StackLayout;
impl<'a> fmt::Display for Ast<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Decl(d) => write!(f, "{}", d),
            Self::Word { path, attrs } => {
                write!(f, "Word call: {}\nAttributes: {:#?}", path, attrs)
            }
            Self::Literal { val } => write!(f, "Push {:?}", val),
        }
    }
}



