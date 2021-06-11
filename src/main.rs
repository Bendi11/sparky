pub mod lex;
pub mod ast;
use std::fmt;

use lalrpop_util::lalrpop_mod;

/// The `Type` enum enumerates all possible types for expressions
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Integer {
        signed: bool,
        width: u8,
    },

    /// Pointer to type
    Ptr(Box<Type>),
}

impl Type {
    pub fn int_ty(s: String) -> Self {
        Self::Integer {
            signed: &s[0..1] == "i",
            width: s[1..].parse().unwrap(),
        }
    }

    /// Generate a pointer type that points to a type of Self
    #[inline]
    pub fn ptr_type(&self) -> Self {
        Self::Ptr(Box::new(self.clone()))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer{width, signed} => write!(f, "{}{}", match signed {
                true => 'i',
                false => 'u',
            }, width),
            Self::Ptr(ty) => write!(f, "pointer to {}", ty)
        }
    }
}

lalrpop_mod!(pub parse);

const SRC: &[u8] = br#"
fun testing(i32 a, u8 ptr, u8 ptr ptr ptr argc,) i32
"#;

fn main() {
    let mut reader = std::io::BufReader::new(SRC);
    let p = lex::Lexer::from_reader(&mut reader);
    println!("{:?}", parse::FunProtoParser::new().parse(p).unwrap());
}
