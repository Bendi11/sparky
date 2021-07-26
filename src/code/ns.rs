//! Structs and functions for handling namespaces and getting contents from them

use std::str::FromStr;
use thiserror::Error;

/// The `Path` struct functions nearly the same as a the [Path](std::path::Path) struct from the standard library,
/// but uses the "::" characters as separators instead of forward/back slashes
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    /// The parts of the path, separated by "::"
    parts: Vec<String>,
}

/// An enum describing all possible errors when parsing a path 
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum PathParseErr {
    #[error("Namespace paths are separated by two ':' colon characters, but only one was found in a row")]
    OneColon,

    #[error("An empty part was encountered in namespace name")]
    EmptyName,
}

impl FromStr for Path {
    type Err = PathParseErr;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = vec![];
        let mut chars = s.clone().chars();
        let mut pos = 0;
        loop {
            let start = pos;
            let mut off = 0;
            loop {
                match chars.next() {
                    Some(':') => {
                        if chars.next() != Some(':') {
                            return Err(PathParseErr::OneColon)
                        }
                        if off == 0 {
                            return Err(PathParseErr::EmptyName)
                        }
    
                        parts.push(s[start..(start + off)].to_owned())
                    },
                    Some(_) => off += 1,
                    None => break,
                }
            }
            pos += off;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_parse() {
        assert_eq!(Path::from_str("test:a"), Err(PathParseErr::OneColon));
        assert_eq!(Path::from_str("testing::one"), Ok(Path {
            parts: vec!["testing".to_owned(), "one".to_owned()]
        }))
    }
}