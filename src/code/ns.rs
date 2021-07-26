//! Structs and functions for handling namespaces and getting contents from them

use std::{convert::Infallible, str::FromStr, rc::Rc, cell::RefCell};
use hashbrown::HashMap;
use inkwell::{types::StructType, values::FunctionValue};

use crate::{Type, ast::FunProto, types::Container};

/// The `Path` struct functions nearly the same as a the [Path](std::path::Path) struct from the standard library,
/// but uses the "::" characters as separators instead of forward/back slashes
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    /// The parts of the path, separated by "::"
    parts: Vec<String>,
}

impl FromStr for Path {
    type Err = Infallible;
    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self{parts: s.split("::").map(|s| s.to_owned()).collect()})
    }
}

impl Path {
    /// Return an iterator over the parts of this path
    #[inline]
    pub fn parts(&self) -> impl Iterator<Item = &String> {
        self.parts.iter()
    }

    /// Get the last element of this namespace path
    #[inline(always)]
    pub fn last(&self) -> Option<&String> {
        self.parts.last()
    }

    /// Get all parts of the path except the final name
    pub fn parent(&self) -> Option<&[String]> {
        if self.parts.len() < 2 {
            return None
        }
        Some(&self.parts[0..self.parts.len()])
    }
}

/// The `Ns` struct contains declarations and other namespaces, 
/// allowing for names to not clash
#[derive(Debug, Clone)]
pub struct Ns<'a, 'c> {
    /// The namespace's name
    pub name: String,

    /// The parent of this namespace
    pub parent: Rc<RefCell< Option<&'a Self> >>,

    /// A hash map of identifiers to defined struct types
    pub struct_types: Rc<RefCell< HashMap<String, (StructType<'c>, Container)> >>,

    /// A hash map of identifiers to defined union types
    pub union_types: Rc<RefCell< HashMap<String, (StructType<'c>, Container)> >>,

    /// A map of function names to function prototypes
    pub funs: Rc<RefCell< HashMap<String, (FunctionValue<'c>, FunProto)> >>,

    /// A map of user - defined type definitions to real types
    pub typedefs: Rc<RefCell< HashMap<String, Type> >>,

    /// Nested namespaces with interior mutability
    pub nested: Rc<RefCell< HashMap<String, &'a Self> >>,
}

impl<'a, 'c> Ns<'a, 'c> {
    /// Get a nested namespace if it exists
    pub fn get_ns(&'a self, path: Path) -> Option<&'a Self> {
        self.get_child(path.parts())
    }

    /// Get a child namespace using an iterator, used in the [get_ns] function
    fn get_child<'b>(&'a self, mut iter: impl Iterator<Item = &'b String>) -> Option<&'a Self> {
        match iter.next() {
            None => Some(&self),
            Some(ns) => self.nested.borrow().get(ns)?.get_child(iter),
        }
    }

    /// Add a child namespace to this namespace
    pub fn add_ns(&'a self, ns: &'a Self) {
        *ns.parent.borrow_mut() = Some(&self);
        self.nested.borrow_mut().insert(ns.name.clone(), ns);
    }

    
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_parse() {
        assert_eq!(Path::from_str("testing::one"), Ok(Path {
            parts: vec!["testing".to_owned(), "one".to_owned()]
        }))
    }
}