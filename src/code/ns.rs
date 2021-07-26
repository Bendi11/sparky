//! Structs and functions for handling namespaces and getting contents from them

use std::{cell::RefCell, convert::Infallible, fmt, iter::FromIterator, str::FromStr};
use hashbrown::HashMap;
use inkwell::{types::StructType, values::FunctionValue};
use log::debug;

use crate::{Type, ast::FunProto, types::Container};

/// The `Path` struct functions nearly the same as a the [Path](std::path::Path) struct from the standard library,
/// but uses the "::" characters as separators instead of forward/back slashes
#[derive(Debug, Default, Clone, PartialEq, Eq)]
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

impl<T: ToString> FromIterator<T> for Path {
    fn from_iter<A: IntoIterator<Item = T>>(iter: A) -> Self {
        Self{parts: iter.into_iter().map(|s| s.to_string()).collect()}
    }
}

impl fmt::Display for Path {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.parts.join("::"))
    }
}

impl Path {
    /// Get the number of names in this namespace
    #[inline(always)]
    pub fn count(&self) -> usize {
        self.parts.len()
    }

    /// Push a name to this path
    #[inline]
    pub fn push(&mut self, item: impl ToString) {
        self.parts.push(item.to_string())
    }

    /// Return an iterator over the parts of this path
    #[inline]
    pub fn parts(&self) -> (impl Iterator<Item = &String> + DoubleEndedIterator<Item = &String>){
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
    pub parent: RefCell< Option<&'a Self> >,

    /// A hash map of identifiers to defined struct types
    pub struct_types: RefCell< HashMap<String, (StructType<'c>, Container)> >,

    /// A hash map of identifiers to defined union types
    pub union_types: RefCell< HashMap<String, (StructType<'c>, Container)> >,

    /// A map of function names to function prototypes
    pub funs: RefCell< HashMap<String, (FunctionValue<'c>, FunProto)> >,

    /// A map of user - defined type definitions to real types
    pub typedefs: RefCell< HashMap<String, Type> >,

    /// Nested namespaces with interior mutability
    pub nested: RefCell< HashMap<String, &'a Self> >,
}

impl<'a, 'c> Ns<'a, 'c> {
    /// Construct a new empty namespace from only a name
    pub fn new_empty(name: String) -> Self {
        Self {
            name, 
            parent: RefCell::new(Default::default()),
            struct_types: RefCell::new(Default::default()),
            union_types: RefCell::new(Default::default()),
            typedefs: RefCell::new(Default::default()),
            funs: RefCell::new(Default::default()),
            nested: RefCell::new(Default::default()),

        }
    }

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
        debug!("Adding child namespace {} to namespace {}", ns.name, self.full_path());
        self.nested.borrow_mut().insert(ns.name.clone(), ns);
    }

    /// Get a struct type from this namespace using the given path
    pub fn get_struct(&'a self, path: Path) -> Option<(StructType<'c>, Container)> {
        let ns = match path.parent() {
            Some(parents) => self.get_child(parents.iter())?,
            None => self
        };
        ns.struct_types.borrow().get(path.last()?).cloned()
    }

    /// Get a union type from this namespace using the given path
    pub fn get_union(&'a self, path: Path) -> Option<(StructType<'c>, Container)> {
        let ns = match path.parent() {
            Some(parents) => self.get_child(parents.iter())?,
            None => self
        };
        ns.union_types.borrow().get(path.last()?).cloned()
    }

    /// Get a function from this namespace using the given path
    pub fn get_fun(&'a self, path: Path) -> Option<(FunctionValue<'c>, FunProto)> {
        let ns = match path.parent() {
            Some(parents) => self.get_child(parents.iter())?,
            None => self
        };
        ns.funs.borrow().get(path.last()?).cloned()
    }

    /// Get a typedef from this namespace using the given path
    pub fn get_typedef(&'a self, path: Path) -> Option<Type> {
        let ns = match path.parent() {
            Some(parents) => self.get_child(parents.iter())?,
            None => self
        };
        ns.typedefs.borrow().get(path.last()?).cloned()
    }

    /// Get the full path to this namespace from the root namespace
    #[inline]
    pub fn full_path(&'a self) -> Path {
        let mut path = Path::default();
        self.path(&mut path);
        path
    }

    /// Get the full path to this namespace
    fn path(&'a self, path: &mut Path) {
        path.parts.push(self.name.clone());
        match self.parent.borrow().as_ref() {
            Some(parent) => parent.path(path),
            None => *path = path.parts().rev().collect()
        }
    }

    /// Fully qualify a name using the full path from root to this namespace, then to the name given
    pub fn qualify(&'a self, name: impl ToString) -> Path {
        let mut path = self.full_path();
        path.push(name.to_string());
        path
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