use std::collections::VecDeque;

use crate::{ast::FunProto, types::Container, Type};
use generational_arena::{Arena, Index};
use hashbrown::HashMap;
use inkwell::{types::StructType, values::FunctionValue};

/// The `namespace` struct holds an amount of items and serves to namespace them
pub struct Namespace<'c> {
    /// A hash map of identifiers to defined struct types
    pub struct_types: HashMap<String, (StructType<'c>, Container)>,

    /// A hash map of identifiers to defined union types
    pub union_types: HashMap<String, (StructType<'c>, Container)>,

    /// A map of function names to function prototypes
    pub funs: HashMap<String, (FunctionValue<'c>, FunProto)>,

    /// The name of this namespace
    pub name: String,

    /// Further nested namespaces
    pub nested: HashMap<String, Index>,

    /// A map of user - defined type definitions to real types
    pub typedefs: HashMap<String, Type>,

    /// The index of this namesapce
    idx: Index,

    /// The full path to this namespace
    pub path: VecDeque<String>,

    /// The parent of this namespace
    pub parent: Option<Index>,
}

impl<'c> Namespace<'c> {
    /// Create a new namespace
    pub fn new<'a>(
        name: String,
        parent: Option<Index>,
        mut path: VecDeque<String>,
        arena: &'a mut Arena<Namespace<'c>>,
    ) -> Index {
        path.push_back(name.clone());
        let me = Self {
            name,
            path,
            idx: Index::from_raw_parts(0, 0),
            typedefs: HashMap::new(),
            funs: HashMap::new(),
            union_types: HashMap::new(),
            struct_types: HashMap::new(),
            nested: HashMap::new(),
            parent,
        };
        let idx = arena.insert(me);
        arena.get_mut(idx).unwrap().idx = idx;
        idx
    }

    /// Return a root namespace
    pub fn root() -> Self {
        Self {
            name: "root".to_owned(),
            path: VecDeque::new(),
            idx: Index::from_raw_parts(0, 0),
            typedefs: HashMap::new(),
            funs: HashMap::new(),
            union_types: HashMap::new(),
            struct_types: HashMap::new(),
            nested: HashMap::new(),
            parent: None,
        }
    }

    #[inline(always)]
    pub fn add_child(&mut self, child: Index, name: String) {
        self.nested.insert(name, child);
    }

    /// Get a child namespace
    pub fn get_child_ns(
        &'c self,
        mut path: VecDeque<String>,
        arena: &Arena<Namespace<'_>>,
    ) -> Option<Index> {
        match path.pop_front() {
            Some(name) => arena
                .get(*self.nested.get(&name)?)
                .unwrap()
                .get_child_ns(path, arena),
            None => Some(self.idx),
        }
    }
}
