//! Arena types that contain a large collection of objects that cannot be
//! removed from the collection, with indexes instead of references

use std::{fmt, hash::Hash, marker::PhantomData, ops};

use hashbrown::HashMap;

/// An index into an [Arena] structure
#[derive(PartialOrd, Ord)]
pub struct Index<T>(usize, PhantomData<T>);

impl<T> std::hash::Hash for Index<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl<T> std::cmp::PartialEq<Index<T>> for Index<T> {
    fn eq(&self, other: &Index<T>) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T> std::cmp::Eq for Index<T> {}

impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}
impl<T> Copy for Index<T> {}

impl<T> Index<T> {
    /// Create a new Index with the internal value
    const fn new(idx: usize) -> Self {
        Self(idx, PhantomData)
    }

    /// Create an index from a raw index value
    /// The caller must ensure that `idx` is a valid index
    /// into the `data` field of an [Arena]
    pub const unsafe fn from_raw(idx: usize) -> Self {
        Self::new(idx)
    }

    /// Get the actual index value of this [Index]
    pub fn val(&self) -> usize {
        self.0
    }
}

/// A collection of items of type `T` all stored in one place, with indices into
/// the arena being used in the place of references
#[derive(Clone)]
pub struct Arena<T> {
    /// The items contained in this arena
    data: Vec<T>,
}

/// Structure similar to the [Arena] that holds its data in a Vec<T>,
/// but only allocates new elements when a unique one is added,
/// so two elements that are equal share the same ID
#[derive(Debug, Clone)]
pub struct Interner<T: Hash + Eq> {
    /// A map of instances of T to their positions in `arena`
    ids: HashMap<T, Index<T>>,
    /// The arena holding instances of `T`
    arena: Arena<T>,
}

impl<T: Hash + Eq + Clone> Interner<T> {
    /// Create a new empty interner
    pub fn new() -> Self {
        Self {
            ids: HashMap::new(),
            arena: Arena::new(),
        }
    }

    /// Insert an item into the arena or return a previously stored ID
    pub fn insert(&mut self, val: T) -> Index<T> {
        match self.ids.get(&val) {
            Some(id) => *id,
            None => {
                let Self { arena, ids } = self;
                arena.insert_with(|id| {
                    ids.insert(val.clone(), id);
                    val
                })
            }
        }
    }

    /// Insert the value into the internal arena and gurantee that it's index
    /// won't be returned by another insert() call
    pub fn insert_nointern(&mut self, val: T) -> Index<T> {
        self.arena.insert_with(|_| val)
    }

    pub fn insert_with_nointern<F: FnOnce(Index<T>) -> T>(&mut self, f: F) -> Index<T> {
        let t = f(Index::new(self.arena.data.len()));
        self.insert_nointern(t)
    }

    /// Insert the element created from a closure that takes an ID, used for
    /// types that contain an ID as a field
    pub fn insert_with<F: FnOnce(Index<T>) -> T>(&mut self, f: F) -> Index<T> {
        let val = f(Index::new(self.arena.data.len()));
        self.insert(val)
    }

    /// Get the item at a specific index
    #[inline]
    pub fn get(&self, idx: Index<T>) -> &T {
        self.arena.get(idx)
    }

    /// # WARNING if used with an index that is interned, this will break interning
    pub fn get_mut(&mut self, idx: Index<T>) -> &mut T {
        self.arena.get_mut(idx)
    }
}

impl<T> Arena<T> {
    /// Create a new arena with a capacity and length of 0
    pub const fn new() -> Self {
        Self { data: Vec::new() }
    }

    /// Create a new Arena with the given preallocated capacity
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            data: Vec::with_capacity(cap),
        }
    }

    /// An an item to this arena and return the index of the item
    pub fn insert(&mut self, val: T) -> Index<T> {
        self.data.push(val);
        Index(self.data.len() - 1, PhantomData)
    }
    /// Add an element to this arena using a closure taking the index of the item to be added
    /// and returning the item
    pub fn insert_with<F: FnOnce(Index<T>) -> T>(&mut self, f: F) -> Index<T> {
        let idx = Index::new(self.data.len());
        let t = f(idx);
        self.insert(t)
    }

    /// Set the element at `idx` to `val`
    #[inline]
    pub fn set(&mut self, idx: Index<T>, val: T) {
        *self.get_mut(idx) = val;
    }

    /// Get an immutable reference to the item referenced by `idx`
    #[inline]
    pub fn get(&self, idx: Index<T>) -> &T {
        self.data
            .get(idx.0)
            .expect("Invalid index used to access arena item")
    }
    /// Get a mutable reference to the item referenced by `idx`
    #[inline]
    pub fn get_mut(&mut self, idx: Index<T>) -> &mut T {
        self.data
            .get_mut(idx.0)
            .expect("Invalid index use to access arena item mutably")
    }

    /// Get an iterator over all items of this arena
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.data.iter()
    }

    /// Get an iterator over all indices in this arena
    pub fn indices(&self) -> impl Iterator<Item = Index<T>> {
        (0..self.data.len() - 1)
            .into_iter()
            .map(|idx| Index::new(idx))
    }
}

impl<T> IntoIterator for Arena<T> {
    type IntoIter = std::vec::IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}
impl<'a, T> IntoIterator for &'a Arena<T> {
    type IntoIter = std::slice::Iter<'a, T>;
    type Item = &'a T;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl<T> ops::Index<Index<T>> for Arena<T> {
    type Output = T;
    #[inline]
    fn index(&self, index: Index<T>) -> &Self::Output {
        self.get(index)
    }
}
impl<T> ops::IndexMut<Index<T>> for Arena<T> {
    #[inline]
    fn index_mut(&mut self, index: Index<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}
impl<T: Hash + Eq + Clone> ops::Index<Index<T>> for Interner<T> {
    type Output = T;
    fn index(&self, index: Index<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T: fmt::Debug> fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}
impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> fmt::Debug for Index<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Index({})", self.0)
    }
}
impl<T> fmt::Display for Index<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        std::fmt::UpperHex::fmt(&self.0, f)
    }
}
