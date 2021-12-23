//! Arena types that contain a large collection of objects that cannot be 
//! removed from the collection, with indexes instead of references

use std::{marker::PhantomData, fmt, ops};


/// An index into an [Arena] structure
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash,)]
pub struct Index<T>(usize, PhantomData<T>);

impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}
impl<T> Copy for Index<T> {}

impl<T> Index<T> {
    /// The type name used in debug printing
    const TYPENAME: &'static str = std::any::type_name::<T>();

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

impl<T> Arena<T> {
    /// Create a new arena with a capacity and length of 0
    pub const fn new() -> Self {
        Self {
            data: Vec::new(),
        }
    }
    
    /// Create a new Arena with the given preallocated capacity
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            data: Vec::with_capacity(cap)
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
        self.data.get(idx.0).expect("Invalid index used to access arena item")
    }
    /// Get a mutable reference to the item referenced by `idx`
    #[inline]
    pub fn get_mut(&mut self, idx: Index<T>) -> &mut T {
        self.data.get_mut(idx.0).expect("Invalid index use to access arena item mutably")
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

impl<T: fmt::Debug> fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries(self.iter())
            .finish()
    }
}

impl<T> fmt::Debug for Index<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Index<{}>({})", Self::TYPENAME, self.0)
    }
}
