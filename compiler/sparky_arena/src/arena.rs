use std::{marker::PhantomData, ops};

use crate::{ArenaKey, TypedKey};

/// An insert-only collection that maps unique IDs to values of type `T`
#[derive(Clone, Debug)]
pub struct Arena<T, Id: ArenaKey = TypedKey<T>> {
    vec: Vec<T>,
    _boo: PhantomData<Id>,
}

impl<T, Id: ArenaKey> Arena<T, Id> {
    /// Create a new `Arena` with no allocated memory for elements
    #[inline]
    pub const fn new() -> Self {
        Self {
            vec: Vec::new(),
            _boo: PhantomData,
        }
    }

    /// Create a new `Arena` with memory allocated for the given number of elements to be stored
    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            vec: Vec::with_capacity(cap),
            _boo: PhantomData,
        }
    }

    /// Insert the given value into this `Arena`, returning a key to access the inserted value
    pub fn insert(&mut self, val: T) -> Id {
        let key = Id::create(self.vec.len());
        self.vec.push(val);
        key
    }

    /// Get an immutable reference to an element referenced by a key
    pub fn get(&self, key: Id) -> &T {
        let key = key.index();
        if key >= self.vec.len() {
            panic!(
                "Attempt to access arena element using an invalid key with index: {}",
                key
            );
        }

        &self.vec[key]
    }

    /// Get a mutable reference to the element referenced by a key
    pub fn get_mut(&mut self, key: Id) -> &mut T {
        let key = key.index();
        if key >= self.vec.len() {
            panic!(
                "Attempt to mutable access arena element using an invalid key with index: {}",
                key
            );
        }

        &mut self.vec[key]
    }
}

impl<T, I: ArenaKey> ops::Index<I> for Arena<T, I> {
    type Output = T;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        self.get(index)
    }
}

impl<T, I: ArenaKey> ops::IndexMut<I> for Arena<T, I> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.get_mut(index)
    }
}
