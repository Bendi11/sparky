use std::marker::PhantomData;

mod arena;

pub use arena::Arena;

pub trait ArenaKey {
    /// Create a key value from a numerical index
    fn create(idx: usize) -> Self;
    /// Get the numerical index into a vector for this key
    fn index(&self) -> usize;
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RawKey<T = usize>(T);

impl<T: Into<usize> + From<usize> + Copy> ArenaKey for RawKey<T> {
    fn create(idx: usize) -> Self {
        Self(T::from(idx))    
    }

    fn index(&self) -> usize {
        self.0.into()
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypedKey<T, I = usize> {
    idx: I,
    _boo: PhantomData<T>,
}

impl<T, I: Into<usize> + From<usize> + Copy> ArenaKey for TypedKey<T, I> {
    fn create(idx: usize) -> Self {
        Self { idx: I::from(idx), _boo: PhantomData }
    }

    fn index(&self) -> usize {
        self.idx.into()
    }
}
