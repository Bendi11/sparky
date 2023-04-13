use std::marker::PhantomData;

mod arena;

pub use arena::Arena;

/// Trait for all types that can be used to index into an [Arena]- allows for type checked and raw
/// indices using the same arena type
pub trait ArenaKey {
    /// Create a key value from a numerical index
    fn create(idx: usize) -> Self;
    /// Get the numerical index into a vector for this key
    fn index(&self) -> usize;
}

#[macro_export]
macro_rules! new_arena_key {
    (
        $(#[$outer:meta])*
        pub struct $name:ident;
    ) => {
        ::sparky_arena::new_arena_key!{
            $(#[$outer])*
            pub struct $name(u32);
        }
    };
    (
        $(#[$outer:meta])*
        pub struct $name:ident($ty:ty);
    ) => {
        $(#[$outer])*
        #[repr(transparent)]
        pub struct $name($ty);

        impl ::sparky_arena::ArenaKey for $name {
            fn create(idx: usize) -> Self { Self(idx as $ty) }
            fn index(&self) -> usize { self.0 as usize }
        }
    };
}

/// A type implementing [ArenaKey] that only contains a basic index value with no type checking,
/// when possible use the [TypedKey] type
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

/// An index into an [Arena] that also contains a type parameter, used for additional type checking
/// to ensure and index into an `Arena<T>` cannot be accidentaly used to access elements in an
/// `Arena<U>`
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypedKey<T, I = usize> {
    idx: I,
    _boo: PhantomData<T>,
}

impl<T, I: Into<usize> + From<usize> + Copy> ArenaKey for TypedKey<T, I> {
    fn create(idx: usize) -> Self {
        Self {
            idx: I::from(idx),
            _boo: PhantomData,
        }
    }

    fn index(&self) -> usize {
        self.idx.into()
    }
}
