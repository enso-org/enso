//! This module defines a typed index struct. Useful to introduce type safety when using indexes
//! several indexable containers.

use crate::prelude::*;



// =============
// === Index ===
// =============

/// Typed newtype for `usize` meant to be used as a typed index.
pub struct Index<T> {
    /// Raw value.
    pub raw : usize,
    phantom : PhantomData<T>
}

impl<T> Index<T> {
    /// Constructor
    pub fn new(raw:usize) -> Self {
        let phantom = default();
        Self {raw,phantom}
    }
}

// === Impls ===

impl<T> Copy for Index<T> {}
impl<T> Eq   for Index<T> {}

impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Hash for Index<T> {
    fn hash<H:std::hash::Hasher>(&self, state:&mut H) {
        self.raw.hash(state)
    }
}

impl<T> PartialEq for Index<T> {
    fn eq(&self, other:&Self) -> bool {
        self.raw == other.raw
    }
}

impl<T> From<Index<T>> for usize {
    fn from(t:Index<T>) -> Self {
        t.raw
    }
}

impl<T> From<&Index<T>> for usize {
    fn from(t:&Index<T>) -> Self {
        t.raw
    }
}

impl<T> From<usize> for Index<T> {
    fn from(t:usize) -> Self {
        Self::new(t)
    }
}

impl<T> From<&usize> for Index<T> {
    fn from(t:&usize) -> Self {
        Self::new(*t)
    }
}

impl<T> Debug for Index<T> {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{}",self.raw)
    }
}

impl<T> Display for Index<T> {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{}",self.raw)
    }
}
