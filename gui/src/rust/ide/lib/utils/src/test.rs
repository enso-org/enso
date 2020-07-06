//! Module with general purpose utilities meant to be used in tests.

pub mod future;
pub mod stream;

/// Traits providing helper methods for test code.
pub mod traits {
    pub use super::ExpectTuple;
    pub use super::future::FutureResultTestExt;
    pub use super::future::FutureTestExt;
    pub use super::stream::StreamTestExt;
}



// ===================
// === ExpectTuple ===
// ===================

// === Trait ===
/// Helper allowing converting between collections and tuples. Unwraps internally,
/// will panic on failure. For test environment only.
pub trait ExpectTuple<T> {
    /// Convert Self to tuple `T`. Panic if collection has different count of elements.
    fn expect_tuple(self) -> T;
}


// === Implementations ===
// TODO [MWU] boilerplate below should be generated with macro

impl<Collection:IntoIterator>
ExpectTuple<(Collection::Item,)> for Collection {
    fn expect_tuple(self) -> (Collection::Item,) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,)
    }
}

impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple(self) -> (Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2)
    }
}

impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple
    (self) -> (Collection::Item,Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        let     v3   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2,v3)
    }
}

impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item,Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple
    (self) -> (Collection::Item,Collection::Item,Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        let     v3   = iter.next().unwrap();
        let     v4   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2,v3,v4)
    }
}

#[allow(clippy::type_complexity)]
impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item,Collection::Item,Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple
    (self)
     -> (Collection::Item,Collection::Item,Collection::Item,Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        let     v3   = iter.next().unwrap();
        let     v4   = iter.next().unwrap();
        let     v5   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2,v3,v4,v5)
    }
}
