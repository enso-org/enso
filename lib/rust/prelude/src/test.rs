//! Module with general purpose utilities meant to be used in tests.

pub mod future;
pub mod stream;

/// Traits providing helper methods for test code.
pub mod traits {
    pub use super::future::FutureResultTestExt;
    pub use super::future::FutureTestExt;
    pub use super::stream::StreamTestExt;
    pub use super::ExpectTuple;
}

use enso_shapely::replace;



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


/// Implements ExpectTuple of tuple of various sizes for collection
/// (something implementing IntoIterator).
///
/// It takes a list of some identifiers. Passing _n_ identifiers generate implementations of tuples
/// up to size _n_. Identifiers shall differ.
macro_rules! impl_expect_tuple_for_collections {
    () => {};
    ($first:ident $(,$ident:ident)*) => {
        impl<T:IntoIterator> ExpectTuple<(T::Item, $(replace!{$ident,T::Item}),*)> for T {
            fn expect_tuple(self) -> (T::Item, $(replace!{$ident,T::Item}),*) {
                let mut iter = self.into_iter();
                let $first   = iter.next().unwrap();
                $(let $ident = iter.next().unwrap();)*
                assert!(iter.next().is_none());
                ($first,$($ident),*)
            }
        }
        impl_expect_tuple_for_collections!{$($ident),*}
    };
}

impl_expect_tuple_for_collections!(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12);
