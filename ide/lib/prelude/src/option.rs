//! This module defines utilities for working with the `Option` type.

/// Adds `for_each` method to the `Option` type.
pub trait OptionOps {
    type Item;
    fn for_each<U,F>(self, f: F) where F : FnOnce(Self::Item) -> U;
}

impl<T> OptionOps for Option<T> {
    type Item = T;
    fn for_each<U,F>(self, f: F) where F : FnOnce(Self::Item) -> U {
        if let Some(x) = self { f(x); }
    }
}
