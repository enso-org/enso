//! This module defines utilities for working with the [`std::option::Option`] type.

/// Adds mapping methods to the `Option` type.
pub trait OptionOps {
    type Item;
    fn map_ref      <U,F> (&self , f:F) -> Option<U> where F : FnOnce(&Self::Item) -> U;
    fn for_each     <U,F> (self  , f:F)              where F : FnOnce(Self::Item)  -> U;
    fn for_each_ref <U,F> (&self , f:F)              where F : FnOnce(&Self::Item) -> U;
}

impl<T> OptionOps for Option<T> {
    type Item = T;

    fn map_ref<U,F>(&self, f:F) -> Option<U> where F : FnOnce(&Self::Item) -> U {
        self.as_ref().map(f)
    }

    fn for_each<U,F>(self, f:F) where F : FnOnce(Self::Item) -> U {
        if let Some(x) = self { f(x); }
    }

    fn for_each_ref<U,F>(&self, f:F) where F : FnOnce(&Self::Item) -> U {
        if let Some(x) = self { f(x); }
    }
}
