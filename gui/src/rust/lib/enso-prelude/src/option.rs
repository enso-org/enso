//! This module defines utilities for working with the `Option` type.

/// Adds mapping methods to the `Option` type.
pub trait OptionOps {
    type Item;
    fn map_none     <F>     (self  , f:F) -> Self      where F : FnOnce();
    fn map_ref      <U,F>   (&self , f:F) -> Option<U> where F : FnOnce(&Self::Item) -> U;
    fn for_each     <U,F>   (self  , f:F)              where F : FnOnce(Self::Item)  -> U;
    fn for_each_ref <U,F>   (&self , f:F)              where F : FnOnce(&Self::Item) -> U;
    fn zip          <U>     (self  , other:Option<U>)      -> Option<(Self::Item,U)>;
    fn zip_with     <U,F,R> (self  , other:Option<U>, f:F) -> Option<R>
        where F:FnOnce(Self::Item,U) -> R;
    /// Returns true if option contains Some with value matching given predicate.
    fn contains_if  <F>   (&self, f:F) -> bool      where F : FnOnce(&Self::Item) -> bool;
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

    fn zip<U>(self, other:Option<U>) -> Option<(T,U)> {
        self.zip_with(other, |a,b| (a,b))
    }

    fn zip_with<U,F,R>(self, other:Option<U>, f:F) -> Option<R>
    where F:FnOnce(T,U) -> R {
        Some(f(self?,other?))
    }

    fn map_none<F>(self, f:F) -> Self
    where F:FnOnce(), T:Sized {
        if self.is_none() { f() }
        self
    }

    fn contains_if<F>(&self, f:F) -> bool where F : FnOnce(&Self::Item) -> bool {
        self.as_ref().map_or(false,f)
    }
}
