//! This module defines utilities for working with the `Rc` and `Weak` types.

use super::option::*;

use std::rc::Rc;
use std::rc::Weak;



// TODO[WD,AO]: Think about merging it with `OptionOps`.
/// Mapping methods to the `Weak` type.
pub trait WeakOps {
    type Target;
    fn for_each<U, F>(self, f: F)
    where F: FnOnce(Self::Target) -> U;
    fn for_each_ref<U, F>(&self, f: F)
    where F: FnOnce(&Self::Target) -> U;
    /// Check for pointer equality between the strong and weak pointer.
    fn strong_ptr_eq(&self, strong: &Self::Target) -> bool;
}

impl<T> WeakOps for Weak<T> {
    type Target = Rc<T>;

    fn for_each<U, F>(self, f: F)
    where F: FnOnce(Self::Target) -> U {
        self.upgrade().for_each(f)
    }

    fn for_each_ref<U, F>(&self, f: F)
    where F: FnOnce(&Self::Target) -> U {
        self.upgrade().for_each_ref(f)
    }

    fn strong_ptr_eq(&self, strong: &Rc<T>) -> bool {
        std::ptr::eq(Rc::as_ptr(strong), self.as_ptr())
    }
}
