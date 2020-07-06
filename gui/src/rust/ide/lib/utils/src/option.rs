//! This module provides utils for the standard Option<T>.

use std::borrow::Borrow;

/// Extension trait for `Vec<T>` with general-purpose utility functions.
pub trait OptionExt<T> : Borrow<Option<T>> {
    /// Returns true iff option contains Some with value matching given predicate.
    fn contains_if(&self, f:impl FnOnce(&T) -> bool) -> bool {
        self.borrow().as_ref().map_or(false,f)
    }
}

impl<T> OptionExt<T> for Option<T> {}
