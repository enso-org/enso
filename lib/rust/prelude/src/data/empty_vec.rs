//! A vector that may not contain any elements. However, it may have storage allocated.

use derivative::Derivative;



// ================
// === EmptyVec ===
// ================

/// A [`std::vec::Vec`] that is constrained to be empty.
#[allow(missing_docs)]
#[derive(Clone, Debug, Derivative, Eq, PartialEq)]
#[derivative(Default(bound = ""))]
pub struct EmptyVec<T> {
    data: Vec<T>,
}

impl<T> EmptyVec<T> {
    #[allow(missing_docs)]
    pub fn new() -> Self {
        Self::default()
    }

    /// Drop any elements from the given `Vec`, and give its allocated memory to this `EmptyVec`.
    /// It can be retrieved later with `take_storage`.
    pub fn set_storage(&mut self, mut data: Vec<T>) {
        data.clear();
        self.data = data;
    }

    /// Return a `Vec` containing no elements, whose allocated storage comes from the most recent
    /// call to `set_storage`, unless `take_storage` has been called since then. Subsequent calls
    /// until the next `set_storage` will return a newly-created `Vec` with no allocated memory.
    pub fn take_storage(&mut self) -> Vec<T> {
        std::mem::take(&mut self.data)
    }
}
