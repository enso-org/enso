//! This module provides utils for the standard Vec<T>.

/// Extension trait for `Vec<T>` with general-purpose utility functions.
pub trait VecExt<T> : AsMut<Vec<T>> {
    /// Attempts to remove `T` if its `index` is valid. If not, it returns `None`.
    fn try_remove(&mut self, index:usize) -> Option<T> {
        let vec = self.as_mut();
        if index < vec.len() {
            Some(vec.remove(index))
        } else {
            None
        }
    }

    /// Attempts to remove the first element of `Vec<T>`, returns `None` if its length is zero.
    fn pop_front(&mut self) -> Option<T> {
        self.try_remove(0)
    }
}

impl<T> VecExt<T> for Vec<T> {}
