//! This module defines utilities for working with the `Result` type.

/// Adds utilities to the `Result` type.
pub trait ResultOps {
    type Item;
    type Error;

    /// Call the given handler if this is an error and promote `Result` to `Option`.
    fn handle_err<F>(self, f:F) -> Option<Self::Item> where F : FnOnce(Self::Error);
}

impl<T,E> ResultOps for Result<T,E> {
    type Item  = T;
    type Error = E;

    fn handle_err<F>(self, f:F) -> Option<Self::Item> where F : FnOnce(Self::Error) {
        self.map_err(f).ok()
    }
}
