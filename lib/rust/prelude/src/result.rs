//! This module defines utilities for working with the `Result` type.



/// Adds utilities to the `Result` type.
pub trait ResultOps {
    type Item;
    type Error;

    /// Call the given handler if this is an error and promote `Result` to `Option`.
    fn handle_err<F>(self, f: F) -> Option<Self::Item>
    where F: FnOnce(Self::Error);
}

pub trait ResultUnwrapBoth {
    type Item;

    /// Unwrap either `Ok` or `Err`. Possible onl if both have the same type
    fn unwrap_both(self) -> Self::Item;
}

impl<T, E> ResultOps for Result<T, E> {
    type Item = T;
    type Error = E;

    fn handle_err<F>(self, f: F) -> Option<Self::Item>
    where F: FnOnce(Self::Error) {
        self.map_err(f).ok()
    }
}

impl<T> ResultUnwrapBoth for Result<T, T> {
    type Item = T;

    fn unwrap_both(self) -> Self::Item {
        match self {
            Ok(t) => t,
            Err(t) => t,
        }
    }
}
