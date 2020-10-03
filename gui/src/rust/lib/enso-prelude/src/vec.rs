//! This module defines utilities for working with the `Vec` type.

/// Adds mapping methods to the `Vec` type.
pub trait VecOps {
    type Item;
    fn extended<I:IntoIterator<Item=Self::Item>>(self, iter:I) -> Self;
}

impl<T> VecOps for Vec<T> {
    type Item = T;
    fn extended<I:IntoIterator<Item=Self::Item>>(mut self, iter:I) -> Self {
        self.extend(iter);
        self
    }
}
