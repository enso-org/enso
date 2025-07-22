//! Extension methods for `Option`.



// use crate::prelude::*;

/// Extension methods for `Option`.
pub trait OptionExt<T> {
    /// Checks if the option is `Some` and contains the given value.
    fn contains<U>(&self, x: &U) -> bool
    where U: PartialEq<T>;
}

impl<T> OptionExt<T> for Option<T> {
    fn contains<U>(&self, x: &U) -> bool
    where U: PartialEq<T> {
        match self {
            Some(y) => x.eq(y),
            None => false,
        }
    }
}
