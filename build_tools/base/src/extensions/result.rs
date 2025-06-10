//! Extension methods for [`Result`].

use crate::prelude::*;

use futures::future::Either;
use std::future::Ready;



/// Extension methods for [`Result`].
pub trait ResultExt<T, E>: Sized {
    /// Maps the `Ok` value to a [`Future`] value. If the result is `Err`, the error is returned
    /// as a [`std::future::Ready`] future.
    fn and_then_async<'a, T2, E2, F, Fut>(
        self,
        f: F,
    ) -> Either<Fut, Ready<std::result::Result<T2, E2>>>
    where
        F: FnOnce(T) -> Fut,
        Fut: Future<Output = std::result::Result<T2, E2>> + Send + 'a,
        E: Into<E2>,
        T2: Send + 'a,
        E2: Send + 'a;

    /// Checks if the result is `Ok` and contains the given value.
    fn contains<U>(&self, x: &U) -> bool
    where U: PartialEq<T>;
}

impl<T, E> ResultExt<T, E> for std::result::Result<T, E> {
    fn and_then_async<'a, T2, E2, F, Fut>(
        self,
        f: F,
    ) -> Either<Fut, Ready<std::result::Result<T2, E2>>>
    where
        F: FnOnce(T) -> Fut,
        Fut: Future<Output = std::result::Result<T2, E2>> + Send + 'a,
        E: Into<E2>,
        T2: Send + 'a,
        E2: Send + 'a,
    {
        match self {
            Ok(v) => f(v).left_future(),
            Err(e) => ready(Err(e.into())).right_future(),
        }
    }

    fn contains<U>(&self, x: &U) -> bool
    where U: PartialEq<T> {
        match self {
            Ok(y) => x == y,
            Err(_) => false,
        }
    }
}
