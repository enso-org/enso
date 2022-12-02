//! Extension methods for [`Result`].

use crate::prelude::*;

use futures::future::Either;
use std::future::Ready;



/// Extension methods for [`Result`].
pub trait ResultExt<T, E>: Sized {
    /// Maps the value and wraps it as a [`Future`].
    #[allow(clippy::type_complexity)]
    fn map_async<'a, T2, F, Fut>(
        self,
        f: F,
    ) -> Either<
        futures::future::Map<Fut, fn(T2) -> std::result::Result<T2, E>>,
        Ready<std::result::Result<T2, E>>,
    >
    where
        F: FnOnce(T) -> Fut,
        Fut: Future<Output = T2> + 'a;

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


    /// Executes another future if this is an error. The error value is passed to a closure to
    /// create this subsequent future.
    fn or_else_async<F, Fut>(self, f: F) -> Either<Ready<Self>, futures::future::IntoFuture<Fut>>
    where
        F: FnOnce(E) -> Fut,
        Fut: TryFuture<Ok = T, Error = E>;

    /// Convert the error type to [`anyhow::Error`].
    ///
    /// If there are additional context-specific information, use [`context`] instead.
    fn anyhow_err(self) -> Result<T>
    where E: Into<anyhow::Error>;

    /// Convert the `[Result]<[Future]>` to `Future<Result>`.
    fn flatten_fut(
        self,
    ) -> Either<Ready<std::result::Result<T::Ok, T::Error>>, futures::future::IntoFuture<T>>
    where T: TryFuture<Error: From<E>>;
}

impl<T, E> ResultExt<T, E> for std::result::Result<T, E> {
    fn map_async<'a, T2, F, Fut>(
        self,
        f: F,
    ) -> Either<
        futures::future::Map<Fut, fn(T2) -> std::result::Result<T2, E>>,
        Ready<std::result::Result<T2, E>>,
    >
    where
        F: FnOnce(T) -> Fut,
        Fut: Future<Output = T2> + 'a,
    {
        match self {
            Ok(v) => f(v).map(Ok as fn(T2) -> std::result::Result<T2, E>).left_future(),
            Err(e) => ready(Err(e)).right_future(),
        }
    }

    fn or_else_async<'a, F, Fut>(
        self,
        f: F,
    ) -> Either<Ready<Self>, futures::future::IntoFuture<Fut>>
    where
        F: FnOnce(E) -> Fut,
        Fut: TryFuture<Ok = T, Error = E>,
    {
        match self {
            Ok(v) => ready(Ok(v)).left_future(),
            Err(e) => f(e).into_future().right_future(),
        }
    }

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

    fn anyhow_err(self) -> Result<T>
    where E: Into<anyhow::Error> {
        self.map_err(E::into)
    }

    fn flatten_fut(
        self,
    ) -> Either<Ready<std::result::Result<T::Ok, T::Error>>, futures::future::IntoFuture<T>>
    where T: TryFuture<Error: From<E>> {
        match self {
            Ok(fut) => fut.into_future().right_future(),
            Err(e) => ready(Err(T::Error::from(e))).left_future(),
        }
    }
}
