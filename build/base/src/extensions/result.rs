//! Extension methods for [`Result`].

use crate::prelude::*;



/// Extension methods for [`Result`].
pub trait ResultExt<T, E>: Sized {
    /// Maps the value and wraps it as a [`Future`].
    #[allow(clippy::type_complexity)]
    fn map_async<'a, T2, F, Fut>(
        self,
        f: F,
    ) -> futures::future::Either<
        futures::future::Map<Fut, fn(T2) -> std::result::Result<T2, E>>,
        std::future::Ready<std::result::Result<T2, E>>,
    >
    where
        F: FnOnce(T) -> Fut,
        Fut: Future<Output = T2> + 'a;

    /// Maps the `Ok` value to a [`Future`] value. If the result is `Err`, the error is returned
    /// as a [`std::future::Ready`] future.
    fn and_then_async<'a, T2, E2, F, Fut>(
        self,
        f: F,
    ) -> futures::future::Either<Fut, std::future::Ready<std::result::Result<T2, E2>>>
    where
        F: FnOnce(T) -> Fut,
        Fut: Future<Output = std::result::Result<T2, E2>> + Send + 'a,
        E: Into<E2>,
        T2: Send + 'a,
        E2: Send + 'a;


    /// Convert the error type to [`anyhow::Error`].
    ///
    /// If there are additional context-specific information, use [`context`] instead.
    fn anyhow_err(self) -> Result<T>
    where E: Into<anyhow::Error>;

    /// Convert the `[Result]<[Future]>` to `Future<Result>`.
    fn flatten_fut(
        self,
    ) -> futures::future::Either<
        std::future::Ready<std::result::Result<T::Ok, T::Error>>,
        futures::future::IntoFuture<T>,
    >
    where T: TryFuture<Error: From<E>>;
}

impl<T, E> ResultExt<T, E> for std::result::Result<T, E> {
    fn map_async<'a, T2, F, Fut>(
        self,
        f: F,
    ) -> futures::future::Either<
        futures::future::Map<Fut, fn(T2) -> std::result::Result<T2, E>>,
        std::future::Ready<std::result::Result<T2, E>>,
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


    fn and_then_async<'a, T2, E2, F, Fut>(
        self,
        f: F,
    ) -> futures::future::Either<Fut, std::future::Ready<std::result::Result<T2, E2>>>
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
    ) -> futures::future::Either<
        std::future::Ready<std::result::Result<T::Ok, T::Error>>,
        futures::future::IntoFuture<T>,
    >
    where T: TryFuture<Error: From<E>> {
        match self {
            Ok(fut) => fut.into_future().right_future(),
            Err(e) => ready(Err(T::Error::from(e))).left_future(),
        }
    }
}
