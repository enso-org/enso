//! Extensions to [`Future`]-related types.

use crate::prelude::*;

use futures_util::future::MapOk;
use futures_util::FutureExt as _;
use futures_util::TryFutureExt as _;



/// Extension methods for [`Future`].
pub trait FutureExt: Future {}

impl<T: ?Sized> FutureExt for T where T: Future {}

/// Extension methods for [`TryFuture`], i.e. the Result-yielding [`Future`]
pub trait TryFutureExt: TryFuture {
    /// Discard the result of successful future.
    fn void_ok(self) -> MapOk<Self, fn(Self::Ok) -> ()>
    where Self: Sized {
        self.map_ok(drop)
    }

    /// Convert the error type of this future to [`anyhow::Error`] and add the context.
    fn context(
        self,
        context: impl Display + Send + Sync + 'static,
    ) -> BoxFuture<'static, Result<Self::Ok>>
    where
        Self: Sized + Send + 'static,
        Self::Error: Into<anyhow::Error> + Send + Sync + 'static,
    {
        self.map_err(|err| err.into().context(context)).boxed()
    }

    /// Convert the error type of this future to [`anyhow::Error`] and add the context.
    fn with_context<F, C>(self, context: F) -> BoxFuture<'static, Result<Self::Ok>>
    where
        Self: Sized + Send + 'static,
        std::result::Result<Self::Ok, Self::Error>: anyhow::Context<Self::Ok, Self::Error>,
        F: FnOnce() -> C + Send + Sync + 'static,
        C: Display + Send + Sync + 'static, {
        self.into_future().map(|res| res.with_context(context)).boxed()
    }
}

impl<T: ?Sized> TryFutureExt for T where T: TryFuture {}
