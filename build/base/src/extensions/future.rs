//! Extensions to [`Future`]-related types.

use crate::prelude::*;

use futures_util::future::ErrInto;
use futures_util::future::Map;
use futures_util::future::MapErr;
use futures_util::future::MapOk;
use futures_util::stream;
use futures_util::FutureExt as _;
use futures_util::TryFutureExt as _;



/// Extension methods for [`Future`].
pub trait FutureExt: Future {
    /// Discard the result of this future.
    fn void(self) -> Map<Self, fn(Self::Output) -> ()>
    where Self: Sized {
        self.map(drop)
    }
}

impl<T: ?Sized> FutureExt for T where T: Future {}

type FlattenResultFn<T, E> =
    fn(std::result::Result<std::result::Result<T, E>, E>) -> std::result::Result<T, E>;

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

    /// Convert the error type of this future to [`anyhow::Error`].
    fn anyhow_err(self) -> MapErr<Self, fn(Self::Error) -> anyhow::Error>
    where
        Self: Sized,
        // TODO: we should rely on `into` rather than `from`
        anyhow::Error: From<Self::Error>, {
        self.map_err(anyhow::Error::from)
    }

    /// If the future is successful, apply the function to the result and return the new future.
    fn and_then_sync<T2, E2, F>(
        self,
        f: F,
    ) -> Map<MapOk<ErrInto<Self, E2>, F>, FlattenResultFn<T2, E2>>
    where
        Self: Sized,
        F: FnOnce(Self::Ok) -> std::result::Result<T2, E2>,
        Self::Error: Into<E2>,
    {
        self.err_into().map_ok(f).map(std::result::Result::flatten)
    }
}

impl<T: ?Sized> TryFutureExt for T where T: TryFuture {}

/// Extension methods for [`TryStream`], i.e. a [`Stream`] that produces [`Result`]s.
pub trait TryStreamExt: TryStream {
    /// Wrap all the errors into [`anyhow::Error`].
    fn anyhow_err(self) -> stream::MapErr<Self, fn(Self::Error) -> anyhow::Error>
    where
        Self: Sized,
        // TODO: we should rely on `into` rather than `from`
        anyhow::Error: From<Self::Error>, {
        self.map_err(anyhow::Error::from)
    }
}

impl<T: ?Sized> TryStreamExt for T where T: TryStream {}
