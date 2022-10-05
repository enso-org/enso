use crate::prelude::*;

use futures_util::future::ErrInto;
use futures_util::future::Map;
use futures_util::future::MapErr;
use futures_util::future::MapOk;
use futures_util::stream;
use futures_util::FutureExt as _;
use futures_util::TryFutureExt as _;



fn void<T>(_t: T) {}

pub trait FutureExt: Future {
    fn void(self) -> Map<Self, fn(Self::Output) -> ()>
    where Self: Sized {
        self.map(void)
    }
}

impl<T: ?Sized> FutureExt for T where T: Future {}

type FlattenResultFn<T, E> =
    fn(std::result::Result<std::result::Result<T, E>, E>) -> std::result::Result<T, E>;

pub trait TryFutureExt: TryFuture {
    fn void_ok(self) -> MapOk<Self, fn(Self::Ok) -> ()>
    where Self: Sized {
        self.map_ok(void)
    }

    fn anyhow_err(self) -> MapErr<Self, fn(Self::Error) -> anyhow::Error>
    where
        Self: Sized,
        // TODO: we should rely on `into` rather than `from`
        anyhow::Error: From<Self::Error>, {
        self.map_err(anyhow::Error::from)
    }

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


pub fn receiver_to_stream<T>(
    mut receiver: tokio::sync::mpsc::Receiver<T>,
) -> impl Stream<Item = T> {
    futures::stream::poll_fn(move |ctx| receiver.poll_recv(ctx))
}



pub trait TryStreamExt: TryStream {
    fn anyhow_err(self) -> stream::MapErr<Self, fn(Self::Error) -> anyhow::Error>
    where
        Self: Sized,
        // TODO: we should rely on `into` rather than `from`
        anyhow::Error: From<Self::Error>, {
        self.map_err(anyhow::Error::from)
    }
}

impl<T: ?Sized> TryStreamExt for T where T: TryStream {}
