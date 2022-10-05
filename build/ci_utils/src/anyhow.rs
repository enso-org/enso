use crate::prelude::*;

use anyhow::Error;



pub trait ResultExt<T, E> {
    fn anyhow_err(self) -> Result<T>;

    #[allow(clippy::type_complexity)]
    fn flatten_fut(
        self,
    ) -> futures::future::Either<
        std::future::Ready<std::result::Result<T::Ok, T::Error>>,
        futures::future::IntoFuture<T>,
    >
    where T: TryFuture<Error: From<E>>;

    // fn flatten_fut(self) -> impl Future<Output = std::result::Result<T::Ok, T::Error>>
    // where T: TryFuture<Error: From<E>> {
    //     async move { fut?.into_future().await }
    // }
    // fn flatten_fut(self)
    // where T: TryFuture;
}

impl<T, E> ResultExt<T, E> for std::result::Result<T, E>
where E: Into<Error>
{
    fn anyhow_err(self) -> Result<T> {
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
