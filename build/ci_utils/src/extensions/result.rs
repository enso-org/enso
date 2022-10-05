use crate::prelude::*;



pub trait ResultExt<T, E>: Sized {
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
}
