use crate::prelude::*;

use futures_util::future::OptionFuture;



#[derive(Copy, Clone, Debug)]
pub enum AsyncPolicy {
    Sequential,
    FutureParallelism,
    TaskParallelism,
}

pub async fn join_all<I, F: Future<Output = std::result::Result<T, E>>, T, E>(
    futures: I,
    parallel: AsyncPolicy,
) -> Vec<Result<T>>
where
    I: IntoIterator<Item = F>,
    F: Send + 'static,
    T: Send + 'static,
    E: Into<anyhow::Error> + Send + 'static,
{
    match parallel {
        AsyncPolicy::Sequential => {
            let mut ret = Vec::new();
            for future in futures {
                ret.push(future.await.anyhow_err());
            }
            ret
        }
        AsyncPolicy::FutureParallelism =>
            futures::future::join_all(futures).await.into_iter().map(|r| r.anyhow_err()).collect(),
        AsyncPolicy::TaskParallelism => {
            let tasks = futures
                .into_iter()
                .map(|future| async move { tokio::task::spawn(future).await?.anyhow_err() });
            futures::future::join_all(tasks).await
        }
    }
}

pub async fn try_join_all<I, F: Future<Output = std::result::Result<T, E>>, T, E>(
    futures: I,
    parallel: AsyncPolicy,
) -> Result<Vec<T>>
where
    I: IntoIterator<Item = F>,
    F: Send + 'static,
    T: Send + 'static,
    E: Into<anyhow::Error> + Send + 'static,
{
    match parallel {
        AsyncPolicy::Sequential => {
            let mut ret = Vec::new();
            for future in futures {
                ret.push(future.await.anyhow_err()?);
            }
            Ok(ret)
        }
        AsyncPolicy::FutureParallelism => futures::future::try_join_all(futures).await.anyhow_err(),
        AsyncPolicy::TaskParallelism => {
            let tasks = futures
                .into_iter()
                .map(|future| async move { tokio::task::spawn(future).await?.anyhow_err() });
            futures::future::try_join_all(tasks).await
        }
    }
}

pub fn perhaps<F: Future>(should_do: bool, f: impl FnOnce() -> F) -> OptionFuture<F> {
    should_do.then(f).into()
}

// pub fn perhaps_spawn_try<'a, F>(
//     should_do: bool,
//     f: impl FnOnce() -> F + 'a,
// ) -> BoxFuture<'static, Result<Option<F::Ok>>>
// where
//     F: TryFuture + Send + 'static,
//     F::Ok: Send + 'static,
//     F::Error: Send + Sync + 'static,
//     anyhow::Error: From<F::Error>,
// {
//     let job = should_do.then(|| tokio::spawn(f().into_future()));
//     async move {
//         if let Some(job) = job {
//             Ok(Some(job.await??))
//         } else {
//             Ok(None)
//         }
//     }
//     .boxed()
// }
