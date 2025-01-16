use crate::prelude::*;

use futures::future::OptionFuture;



#[derive(Copy, Clone, Debug)]
pub enum AsyncPolicy {
    Sequential,
    FutureParallelism,
}

pub async fn join_all<I, F, T, E>(futures: I, parallel: AsyncPolicy) -> Vec<Result<T>>
where
    I: IntoIterator<Item = F>,
    F: Future<Output = std::result::Result<T, E>> + Send + 'static,
    T: Send + 'static,
    E: Into<anyhow::Error> + Send + 'static, {
    match parallel {
        AsyncPolicy::Sequential => {
            let mut ret = Vec::new();
            for future in futures {
                ret.push(future.await.map_err(Into::into));
            }
            ret
        }
        AsyncPolicy::FutureParallelism => futures::future::join_all(futures)
            .await
            .into_iter()
            .map(|r| r.map_err(Into::into))
            .collect(),
    }
}

pub fn perhaps<F: Future>(should_do: bool, f: impl FnOnce() -> F) -> OptionFuture<F> {
    should_do.then(f).into()
}
