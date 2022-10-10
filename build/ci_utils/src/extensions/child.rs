use crate::prelude::*;



pub trait ChildExt {
    fn wait_ok(&mut self) -> BoxFuture<Result>;
}

impl ChildExt for tokio::process::Child {
    fn wait_ok(&mut self) -> BoxFuture<Result> {
        async move { self.wait().await?.exit_ok().anyhow_err() }.boxed()
    }
}
