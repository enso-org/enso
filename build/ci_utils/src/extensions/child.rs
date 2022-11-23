use crate::prelude::*;

use sysinfo::Pid;



/// Extension methods for [`tokio::process::Child`].
pub trait ChildExt {
    /// Wait for the process completion and represent non-zero exit code as an error.
    fn wait_ok(&mut self) -> BoxFuture<Result>;

    /// Kill the process and all its descendants.
    ///
    /// Note that in case of partial failures, the function will at most log the error and continue.
    fn kill_subtree(&self);
}

impl ChildExt for tokio::process::Child {
    fn wait_ok(&mut self) -> BoxFuture<Result> {
        async move { self.wait().await?.exit_ok().anyhow_err() }.boxed()
    }

    fn kill_subtree(&self) {
        let Some(pid) = self.id().map(Pid::from_u32) else {
            // Not necessarily that bad, as the process might have already exited. 
            // Still, we don't know about its descendants, so we cannot kill them.
            warn!("Failed to get PID of the process.");
            return;
        };
        crate::process::kill_process_subtree(pid)
    }
}
