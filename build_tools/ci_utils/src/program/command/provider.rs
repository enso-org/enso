use crate::prelude::*;



pub trait CommandProvider {
    fn command(&self) -> Result<Command>;
}

pub trait CommandProviderExt: CommandProvider {
    fn call_arg(&self, arg: impl AsRef<OsStr>) -> BoxFuture<'static, Result> {
        self.call_args(once(arg))
    }

    fn call_args(&self, args: impl IntoIterator<Item: AsRef<OsStr>>) -> BoxFuture<'static, Result> {
        self.command().and_then_async(|mut cmd| cmd.args(args).run_ok()).boxed()
    }
}

impl<T: CommandProvider + ?Sized> CommandProviderExt for T {}
