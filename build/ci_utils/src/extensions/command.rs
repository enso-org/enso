use crate::prelude::*;

use std::fmt::Write;



pub trait CommandExt {
    // fn run_ok(&mut self, program: &impl Program) -> BoxFuture<'static, Result<()>>;
    //
    // fn output_ok(&mut self) -> BoxFuture<'static, Result<Output>>;
    // // TODO: `spawn` but does logs like some other methods. They all need a naming unification
    // pass. fn spawn_nicer(&mut self) -> Result<Child>;

    fn as_std(&self) -> &std::process::Command;

    fn describe(&self) -> String {
        let mut ret = String::new();
        let pretty_printed = format!("{:?}", self.as_std());
        let _ = write!(ret, "Command:\n\t{pretty_printed}");
        if let Some(cwd) = self.as_std().get_current_dir() {
            let _ = write!(ret, "\n\twith working directory: {}", cwd.display());
        };
        let env = self.as_std().get_envs();
        if !env.is_empty() {
            let _ = write!(ret, "\n\twith environment overrides:");
        }
        for (name, val) in self.as_std().get_envs() {
            let _ = write!(
                ret,
                "\n\t\t{}={}",
                name.to_string_lossy(),
                val.map_or(default(), OsStr::to_string_lossy)
            );
        }
        ret
        // ?self.as_std().get_program()
    }
}

#[allow(unused_qualifications)]
impl CommandExt for crate::program::command::Command {
    fn as_std(&self) -> &std::process::Command {
        self.inner.as_std()
    }
}


impl CommandExt for std::process::Command {
    fn as_std(&self) -> &std::process::Command {
        self
    }
}

impl CommandExt for tokio::process::Command {
    fn as_std(&self) -> &std::process::Command {
        self.as_std()
    }
    // fn run_ok(&mut self) -> BoxFuture<'static, Result<()>> {
    //     let pretty = self.describe();
    //     debug!("Will run: {}", pretty);
    //     let status = self.status();
    //     async move { status.await?.exit_ok().context(format!("Command failed: {}", pretty)) }
    //         .boxed()
    // }
    //
    // fn output_ok(&mut self) -> BoxFuture<'static, Result<Output>> {
    //     let pretty = self.describe();
    //     debug!("Will run: {}", pretty);
    //     let output = self.output();
    //     async move { output.await.context(format!("Command failed: {}", pretty)) }.boxed()
    // }
    //
    // fn spawn_nicer(&mut self) -> Result<Child> {
    //     let pretty = self.describe();
    //     debug!("Spawning {}", pretty);
    //     self.spawn().context(format!("Failed to spawn: {}", pretty))
    // }
}
