use crate::prelude::*;

use crate::env::accessor::TypedVariable;

use anyhow::Context;
use std::borrow::BorrowMut;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::process::ExitStatus;
use std::process::Output;
use std::process::Stdio;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncRead;
use tokio::io::BufReader;
use tokio::process::Child;
use tokio::task::JoinHandle;
use tracing::field;


// ==============
// === Export ===
// ==============

pub mod provider;



#[macro_export]
macro_rules! new_command_type {
    ($program_name:ident, $command_name:ident) => {
        #[derive(Debug, Deref, DerefMut)]
        pub struct $command_name(pub $crate::program::command::Command);

        impl Borrow<$crate::program::command::Command> for $command_name {
            fn borrow(&self) -> &$crate::program::command::Command {
                &self.0
            }
        }

        impl BorrowMut<$crate::program::command::Command> for $command_name {
            fn borrow_mut(&mut self) -> &mut $crate::program::command::Command {
                &mut self.0
            }
        }

        impl From<$crate::program::command::Command> for $command_name {
            fn from(inner: $crate::program::command::Command) -> Self {
                $command_name(inner)
            }
        }

        impl From<$command_name> for $crate::program::command::Command {
            fn from(inner: $command_name) -> Self {
                inner.0
            }
        }

        impl $command_name {
            pub fn into_inner(self) -> $crate::program::command::Command {
                self.0
            }
        }

        impl $crate::program::command::IsCommandWrapper for $command_name {
            fn borrow_mut_command(&mut self) -> &mut tokio::process::Command {
                self.0.borrow_mut_command()
            }
        }

        impl $crate::program::command::MyCommand<$program_name> for $command_name {}
    };
    () => {
        new_command_type!(Command);
    };
}



pub trait MyCommand<P: Program>: BorrowMut<Command> + From<Command> + Into<Command> {
    fn new_program<S: AsRef<OsStr>>(program: S) -> Self {
        let inner = tokio::process::Command::new(program);
        let inner = Command::new_over::<P>(inner);
        Self::from(inner)
    }

    fn spawn(&mut self) -> Result<Child> {
        self.borrow_mut().spawn().anyhow_err()
    }
}

pub trait IsCommandWrapper {
    fn borrow_mut_command(&mut self) -> &mut tokio::process::Command;

    fn with_applied<M: Manipulator>(mut self, manipulator: &M) -> Self
    where Self: Sized {
        manipulator.apply(&mut self);
        self
    }

    fn apply<M: Manipulator>(&mut self, manipulator: &M) -> &mut Self {
        manipulator.apply(self);
        self
    }

    fn apply_iter(&mut self, iter: impl IntoIterator<Item = impl Manipulator>) -> &mut Self {
        for manipulator in iter {
            self.apply(&manipulator);
        }
        self
    }

    fn apply_opt<M: Manipulator>(&mut self, manipulator: Option<&M>) -> &mut Self {
        if let Some(m) = manipulator {
            self.apply(m);
        }
        self
    }

    fn try_applying<M: FallibleManipulator>(&mut self, manipulator: &M) -> Result<&mut Self> {
        manipulator.try_applying(self).map(|_| self)
    }

    fn set_env<T: TypedVariable, V: Borrow<T::Borrowed> + ?Sized>(
        &mut self,
        variable: T,
        value: &V,
    ) -> Result<&mut Self> {
        self.env(variable.name(), variable.generate(value.borrow())?);
        Ok(self)
    }

    fn set_env_opt<T: TypedVariable, V: Borrow<T::Borrowed> + ?Sized>(
        &mut self,
        variable: T,
        value: Option<&V>,
    ) -> Result<&mut Self> {
        if let Some(value) = value {
            self.set_env(variable, value)
        } else {
            Ok(self)
        }
    }

    ///////////

    fn arg<S: AsRef<OsStr>>(&mut self, arg: S) -> &mut Self {
        self.borrow_mut_command().arg(arg);
        self
    }

    fn args<I, S>(&mut self, args: I) -> &mut Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>, {
        self.borrow_mut_command().args(args);
        self
    }

    fn env<K, V>(&mut self, key: K, val: V) -> &mut Self
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>, {
        self.borrow_mut_command().env(key, val);
        self
    }

    fn envs<I, K, V>(&mut self, vars: I) -> &mut Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: AsRef<OsStr>,
        V: AsRef<OsStr>, {
        self.borrow_mut_command().envs(vars);
        self
    }

    fn env_remove<K: AsRef<OsStr>>(&mut self, key: K) -> &mut Self {
        self.borrow_mut_command().env_remove(key);
        self
    }

    fn env_clear(&mut self) -> &mut Self {
        self.borrow_mut_command().env_clear();
        self
    }

    fn current_dir<Pa: AsRef<Path>>(&mut self, dir: Pa) -> &mut Self {
        self.borrow_mut_command().current_dir(dir);
        self
    }

    fn stdin<T: Into<Stdio>>(&mut self, cfg: T) -> &mut Self {
        self.borrow_mut_command().stdin(cfg);
        self
    }

    fn stdout<T: Into<Stdio>>(&mut self, cfg: T) -> &mut Self {
        self.borrow_mut_command().stdout(cfg);
        self
    }

    fn stderr<T: Into<Stdio>>(&mut self, cfg: T) -> &mut Self {
        self.borrow_mut_command().stderr(cfg);
        self
    }

    fn kill_on_drop(&mut self, kill_on_drop: bool) -> &mut Self {
        self.borrow_mut_command().kill_on_drop(kill_on_drop);
        self
    }

    #[cfg(windows)]
    #[cfg_attr(docsrs, doc(cfg(windows)))]
    fn creation_flags(&mut self, flags: u32) -> &mut Self {
        self.borrow_mut_command().creation_flags(flags);
        self
    }

    #[cfg(unix)]
    #[cfg_attr(docsrs, doc(cfg(unix)))]
    fn uid(&mut self, id: u32) -> &mut Self {
        self.borrow_mut_command().uid(id);
        self
    }

    #[cfg(unix)]
    #[cfg_attr(docsrs, doc(cfg(unix)))]
    fn gid(&mut self, id: u32) -> &mut Self {
        self.borrow_mut_command().gid(id);
        self
    }

    // fn spawn(&mut self) -> Result<Child> {
    //     self.borrow_mut_command().spawn().anyhow_err()
    // }
    //
    //
    // fn status(&mut self) -> BoxFuture<'static, Result<ExitStatus>> {
    //     let fut = self.borrow_mut_command().status();
    //     async move { fut.await.anyhow_err() }.boxed()
    // }
    //
    // fn output(&mut self) -> BoxFuture<'static, Result<Output>> {
    //     let fut = self.borrow_mut_command().output();
    //     async move { fut.await.anyhow_err() }.boxed()
    // }



    /// Value-based variant of [`Self::current_dir`], for convenience.
    fn with_current_dir(self, dir: impl AsRef<Path>) -> Self
    where Self: Sized {
        let mut this = self;
        this.current_dir(dir);
        this
    }

    /// Value-based variant of [`Self::with_stdin`], for convenience.
    fn with_stdin(self, stdin: Stdio) -> Self
    where Self: Sized {
        let mut this = self;
        this.stdin(stdin);
        this
    }
}

impl<T: BorrowMut<tokio::process::Command>> IsCommandWrapper for T {
    fn borrow_mut_command(&mut self) -> &mut tokio::process::Command {
        self.borrow_mut()
    }
}

impl<P: Program> MyCommand<P> for Command {
    fn new_program<S: AsRef<OsStr>>(program: S) -> Self {
        let inner = tokio::process::Command::new(program);
        Self::new_over::<P>(inner)
    }
}

pub trait CommandOption {
    fn arg(&self) -> Option<&str> {
        None
    }
    fn args(&self) -> Vec<&str> {
        vec![]
    }
}

pub struct Command {
    pub inner:          tokio::process::Command,
    pub status_checker: Arc<dyn Fn(ExitStatus) -> Result + Send + Sync>,
}

impl Borrow<tokio::process::Command> for Command {
    fn borrow(&self) -> &tokio::process::Command {
        &self.inner
    }
}

impl BorrowMut<tokio::process::Command> for Command {
    fn borrow_mut(&mut self) -> &mut tokio::process::Command {
        &mut self.inner
    }
}

impl Debug for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.inner)
    }
}

impl Command {
    pub fn new<S: AsRef<OsStr>>(program: S) -> Command {
        let inner = tokio::process::Command::new(program);
        let status_checker = Arc::new(|status: ExitStatus| status.exit_ok().anyhow_err());
        Self { inner, status_checker }
    }

    pub fn new_over<P: Program + 'static>(inner: tokio::process::Command) -> Self {
        Command { inner, status_checker: Arc::new(P::handle_exit_status) }
    }

    pub fn spawn_intercepting(&mut self) -> Result<Child> {
        self.stdout(Stdio::piped());
        self.stderr(Stdio::piped());

        let program = self.inner.as_std().get_program();
        let program = Path::new(program).file_stem().unwrap_or_default().to_os_string();
        let program = program.to_string_lossy();

        let mut child = self.spawn()?;

        // FIXME unwraps
        spawn_log_processor(format!("{program}ℹ️"), child.stdout.take().unwrap());
        spawn_log_processor(format!("{program}⚠️"), child.stderr.take().unwrap());
        Ok(child)
    }

    pub fn run_ok(&mut self) -> BoxFuture<'static, Result<()>> {
        let pretty = self.describe();
        let span = info_span!(
            "Running process.",
            status = tracing::field::Empty,
            pid = tracing::field::Empty,
            command = tracing::field::Empty,
        )
        .entered();
        let child = self.spawn_intercepting();
        let status_checker = self.status_checker.clone();
        async move {
            let mut child = child?;
            let status = child
                .wait()
                .inspect_ok(|exit_status| {
                    tracing::Span::current().record("status", exit_status.code());
                })
                .await?;
            status_checker(status).context(format!("Command failed: {pretty}"))
        }
        .instrument(span.exit())
        .boxed()
    }

    pub fn output_ok(&mut self) -> BoxFuture<'static, Result<Output>> {
        let pretty = self.describe();
        let span = info_span!(
            "Running process for the output.",
            status = tracing::field::Empty,
            pid = tracing::field::Empty,
            command = tracing::field::Empty,
        )
        .entered();

        self.stdout(Stdio::piped());
        self.stderr(Stdio::piped());
        let child = self.spawn();
        let status_checker = self.status_checker.clone();
        async move {
            let child = child?;
            let output =
                child.wait_with_output().await.context("Failed while waiting for output.")?;
            tracing::Span::current().record("status", output.status.code());
            status_checker(output.status).with_context(|| {
                format!(
                    "Stdout:\n{}\n\nStderr:\n{}\n",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                )
            })?;
            Result::Ok(output)
        }
        .map_err(move |e| e.context(format!("Failed to get output of the command: {pretty}")))
        .instrument(span.exit())
        .boxed()
    }

    pub fn run_stdout(&mut self) -> BoxFuture<'static, Result<String>> {
        let output = self.output_ok();
        async move {
            output
                .await?
                .into_stdout_string()
                .context("Failed to decode standard output as UTF8 text.")
        }
        .boxed()
    }

    pub fn spawn(&mut self) -> Result<Child> {
        let pretty = self.describe();

        let current_span = tracing::Span::current();
        if current_span.field("command").is_some() {
            tracing::Span::current().record("command", field::display(&pretty));
            debug!("Spawning.");
        } else {
            debug!("Spawning {}.", pretty);
        }

        self.inner.spawn().context(format!("Failed to spawn: {pretty}")).inspect(|child| {
            if let Some(pid) = child.id() {
                current_span.record("pid", pid);
            }
        })
    }

    // pub fn status(&mut self) -> BoxFuture<'static, Result<ExitStatus>> {
    //     let fut = self.borrow_mut_command().status();
    //     async move { fut.await.anyhow_err() }.boxed()
    // }
    //
    // pub fn output(&mut self) -> BoxFuture<'static, Result<Output>> {
    //     let fut = self.borrow_mut_command().output();
    //     async move { fut.await.anyhow_err() }.boxed()
    // }
}

impl Command {
    pub fn with_arg(self, arg: impl AsRef<OsStr>) -> Self {
        let mut this = self;
        this.arg(arg);
        this
    }

    pub fn with_args(self, args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> Self {
        let mut this = self;
        this.args(args);
        this
    }

    pub fn with_stdin(self, stdin: Stdio) -> Self {
        let mut this = self;
        this.stdin(stdin);
        this
    }

    pub fn with_stdout(self, stdout: Stdio) -> Self {
        let mut this = self;
        this.stdout(stdout);
        this
    }

    pub fn with_stderr(self, stderr: Stdio) -> Self {
        let mut this = self;
        this.stderr(stderr);
        this
    }
}

pub fn spawn_log_processor(
    prefix: String,
    out: impl AsyncRead + Send + Unpin + 'static,
) -> JoinHandle<Result> {
    tokio::task::spawn(
        async move {
            trace!("{prefix} <START>");
            let bufread = BufReader::new(out);
            let mut lines = bufread.split(b'\n');
            while let Some(line_bytes) = lines.next_segment().await? {
                match String::from_utf8(line_bytes) {
                    Ok(line) => {
                        let line = line.trim_end_matches('\r');
                        info!("{prefix} {line}");
                    }
                    Err(e) => {
                        error!("{prefix} Failed to decode a line from output: {e}");
                        warn!(
                            "{prefix} Raw buffer: {:?}. Decoded with placeholders: {}",
                            e.as_bytes(),
                            String::from_utf8_lossy(e.as_bytes())
                        );
                    }
                }
            }
            trace!("{prefix} <ENDUT>");
            Result::Ok(())
        }
        .inspect_err(|e| error!("Fatal error while processing process output: {e}")),
    )
}

pub trait Manipulator {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C);
}

impl Manipulator for String {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.arg(self);
    }
}

impl Manipulator for &str {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.arg(self);
    }
}

impl Manipulator for OsString {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.arg(self);
    }
}

impl<T: Manipulator> Manipulator for Option<T> {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        if let Some(value) = self {
            value.apply(command);
        }
    }
}

pub trait FallibleManipulator {
    fn try_applying<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) -> Result;
}


#[cfg(test)]
mod tests {
    // use super::*;
    // use crate::global::new_spinner;
    // // use crate::global::println;
    // use tokio::io::AsyncBufReadExt;
    // use tokio::io::AsyncRead;
    // use tokio::io::BufReader;
    // use tokio::process::ChildStdout;
    // use tokio::task::JoinHandle;

    // pub fn spawn_log_processor(
    //     prefix: String,
    //     out: impl AsyncRead + Send + Unpin + 'static,
    // ) -> JoinHandle<Result> {
    //     tokio::task::spawn(async move {
    //         let bufread = BufReader::new(out);
    //         let mut lines = bufread.lines();
    //         while let Some(line) = lines.next_line().await? {
    //             println(format!("{} {}", prefix, line))
    //         }
    //         println(format!("{} {}", prefix, "<ENDUT>"));
    //         Result::Ok(())
    //     })
    // }U
    //
    // pub fn spawn_logged(cmd: &mut Command) {
    //     cmd.stdout(Stdio::piped());
    //     cmd.stderr(Stdio::piped());
    // }
    //
    // #[tokio::test]
    // async fn test_cmd_out_interception() -> Result {
    //     pretty_env_logger::init();
    //     let mut cmd = Command::new("cargo");
    //     cmd.arg("update");
    //     cmd.stdout(Stdio::piped());
    //     cmd.stderr(Stdio::piped());
    //
    //     let mut child = cmd.spawn()?;
    //     spawn_log_processor("[out]".into(), child.stdout.take().unwrap());
    //     spawn_log_processor("[err]".into(), child.stderr.take().unwrap());
    //     let bar = new_spinner(format!("Running {:?}", cmd));
    //     child.wait().await?;
    //     Ok(())
    // }
    //
    // #[tokio::test]
    // async fn spawning() -> Result {
    //     println!("Start");
    //     tokio::process::Command::new("python").spawn()?.wait().await?;
    //     println!("Finish");
    //     Ok(())
    // }
}
