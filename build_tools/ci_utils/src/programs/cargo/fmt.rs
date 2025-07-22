use crate::prelude::*;

use crate::programs::Cargo;

use std::process::Stdio;



#[context("Failed to run cargo fmt on path '{}'", path.as_ref().display())]
pub async fn format(path: impl AsRef<Path>) -> Result {
    command()?.with_stdin(Stdio::null()).with_current_dir(&path).run_ok().await
}

/// Base command invoking cargo-fmt.
pub fn command() -> Result<Command> {
    Cargo.cmd().map(|c| c.with_arg("fmt"))
}
