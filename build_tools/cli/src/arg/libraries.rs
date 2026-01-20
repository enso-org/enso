use crate::prelude::*;

use clap::Args;
use clap::Subcommand;

#[derive(Subcommand, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    /// Run enso parser on Enso source files.
    CheckSyntax,
    /// Run a linter on Enso source files.
    Lint,
}

#[derive(Args, Clone, Copy, Debug)]
pub struct Target {
    #[clap(subcommand)]
    pub action: Command,
}
