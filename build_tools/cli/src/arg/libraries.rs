use crate::prelude::*;

use clap::Args;
use clap::Subcommand;



#[derive(Subcommand, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    /// Check syntax of all Enso source files.
    Lint,
}

#[derive(Args, Clone, Copy, Debug)]
pub struct Target {
    #[clap(subcommand)]
    pub action: Command,
}
