use crate::prelude::*;

use clap::Args;
use clap::Subcommand;



#[derive(Subcommand, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    /// Generate Java.
    Build,
    /// Generate Java and run self-tests.
    Test,
}

#[derive(Args, Clone, Copy, Debug)]
pub struct Target {
    #[clap(subcommand)]
    pub action: Command,
}
