use crate::prelude::*;

use crate::source_args_hlp;
use crate::BuildJob;

use clap::Args;
use clap::Subcommand;
use enso_build::project::runtime::Runtime;



source_args_hlp!(Runtime, "runtime", BuildInput);

#[derive(Args, Clone, Copy, Debug, PartialEq, Eq)]
pub struct BuildInput {}

#[derive(Subcommand, Clone, Debug, PartialEq)]
pub enum Command {
    /// Build the WASM package.
    Build(BuildJob<Runtime>),
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    /// Command for Engine Runtime component.
    #[clap(subcommand, name = "command")]
    pub command: Command,
}
