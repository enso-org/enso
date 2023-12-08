use enso_build::prelude::*;

use crate::arg::BuildJob;
use crate::arg::Source;
use crate::source_args_hlp;

use clap::Args;
use clap::Subcommand;
use enso_build::project::gui2::Gui2;



source_args_hlp!(Gui2, "gui2", BuildInput);

#[derive(Args, Clone, Copy, Debug, PartialEq)]
pub struct BuildInput {}

#[derive(Subcommand, Clone, Debug, PartialEq)]
pub enum Command {
    /// Builds the GUI from the local sources.
    Build(BuildJob<Gui2>),
    /// Gets the GUI, either by compiling it from scratch or downloading from an external source.
    Get(Source<Gui2>),
    /// Runs the GUI's unit tests.
    Test,
    /// Run linter on the GUI's sources.
    Lint,
    /// Continuously rebuilds GUI when its sources are changed and serves it using dev-server.
    Watch,
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    /// Command for GUI package.
    #[clap(subcommand)]
    pub command: Command,
}
