use enso_build::prelude::*;

use crate::arg::BuildJob;
use crate::arg::Source;
use crate::source_args_hlp;

use clap::Args;
use clap::Subcommand;
use enso_build::project::gui::Gui;



source_args_hlp!(Gui, "gui", BuildInput);

#[derive(Args, Clone, Copy, Debug, PartialEq)]
#[group(skip)]
pub struct BuildInput {}

#[derive(Subcommand, Clone, Debug, PartialEq)]
pub enum Command {
    /// Builds the GUI from the local sources.
    Build(BuildJob<Gui>),
    /// Gets the GUI, either by compiling it from scratch or downloading from an external source.
    Get(Source<Gui>),
    /// Runs the GUI's linting and unit tests.
    Check,
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    /// Command for GUI package.
    #[clap(subcommand)]
    pub command: Command,
}
