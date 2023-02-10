use enso_build::prelude::*;

use crate::arg::BuildJob;
use crate::arg::Source;
use crate::arg::WatchJob;
use crate::source_args_hlp;
use crate::IsWatchableSource;

use clap::Args;
use clap::Subcommand;
use enso_build::project::gui::Gui;
use enso_build::project::wasm::Wasm;



source_args_hlp!(Gui, "gui", BuildInput);

impl IsWatchableSource for Gui {
    type WatchInput = WatchInput;
}

#[derive(Args, Clone, Debug, PartialEq)]
pub struct BuildInput {
    #[clap(flatten)]
    pub wasm: Source<Wasm>,
}

#[derive(Args, Clone, Debug, PartialEq)]
pub struct WatchInput {
    #[clap(flatten)]
    pub wasm: <Wasm as IsWatchableSource>::WatchInput,
}

#[derive(Subcommand, Clone, Debug, PartialEq)]
pub enum Command {
    /// Builds the GUI from the local sources.
    Build(BuildJob<Gui>),
    /// Gets the GUI, either by compiling it from scratch or downloading from an external source.
    Get(Source<Gui>),
    /// Continuously rebuilds GUI when its sources are changed and serves it using dev-server.
    Watch(WatchJob<Gui>),
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    /// Command for GUI package.
    #[clap(subcommand)]
    pub command: Command,
}
