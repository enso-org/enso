use crate::prelude::*;

use crate::source_args_hlp;

use clap::Args;
use clap::Subcommand;
use enso_build::project::gui2::Gui2;



source_args_hlp!(Target, "ide2", BuildInput);

pub type BuildInput = crate::arg::ide::BuildInput<Gui2>;

#[derive(Subcommand, Clone, Debug)]
pub enum Command {
    /// Builds both Project Manager and GUI, puts them together into a single, client Electron
    /// application.
    Build {
        #[clap(flatten)]
        params: BuildInput,
    },
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    #[clap(subcommand)]
    pub command: Command,
}
