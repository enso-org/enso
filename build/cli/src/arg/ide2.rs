use crate::prelude::*;

use crate::source_args_hlp;

use clap::Args;
use clap::Subcommand;
use enso_build::project::gui2::Gui2;
use octocrab::models::ReleaseId;



source_args_hlp!(Target, "ide2", BuildInput);

pub type BuildInput = crate::arg::ide::BuildInput<Gui2, Target>;

#[derive(Subcommand, Clone, Debug)]
pub enum Command {
    /// Builds both Project Manager and GUI, puts them together into a single, client Electron
    /// application.
    Build {
        #[clap(flatten)]
        params: BuildInput,
    },
    /// Build and upload the new IDE as a release asset.
    /// This command is intended for CI-use only.
    Upload {
        #[clap(flatten)]
        params:     BuildInput,
        #[clap(long, env = *enso_build::env::ENSO_RELEASE_ID)]
        release_id: ReleaseId,
    },
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    #[clap(subcommand)]
    pub command: Command,
}
