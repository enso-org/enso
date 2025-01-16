use crate::prelude::*;

use crate::arg::OutputPath;
use crate::arg::Source;
use crate::source_args_hlp;

use clap::Args;
use clap::Subcommand;
use enso_build::project::backend::Backend;
use enso_build::project::gui::Gui;
use octocrab::models::ReleaseId;



source_args_hlp!(Target, "ide", BuildInput);

#[derive(Args, Clone, Debug, PartialEq)]
#[group(skip)]
pub struct BuildInput {
    #[clap(flatten)]
    pub gui:             Source<Gui>,
    #[clap(flatten)]
    pub project_manager: Source<Backend>,
    #[clap(flatten)]
    pub output_path:     OutputPath<Target>,
    /// Override the default target for electron-builder. E.g. pass `dir` for unpacked directory
    /// (fastest). See <https://www.electron.build> for all supported targets.
    #[clap(long, enso_env())]
    pub electron_target: Option<String>,
    #[clap(long, enso_env())]
    pub sign_artifacts:  bool,
}

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
