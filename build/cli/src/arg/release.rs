use crate::prelude::*;

use clap::Args;
use clap::Subcommand;



#[derive(Args, Clone, Debug)]
pub struct DeployRuntime {
    #[clap(long, default_value = enso_build::aws::ecr::runtime::NAME, enso_env())]
    pub ecr_repository: String,
}

#[derive(Args, Clone, Copy, Debug)]
pub struct DeployGui {}

/// Structure that represents `promote` subcommand arguments.
#[derive(Args, Clone, Debug)]
pub struct Promote {
    /// Version that the commit will be published as.
    #[clap()]
    pub version: String,
}

#[derive(Subcommand, Clone, Debug)]
pub enum Action {
    /// Create a release draft on GitHub.
    CreateDraft,
    /// Build the runtime image and push it to ECR.
    DeployRuntime(DeployRuntime),
    /// Upload the GUI to the S3 Bucket and notify.
    DeployGui(DeployGui),
    Publish,
    Promote(Promote),
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    #[clap(subcommand)]
    pub action: Action,
}
