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

#[derive(Subcommand, Clone, Debug)]
pub enum Action {
    CreateDraft,
    /// Build the runtime image and push it to ECR.
    DeployRuntime(DeployRuntime),
    /// Upload the GUI to the S3 Bucket and notify.
    DeployGui(DeployGui),
    Publish,
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    #[clap(subcommand)]
    pub action: Action,
}
