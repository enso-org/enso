use crate::prelude::*;

use clap::Args;
use clap::Subcommand;



#[derive(Args, Clone, Debug)]
pub struct DeployToEcr {
    #[clap(long, default_value = enso_build::aws::ecr::runtime::NAME, enso_env())]
    pub ecr_repository: String,
}

#[derive(Subcommand, Clone, Debug)]
pub enum Action {
    CreateDraft,
    /// Build the runtime image and push it to ECR.
    DeployToEcr(DeployToEcr),
    Publish,
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    #[clap(subcommand)]
    pub action: Action,
}
