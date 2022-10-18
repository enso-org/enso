use crate::prelude::*;
use ide_ci::github;

use ide_ci::github::RepoRef;


pub const CLOUD_REPO: RepoRef = RepoRef { owner: "enso-org", name: "cloud-v2" };

pub const BUILD_IMAGE_WORKFLOW: &str = "build-image.yaml";

/// Build Image workflow input. Follows schema defined by
/// https://github.com/enso-org/cloud-v2/blob/main/.github/workflows/build-image.yaml#L4
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct BuildImageInput<T> {
    runtime_version: T,
}

impl<T> BuildImageInput<T> {
    pub fn new(runtime_version: T) -> Self {
        Self { runtime_version }
    }
}

/// This function tells the cloud to build Enso Backend image, based on the image that is in ECR.
///
/// In general, we want this function to be invoked after each ECR push.
#[instrument(fields(%version), skip(octocrab))]
pub async fn build_image_workflow_dispatch_input(octocrab: &Octocrab, version: &Version) -> Result {
    // We want to call our workflow on the default branch.
    let default_branch = CLOUD_REPO.repos(octocrab).get().await?.default_branch.with_context(|| {
        format!(
            "Failed to get the default branch of the {} repository. Missing field: `default_branch`.",
            CLOUD_REPO
        )
    })?;

    debug!("Will invoke on ref: '{}'", default_branch);
    let input = BuildImageInput::new(version);
    info!("Dispatching the cloud workflow to build the image.");
    github::workflow::dispatch(octocrab, &CLOUD_REPO, BUILD_IMAGE_WORKFLOW, default_branch, &input)
        .await
}
