use crate::prelude::*;

use ide_ci::github::RepoRef;

pub const CLOUD_REPO: RepoRef = RepoRef { owner: "enso-org", name: "cloud-v2" };

pub const CLOUD_DEFAULT_BRANCH: &str = "main";

/// This function tells the cloud to build Enso Backend image, based on the image that is in ECR.
///
/// In general, we want this function to be invoked after each ECR push.
#[instrument(fields(%version), skip(octocrab))]
pub async fn build_image_workflow_dispatch_input(octocrab: &Octocrab, version: &Version) -> Result {
    // Follows schema defined by
    // https://github.com/enso-org/cloud-v2/blob/main/.github/workflows/build-image.yaml#L4
    let input = serde_json::json!({
        "runtime_version": version.to_string(),
    });
    info!("Dispatching the cloud workflow to build the image.");
    CLOUD_REPO.dispatch_workflow(octocrab, "build-image.yaml", CLOUD_DEFAULT_BRANCH, &input).await
}
