//! Code for dealing with the [Enso Cloud repository](https://github.com/enso-org/cloud-v2).

use crate::prelude::*;

use ide_ci::github::RepoRef;



/// The cloud repository.
pub const CLOUD_REPO: RepoRef = RepoRef { owner: "enso-org", name: "cloud-v2" };

/// The workflow we need to invoke to build the backend image.
pub const BUILD_IMAGE_WORKFLOW: &str = "build-image.yaml";

/// Build Image workflow input. Follows schema defined by
/// https://github.com/enso-org/cloud-v2/blob/main/.github/workflows/build-image.yaml#L4
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct BuildImageInput<T> {
    pub runtime_version: T,
}

impl<T> BuildImageInput<T> {
    /// Create a new model of "build-image" workflow input.
    pub fn new(runtime_version: T) -> Self {
        Self { runtime_version }
    }
}

/// This function tells the cloud to build Enso Backend image, based on the image that is in ECR.
///
/// In general, we want this function to be invoked after each ECR push.
#[instrument(fields(%version), skip(octocrab))]
pub async fn build_image_workflow_dispatch_input(octocrab: &Octocrab, version: &Version) -> Result {
    let repo = CLOUD_REPO.handle(octocrab);
    repo.dispatch_workflow(BUILD_IMAGE_WORKFLOW, &BuildImageInput::new(version.to_string())).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use ide_ci::github::setup_octocrab;

    #[tokio::test]
    #[ignore]
    async fn manual_call() -> Result {
        setup_logging()?;
        let octo = setup_octocrab().await?;
        build_image_workflow_dispatch_input(&octo, &Version::parse("2022.1.1-nightly.2022-10-18")?)
            .await?;
        Ok(())
    }
}
