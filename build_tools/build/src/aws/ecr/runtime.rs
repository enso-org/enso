//! This module contains data necessary to deploy Enso Runtime to the cloud.

use crate::prelude::*;

use crate::paths::generated;

use ide_ci::programs::docker::BuildOptions;
use ide_ci::programs::docker::ImageId;
use ide_ci::programs::Docker;



/// Name of the ECR repository with Runtime images.
pub const NAME: &str = "runtime";

/// Region where the ECR repository with Runtime images is located.
pub const REGION: &str = "eu-west-1";

/// Build the Runtime Docker image from the Engine package.
#[instrument(fields(%docker_context, %engine_package_root))]
pub async fn build_runtime_image(
    docker_context: &generated::RepoRootToolsCiDockerEngine,
    engine_package_root: &generated::EnginePackage,
    tag: String,
) -> Result<ImageId> {
    let mut opts = BuildOptions::new(engine_package_root);
    opts.file = Some(docker_context.dockerfile.to_path_buf());
    opts.tags.push(tag);
    opts.add_build_context_local("docker-tools", docker_context);
    let id = Docker.build(opts).await?;
    Ok(id)
}

#[cfg(test)]
mod tests {
    use crate::repo::deduce_repository_path;

    use super::*;

    /// Convenience test that builds the Runtime image.
    ///
    /// The engine must be already built.
    #[tokio::test]
    #[ignore]
    async fn test_runtime() -> Result {
        setup_logging().ok();
        let tag = "runtime:0.0.0-dev";
        info!("Current directory: {}", ide_ci::env::current_dir()?.display());
        let root = deduce_repository_path()?;
        let root = root.absolutize()?;
        info!("Repository root: {}", root.display());
        let engine_package = generated::EnginePackage::new_root(
            root.join("built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev"),
        );
        let dockerfile =
            generated::RepoRootToolsCiDockerEngine::new_root(root.join("tools/ci/docker/engine"));
        let id = build_runtime_image(&dockerfile, &engine_package, tag.to_string()).await?;
        info!("Built image: {}", id);
        Ok(())
    }
}
