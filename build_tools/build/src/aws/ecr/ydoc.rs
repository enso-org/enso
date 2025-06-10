//! This module contains data necessary to deploy Ydoc to the cloud.

use crate::prelude::*;

use crate::paths::generated;

use ide_ci::programs::docker::BuildOptions;
use ide_ci::programs::docker::ImageId;
use ide_ci::programs::Docker;



/// Name of the ECR repository with Ydoc images.
pub const NAME: &str = "ydoc";

/// Region where the ECR repository with Runtime images is located.
pub const REGION: &str = "eu-west-1";

/// Build the polyglot Ydoc Docker image.
#[instrument(fields(%docker_context, %ydoc_native_image))]
pub async fn build_ydoc_polyglot_image(
    docker_context: &generated::RepoRootToolsCiDockerYdocServerPolyglot,
    ydoc_native_image: &generated::RepoRootLibJavaYdocServerTargetNativeImage,
    tag: String,
) -> Result<ImageId> {
    let mut opts = BuildOptions::new(docker_context);
    opts.file = Some(docker_context.dockerfile.to_path_buf());
    opts.tags.push(tag);
    opts.add_build_context_local("native-image", ydoc_native_image);
    let id = Docker.build(opts).await?;
    Ok(id)
}

/// Build the Node.js Ydoc Docker image.
#[instrument(fields(%docker_context, %app_ydoc_server_nodejs))]
pub async fn build_ydoc_nodejs_image(
    docker_context: &generated::RepoRootToolsCiDockerYdocServerNodejs,
    app_ydoc_server_nodejs: &generated::RepoRootAppYdocServerNodejs,
    tag: String,
) -> Result<ImageId> {
    let mut opts = BuildOptions::new(app_ydoc_server_nodejs);
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

    /// Convenience test that builds the polyglot Ydoc image.
    ///
    /// The Ydoc native image must be already built.
    #[tokio::test]
    #[ignore]
    async fn test_ydoc_polyglot() -> Result {
        setup_logging().ok();
        let tag = "ydoc-server-polyglot:0.0.0-dev";
        info!("Current directory: {}", ide_ci::env::current_dir()?.display());
        let root = deduce_repository_path()?;
        let root = root.absolutize()?;
        info!("Repository root: {}", root.display());
        let dockerfile = generated::RepoRootToolsCiDockerYdocServerPolyglot::new_root(
            root.join("tools/ci/docker/ydoc-server-polyglot"),
        );
        let ydoc_native_image = generated::RepoRootLibJavaYdocServerTargetNativeImage::new_root(
            root.join("lib/java/ydoc-server/target/native-image"),
        );
        let id =
            build_ydoc_polyglot_image(&dockerfile, &ydoc_native_image, tag.to_string()).await?;
        info!("Built image: {}", id);
        Ok(())
    }

    /// Convenience test that builds the Node.js Ydoc image.
    ///
    /// The `app/ydoc-server-nodejs` distribution must be already built.
    #[tokio::test]
    #[ignore]
    async fn test_ydoc_nodejs() -> Result {
        setup_logging().ok();
        let tag = "ydoc-server-nodejs:0.0.0-dev";
        info!("Current directory: {}", ide_ci::env::current_dir()?.display());
        let root = deduce_repository_path()?;
        let root = root.absolutize()?;
        info!("Repository root: {}", root.display());
        let docker_context = generated::RepoRootToolsCiDockerYdocServerNodejs::new_root(
            root.join("tools/ci/docker/ydoc-server-nodejs"),
        );
        let app_ydoc_server_nodejs =
            generated::RepoRootAppYdocServerNodejs::new_root(root.join("app/ydoc-server-nodejs"));
        let id = build_ydoc_nodejs_image(&docker_context, &app_ydoc_server_nodejs, tag.to_string())
            .await?;
        info!("Built image: {}", id);
        Ok(())
    }
}
