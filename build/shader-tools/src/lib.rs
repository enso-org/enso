#![feature(default_free_fn)]

use prelude::*;

use ide_ci::define_env_var;
use ide_ci::github::release;
use ide_ci::github::repo;
use ide_ci::github::setup_octocrab;
use ide_ci::github::RepoRef;

pub use ide_ci::prelude;

pub mod ci;
pub mod shaderc;
pub mod spirv_cross;

/// Repository where we store releases of the shader tools.
pub const SHADER_TOOLS_REPO: RepoRef = RepoRef { owner: "enso-org", name: "shader-tools" };

define_env_var! {
    /// ID of the release that we are deploying built assets to.
    ENSO_RELEASE_ID, octocrab::models::ReleaseId;
}

/// Create a package with shader tools for the current platform.
///
/// The package can be later uploaded as a release asset.
#[context("Failed to create a package with shader tools.")]
pub async fn create_package(output_archive: &Path) -> Result {
    let package = tempfile::tempdir()?;
    shaderc::generate_stripped_package(package.path()).await?;
    spirv_cross::compile_spirv_cross(package.path()).await?;
    ide_ci::archive::compress_directory(&output_archive, package.path()).await?;
    Ok(())
}

/// Get the handle to shader tools repository using system environment.
///
/// Note that currently the repository is hardcoded to `enso-org/shader-tools`.
#[context("Failed to get the handle to shader tools repository.")]
pub async fn repo_handle_from_env() -> Result<repo::Handle<RepoRef<'static>>> {
    let octo = setup_octocrab().await?;
    let handle = SHADER_TOOLS_REPO.into_handle(&octo);
    Ok(handle)
}

/// Get the handle to the release that we are deploying assets to.
///
/// Requires some environment setup, done by [`create_release`].
#[context("Failed to get the handle to the release that we are deploying assets to.")]
pub async fn release_handle_from_env() -> Result<release::Handle> {
    let repo::Handle { octocrab, repo } = repo_handle_from_env().await?;
    let handle = release::Handle::new(&octocrab, repo, ENSO_RELEASE_ID.get()?);
    Ok(handle)
}

/// Create a release for the current version of the shader tools.
#[context("Failed to create a release for the current version of the shader tools.")]
pub async fn create_release(
    handle: repo::Handle<impl IsRepo>,
    tag: impl AsRef<str>,
) -> Result<octocrab::models::repos::Release> {
    let release = handle.repos().releases().create(&tag).draft(true).send().await?;
    ide_ci::actions::workflow::set_output(ENSO_RELEASE_ID.name, &release.id).await?;
    Ok(release)
}
