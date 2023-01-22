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

pub const SHADER_TOOLS_REPO: RepoRef = RepoRef { owner: "enso-org", name: "shader-tools" };

define_env_var! {
    /// ID of the release that we are deploying built assets to.
    ENSO_RELEASE_ID, octocrab::models::ReleaseId;
}

pub async fn create_package(output_archive: &Path) -> Result {
    let package = tempfile::tempdir()?;
    shaderc::strip_shaderc_package(package.path()).await?;
    spirv_cross::compile_spirv_cross(package.path()).await?;
    ide_ci::archive::compress_directory(&output_archive, package.path()).await?;
    Ok(())
}

pub async fn repo_handle_from_env() -> Result<repo::Handle<RepoRef<'static>>> {
    let octo = setup_octocrab().await?;
    // let repo = RepoRef::new("enso-org", "enso");
    let handle = SHADER_TOOLS_REPO.into_handle(&octo);
    Ok(handle)
}

pub async fn release_handle_from_env() -> Result<release::Handle> {
    let repo::Handle { octocrab, repo } = repo_handle_from_env().await?;
    let handle = release::Handle::new(&octocrab, repo, ENSO_RELEASE_ID.get()?);
    Ok(handle)
}

pub async fn create_release(
    handle: repo::Handle<impl IsRepo>,
    tag: impl AsRef<str>,
) -> Result<octocrab::models::repos::Release> {
    let release = handle.repos().releases().create(&tag).draft(true).send().await?;
    ide_ci::actions::workflow::set_output(ENSO_RELEASE_ID.name, &release.id).await?;
    Ok(release)
}
