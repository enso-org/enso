use prelude::*;

use ide_ci::define_env_var;
use ide_ci::github::release;
use ide_ci::github::repo;
use ide_ci::github::setup_octocrab;
use ide_ci::github::RepoRef;

pub use ide_ci::prelude;

pub mod cmake;
pub mod shaderc;
pub mod spirv_cross;

pub const SHADER_TOOLS_REPO: RepoRef = RepoRef { owner: "enso-org", name: "shader-tools" };

define_env_var! {
    /// ID of the release that we are deploying built assets to.
    ENSO_RELEASE_ID, octocrab::models::ReleaseId;
}

#[tokio::test]
#[ignore]
async fn test_run() -> Result {
    let tag = "0.1.0";

    setup_logging()?;
    let octo = setup_octocrab().await?;
    let handle = SHADER_TOOLS_REPO.handle(&octo);
    let release = create_release(handle, tag).await?;

    let package = tempfile::tempdir()?;
    shaderc::strip_shaderc_package(package.path()).await?;
    spirv_cross::compile_spirv_cross(package.path()).await?;

    let release_handle = release::Handle::new(&octo, SHADER_TOOLS_REPO, release.id);
    release_handle.upload_compressed_dir(package.path()).await?;


    Ok(())
}


pub async fn repo_handle_from_env() -> Result<repo::Handle<RepoRef<'static>>> {
    let octo = setup_octocrab().await?;
    let repo = RepoRef::new("enso-org", "enso");
    let handle = repo.into_handle(&octo);
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
