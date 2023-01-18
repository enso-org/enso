use ide_ci::define_env_var;
use ide_ci::github::release;
use ide_ci::github::repo;
use ide_ci::github::setup_octocrab;
use ide_ci::github::RepoRef;
pub use ide_ci::prelude;

use prelude::*;

pub const SPIRV_TOOLS_URL: &str = "https://github.com/KhronosGroup/SPIRV-Cross";

pub mod cmake;



define_env_var! {
    /// ID of the release that we are deploying built assets to.
    ENSO_RELEASE_ID, octocrab::models::ReleaseId;
}

pub async fn repo_handle_from_env() -> Result<repo::Handle<RepoRef<'static>>> {
    let octo = setup_octocrab().await?;
    let repo = RepoRef::new("enso-org", "enso");
    let handle = repo.into_handle(&octo);
    Ok(handle)
}

pub async fn release_handle_from_env() -> Result<impl IsRelease> {
    let repo::Handle { octocrab, repo } = repo_handle_from_env().await?;
    let handle = release::Handle::new(&octocrab, repo, ENSO_RELEASE_ID.get()?);
    Ok(handle)
}

pub async fn create_release(
    handle: repo::Handle<impl IsRepo>,
    tag: impl AsRef<str>,
) -> Result<octocrab::models::repos::Release> {
    let release = handle.repos().releases().create(&tag).send().await?;
    ide_ci::actions::workflow::set_output(ENSO_RELEASE_ID.name, &release.id).await?;
    Ok(release)
}


pub mod shaderc;


use crate::cmake::CMake;
use crate::cmake::SetVariable;
use ide_ci::programs::vs::apply_dev_environment;
use ide_ci::programs::Git;

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn compile_spirv_cross() -> Result {
        setup_logging()?;
        if TARGET_OS == OS::Windows {
            apply_dev_environment().await?;
        }
        // let path = Path::new(r"C:\temp\spirv-cross");
        let path = tempfile::tempdir()?;
        let path = path.as_ref();
        let build_dir = path.join("_build");
        let install_dir = path.join("_install");
        ide_ci::fs::tokio::reset_dir(&path).await?;

        let git = Git.clone(&path, &(SPIRV_TOOLS_URL.try_into()?)).await?;

        ide_ci::fs::tokio::reset_dir(&build_dir).await?;
        CMake
            .cmd()?
            .arg(&path)
            .apply(&SetVariable::option("SPIRV_CROSS_ENABLE_TESTS", false))
            .current_dir(&build_dir)
            .run_ok()
            .await?;
        CMake
            .cmd()?
            .arg("--build")
            .arg(".")
            .arg("-j")
            .args(["--config", "Release"])
            .current_dir(&build_dir)
            .run_ok()
            .await?;

        ide_ci::fs::tokio::reset_dir(&install_dir).await?;
        CMake
            .cmd()?
            .arg("--install")
            .arg(".")
            .args(["--prefix", install_dir.as_str()])
            .current_dir(&build_dir)
            .run_ok()
            .await?;

        let archive_name = format!("spirv-cross-{}.tar.gz", TARGET_OS);
        let package = ide_ci::archive::create(&archive_name, [install_dir.join("bin")]).await?;


        Ok(())
    }
}
