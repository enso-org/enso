//! Building and packaging [`spirv_cross`](ide_ci::programs::spirv_cross::SpirvCross).

use crate::prelude::*;

use ide_ci::programs::cmake::SetVariable;
use ide_ci::programs::vs::apply_dev_environment;
use ide_ci::programs::vs::Cl;
use ide_ci::programs::CMake;
use ide_ci::programs::Git;

/// Address of the SPIRV-Cross GitHub repository.
pub const REPOSITORY_URL: &str = "https://github.com/KhronosGroup/SPIRV-Cross";

pub async fn compile_spirv_cross(output_dir: &Path) -> Result {
    if TARGET_OS == OS::Windows && Cl.lookup().is_err() {
        apply_dev_environment().await?;
    }

    let path = tempfile::tempdir()?;
    let path = path.as_ref();
    let build_dir = path.join("_build");
    let install_dir = path.join("_install");
    ide_ci::fs::tokio::reset_dir(&path).await?;

    let _git = Git.clone(&path, &(REPOSITORY_URL.try_into()?)).await?;

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

    let relative_binary_path =
        PathBuf::from_iter(["bin", "spirv-cross"]).with_executable_extension();
    ide_ci::fs::tokio::copy_between(&install_dir, &output_dir, relative_binary_path).await?;
    Ok(())
}
