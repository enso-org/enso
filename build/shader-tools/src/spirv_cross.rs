//! Building and packaging [`SPIRV-Cross`](SpirvCross).

use crate::prelude::*;

use ide_ci::programs::cmake;
use ide_ci::programs::cmake::SetVariable;
use ide_ci::programs::spirv_cross::SpirvCross;
use ide_ci::programs::vs::apply_dev_environment;
use ide_ci::programs::vs::Cl;
use ide_ci::programs::Git;



/// Address of the SPIRV-Cross GitHub repository.
pub const REPOSITORY_URL: &str = "https://github.com/KhronosGroup/SPIRV-Cross";

/// Binaries that we want to package.
pub fn binaries_to_package() -> [&'static str; 1] {
    [SpirvCross.executable_name()]
}

/// Build and install the SPIRV-Cross to the given directory.
pub async fn build_and_install(
    source_dir: impl AsRef<Path>,
    install_dir: impl AsRef<Path>,
) -> Result {
    let build_dir = tempfile::tempdir()?;
    cmake::generate(&source_dir, &build_dir)?
        // We only want the tool binary, we don't want to bother with building tests.
        .apply(&SetVariable::option("SPIRV_CROSS_ENABLE_TESTS", false))
        // Note [Configuration]
        .apply(&cmake::build_type(cmake::Configuration::Release))
        .run_ok()
        .await?;

    cmake::build(&build_dir)?
        // Note [Configuration]
        .apply(&cmake::BuildOption::Configuration(cmake::Configuration::Release))
        .run_ok()
        .await?;

    // Note [Configuration]
    // ~~~~~~~~~~~~~~~~~~~~
    // We pass the Release configuration twice, but it will be used only once. It will be used in
    // the generation phase, if the generator is a single-config one. Otherwise, it will be used in
    // the build phase. We do not want to assume any particular kind of generator, so we support
    // both cases.

    ide_ci::fs::tokio::reset_dir(&install_dir).await?;
    cmake::install(&build_dir, &install_dir)?.run_ok().await?;
    Ok(())
}

/// Download sources of the SPIRV-Cross, build them and package the binary we need.
pub async fn generate_spirv_cross_package(output_dir: &Path) -> Result {
    if TARGET_OS == OS::Windows && Cl.lookup().is_err() {
        apply_dev_environment().await?;
    }

    let path = tempfile::tempdir()?;
    let path = path.as_ref();
    let install_dir = path.join("_install");
    ide_ci::fs::tokio::reset_dir(&path).await?;
    let _git = Git.clone(&path, &(REPOSITORY_URL.try_into()?)).await?;

    build_and_install(&path, &install_dir).await?;
    for binary in binaries_to_package() {
        let relative_binary_path = PathBuf::from_iter(["bin", binary]).with_executable_extension();
        ide_ci::fs::tokio::copy_between(&install_dir, &output_dir, relative_binary_path).await?;
    }
    Ok(())
}
