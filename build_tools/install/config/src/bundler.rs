//! Utilities for building the Windows installer/uninstaller for the Enso application.
//!
//! See the [`bundle`] function as the main entry point.

use crate::prelude::*;

use crate::INSTALLER_NAME;
use crate::UNINSTALLER_NAME;

use ide_ci::env::known::electron_builder::WindowsSigningCredentials;
use ide_ci::programs::cargo;
use ide_ci::programs::Cargo;



/// Input necessary to generate a Windows installer from unpacked Electron application bundle.
#[derive(Debug)]
pub struct Config {
    /// File to the JSON file containing the Electron Builder configuration.
    pub electron_builder_config:  PathBuf,
    /// Path to the directory containing the unpacked Electron application bundle.
    ///
    /// It is obtained by running the `electron-builder` with the `--dir` option.
    pub unpacked_electron_bundle: PathBuf,
    /// Path to the root of the repository.
    pub repo_root:                PathBuf,
    /// Path where the generated installer should be saved.
    pub output_file:              PathBuf,
    /// Path to the directory where intermediate files should be stored.
    pub intermediate_dir:         PathBuf,
    /// Certificate used to sign the installer and uninstaller.
    ///
    /// If `None`, the installer and uninstaller will not be signed.
    pub certificate:              Option<WindowsSigningCredentials>,
}

/// Builds a package using Cargo and optionally signs it with a certificate.
pub async fn build_package(
    crate_name: &str,
    output_file: &Path,
    certificate: Option<&WindowsSigningCredentials>,
    prepare_env: impl FnOnce(&mut Command) -> Result<&mut Command>,
) -> Result {
    let temp_dir = tempfile::tempdir()?;
    let mut cmd = Cargo.cmd()?;
    prepare_env(&mut cmd)?;
    cmd.apply(&cargo::Command::Build)
        .arg("--release")
        .arg("--package")
        .arg(crate_name)
        .arg("-Z")
        .arg("unstable-options")
        .arg("--out-dir")
        .arg(temp_dir.path())
        .run_ok()
        .await?;

    let built_exe = temp_dir.path().join(crate_name).with_executable_extension();
    if let Some(certificate) = certificate {
        certificate.sign(&built_exe).await?;
    }
    ide_ci::fs::tokio::copy(&built_exe, output_file).await?;
    Ok(())
}

/// Package the Enso unpacked Electron application bundle (electron-builder's output) into an
/// installer.
///
/// First, the uninstaller is built and signed. Then, the payload is prepared and the installer is
/// built and signed.
pub async fn bundle(config: Config) -> Result {
    let Config {
        electron_builder_config,
        unpacked_electron_bundle,
        repo_root,
        output_file,
        intermediate_dir,
        certificate,
    } = config;

    // The uninstaller must be built first, as it is part of the distribution package.
    let uninstaller_path =
        unpacked_electron_bundle.join(UNINSTALLER_NAME).with_executable_extension();
    build_package(UNINSTALLER_NAME, &uninstaller_path, certificate.as_ref(), |cmd| {
        cmd.current_dir(&repo_root);
        cmd.set_env(crate::ENSO_BUILD_ELECTRON_BUILDER_CONFIG, &electron_builder_config)
    })
    .await?;

    // Prepare the archive payload for the installer.
    let archive_path = intermediate_dir.join("payload.tar.gz");
    let metadata_path = intermediate_dir.join("payload.json");
    crate::payload::prepare_payload(&unpacked_electron_bundle, &archive_path, &metadata_path)
        .await?;

    // Finally build the installer.
    build_package(INSTALLER_NAME, &output_file, certificate.as_ref(), |cmd| {
        cmd.current_dir(&repo_root)
            .set_env(crate::ENSO_INSTALL_ARCHIVE_PATH, &archive_path)?
            .set_env(crate::ENSO_INSTALL_METADATA_PATH, &metadata_path)?
            .set_env(crate::ENSO_BUILD_ELECTRON_BUILDER_CONFIG, &electron_builder_config)
    })
    .await
}
