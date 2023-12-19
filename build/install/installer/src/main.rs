// === Features ===
#![feature(core_intrinsics)]
#![feature(default_free_fn)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_build::prelude::*;

use enso_install::config::APPLICATION_EXECUTABLE;
use enso_install::config::APPLICATION_SHORTCUT_NAME;
use enso_install::config::APPLICATION_UNINSTALL_KEY;



pub fn register_file_association(executable_path: impl AsRef<Path>) -> Result {
    let enso_file_type = enso_install::win::prog_id::FileType {
        application_path: executable_path.as_ref().to_path_buf(),
        prog_id:          enso_install::config::SOURCE_FILE_PROG_ID.to_string(),
        friendly_name:    "Enso Source File".to_string(),
        info_tip:         "Enso Source File".to_string(),
    };
    let enso_extension = enso_install::win::prog_id::FileExtension {
        extension:      ".enso".to_string(),
        prog_id:        enso_install::config::SOURCE_FILE_PROG_ID.to_string(),
        mime_type:      "text/plain".to_string(),
        perceived_type: enso_install::win::prog_id::PerceivedType::Text,
    };
    enso_extension.register()?;
    enso_file_type.register()?;

    let enso_bundle_file_type = enso_install::win::prog_id::FileType {
        application_path: executable_path.as_ref().to_path_buf(),
        prog_id:          enso_install::config::PROJECT_BUNDLE_PROG_ID.to_string(),
        friendly_name:    "Enso Project Bundle".to_string(),
        info_tip:         "Enso Project Bundle".to_string(),
    };
    let enso_bundle_extension = enso_install::win::prog_id::FileExtension {
        extension:      ".enso-project".to_string(),
        prog_id:        enso_install::config::PROJECT_BUNDLE_PROG_ID.to_string(),
        mime_type:      "application/gzip".to_string(), // it's renamed tar.gz
        perceived_type: enso_install::win::prog_id::PerceivedType::Text,
    };
    enso_bundle_extension.register()?;
    enso_bundle_file_type.register()?;

    info!("Refreshing file associations in the shell.");
    enso_install::win::refresh_file_associations();

    Ok(())
}

/// Set application as the default handler for the given URL protocol, e.g. `enso://`.
///
/// This is necessary for the deep linking to work.
pub fn register_url_protocol(executable_path: &Path, protocol: &str) -> Result {
    let info = enso_install::win::prog_id::ProtocolInfo::new(protocol, executable_path);
    info.register()
}

pub fn register_uninstaller(
    app_pretty_name: &str,
    install_directory: &Path,
    uninstaller_path: &Path,
) -> Result {
    let mut uninstall_info = enso_install::win::uninstall::UninstallInfo::new(
        app_pretty_name,
        uninstaller_path.as_str(),
    );
    uninstall_info.install_location = Some(install_directory.display().to_string());
    uninstall_info.install_date = Some(chrono::Local::now().to_string());
    uninstall_info.publisher = Some(enso_install::config::PUBLISHER_NAME.to_string());
    uninstall_info.display_icon = Some(uninstaller_path.display().to_string());
    uninstall_info.display_version = Some(enso_install::config::VERSION.to_string());
    uninstall_info.write_to_registry(APPLICATION_UNINSTALL_KEY)?;
    Ok(())
}

/// Install Enso.
///
/// The archive payload is binary data of the tar.gz archive that contains the Enso app.
pub fn install(install_location: impl AsRef<Path>, archive_payload: &[u8]) -> Result {
    let install_location = install_location.as_ref();
    info!("Removing old installation files (if present).");
    ide_ci::fs::reset_dir(&install_location)?;

    let to_our_path = |path_in_archive: &Path| -> Option<PathBuf> {
        Some(install_location.join(path_in_archive))
    };
    let executable_location = install_location.join(APPLICATION_EXECUTABLE);

    // Extract the files.
    let decoder = flate2::read::GzDecoder::new(archive_payload);
    let archive = tar::Archive::new(decoder);
    info!("Extracting files.");
    ide_ci::archive::tar::extract_files_sync(archive, to_our_path)?;

    info!("Registering file types.");
    register_file_association(&executable_location)?;

    info!("Registering URL protocol.");
    register_url_protocol(&executable_location, "enso")?;

    info!("Registering the application path.");
    let app_paths_info = enso_install::win::app_paths::AppPathInfo::new(&executable_location);
    app_paths_info.write_to_registry()?;

    info!("Registering the uninstaller.");
    register_uninstaller(
        enso_install::config::APPLICATION_PRETTY_NAME,
        install_location,
        &install_location.join("enso-uninstaller.exe"),
    )?;

    info!("Creating Start Menu entry.");
    enso_install::win::shortcut::Location::Menu
        .create_shortcut(APPLICATION_SHORTCUT_NAME, &executable_location)?;

    info!("Creating Desktop shortcut.");
    enso_install::win::shortcut::Location::Desktop
        .create_shortcut(APPLICATION_SHORTCUT_NAME, &executable_location)?;


    info!("Installation complete.");
    Ok(())
}

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let lock = enso_install::lock()?;
    let _guard = lock.lock()?;
    let archive = enso_install::win::resource::get_binary("ARCHIVE_ID")?;
    let install_dir = ide_ci::env::known::win::LOCALAPPDATA.get()?.join("Programs").join("Enso");
    install(install_dir, archive)?;
    Ok(())
}
