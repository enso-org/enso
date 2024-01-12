use ide_ci::prelude::*;



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
    config: &Config,
    install_directory: &Path,
    uninstaller_path: &Path,
) -> Result {
    let mut uninstall_info = enso_install::win::uninstall::UninstallInfo::new(
        &config.pretty_name,
        uninstaller_path.as_str(),
    );
    uninstall_info.install_location = Some(install_directory.display().to_string());
    uninstall_info.install_date = Some(chrono::Local::now().to_string());
    uninstall_info.publisher = Some(config.publisher.clone());
    uninstall_info.display_icon = Some(uninstaller_path.display().to_string());
    uninstall_info.display_version = Some(config.version.to_string());
    uninstall_info.write_to_registry(&config.uninstall_key)?;
    Ok(())
}

/// Install Enso.
///
/// The archive payload is binary data of the tar.gz archive that contains the Enso app.
pub fn install(
    install_location: impl AsRef<Path>,
    archive_payload: &[u8],
    config: &Config,
) -> Result {
    let install_location = install_location.as_ref();
    info!("Removing old installation files (if present).");
    ide_ci::fs::reset_dir(&install_location)?;

    let to_our_path = |path_in_archive: &Path| -> Option<PathBuf> {
        Some(install_location.join(path_in_archive))
    };
    let executable_location = install_location.join(&config.executable_filename);

    // Extract the files.
    let decoder = flate2::read::GzDecoder::new(archive_payload);
    let archive = tar::Archive::new(decoder);
    info!("Extracting files.");
    ide_ci::archive::tar::extract_files_sync(archive, to_our_path)?;

    info!("Registering file types.");
    register_file_association(&executable_location)?;

    for protocol in &config.url_protocols {
        info!("Registering URL protocol '{protocol}'.");
        register_url_protocol(&executable_location, protocol)?;
    }

    info!("Registering the application path.");
    let app_paths_info = enso_install::win::app_paths::AppPathInfo::new(&executable_location);
    app_paths_info.write_to_registry()?;

    info!("Registering the uninstaller.");
    register_uninstaller(
        &config,
        install_location,
        &install_location.join("enso-uninstaller.exe"),
    )?;

    info!("Creating Start Menu entry.");
    enso_install::win::shortcut::Location::Menu
        .create_shortcut(&config.shortcut_name, &executable_location)?;

    info!("Creating Desktop shortcut.");
    enso_install::win::shortcut::Location::Desktop
        .create_shortcut(&config.shortcut_name, &executable_location)?;


    info!("Installation complete.");
    Ok(())
}

/// All the configuration and constants needed to build the installer.
pub struct Config {
    /// E.g. `Enso.exe`.
    pub executable_filename: PathBuf,

    /// E.g. `New Byte Order sp. z o.o.`.
    pub publisher: String,

    /// E.g. `Enso`.
    pub pretty_name: String,

    /// E.g. `Enso`.
    ///
    /// Used for entries in the Start Menu and Desktop.
    pub shortcut_name: String,

    /// The name of the registry key where uninstall information is stored, e.g. `Enso`.
    ///
    /// The key is located under
    /// `HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Uninstall`.
    pub uninstall_key: String,

    /// Version of the application.
    pub version: Version,

    /// The URL protocols that will be registered for the application, e.g. `enso`.
    pub url_protocols: Vec<String>,

    /// File associations.
    pub file_associations:
        Vec<(enso_install::win::prog_id::FileType, enso_install::win::prog_id::FileExtension)>,
}


pub fn fill_config() -> Result<Config> {
    let electron = enso_install_config::sanitized_electron_builder_config()?;

    let executable_filename = electron.product_name.with_executable_extension();
    let publisher = enso_install::config::PUBLISHER_NAME.to_string();
    let pretty_name = electron.product_name.clone();
    let shortcut_name = pretty_name.clone();
    let uninstall_key = pretty_name.clone();
    let version = electron.extra_metadata.version.clone();
    let url_protocols = electron.protocols.iter().flat_map(|p| &p.schemes).map_into().collect();
    let file_associations = default();
    Ok(Config {
        executable_filename,
        publisher,
        pretty_name,
        shortcut_name,
        uninstall_key,
        version,
        url_protocols,
        file_associations,
    })
}
