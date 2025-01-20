//! Information defining the installer's behavior.

use crate::prelude::*;



/// All the configuration and constants needed to build the installer.
#[derive(Clone, Debug)]
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

/// Generate the `Config` for the installer from the compiled-in Electron Builder configuration.
pub fn fill_config() -> Result<Config> {
    let electron = enso_install::sanitized_electron_builder_config();

    let executable_filename = electron.product_name.with_executable_extension();
    let publisher = electron.extra_metadata.installer.publisher.clone();
    let pretty_name = electron.product_name.clone();
    let shortcut_name = pretty_name.clone();
    let uninstall_key = pretty_name.clone();
    let version = electron.extra_metadata.version.clone();
    let url_protocols = electron.protocols.iter().flat_map(|p| &p.schemes).map_into().collect();
    let file_associations = electron
        .extra_metadata
        .installer
        .file_associations
        .iter()
        .map(|file_association| {
            let prog_id = file_association.prog_id.clone();
            let extension = file_association.ext.clone();
            let mime_type = file_association.mime_type.clone();
            let perceived_type =
                enso_install::win::prog_id::PerceivedType::from_mime_type(&mime_type)?;
            let file_type = enso_install::win::prog_id::FileType {
                application_path: executable_filename.clone(),
                prog_id:          prog_id.clone(),
                friendly_name:    file_association.name.clone(),
                info_tip:         file_association.name.clone(),
            };
            let file_extension = enso_install::win::prog_id::FileExtension {
                extension,
                prog_id,
                mime_type,
                perceived_type,
            };
            Result::Ok((file_type, file_extension))
        })
        .try_collect()?;
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
