#![feature(core_intrinsics)]
#![feature(default_free_fn)]
// #![windows_subsystem = "windows"]

use enso_build::prelude::*;
use windows::core::HSTRING;
use windows::Win32::Foundation::WIN32_ERROR;
// use std::intrinsics::black_box;
use winreg::enums::HKEY_CURRENT_USER;


use enso_install::config::APPLICATION_SHORTCUT_NAME;
use enso_install::config::APPLICATION_UNINSTALL_KEY;
use winreg::enums::*;
use winreg::RegKey;

/// Represents an association between a file extension and an application.
pub struct FileType {
    /// The file extensions, *including* the leading dot.
    pub extensions:       Vec<String>,
    /// The absolute path to the application executable.
    pub application_path: PathBuf,
    /// The `ProgID` of the file type - a unique identifier for the file type.
    pub prog_id:          String,
    /// The friendly name of the file type.
    pub friendly_name:    String,
}

impl FileType {
    #[instrument(err, level = "info", skip(self), fields(extensions = ?self.extensions))]
    pub fn register(&self) -> Result {
        ensure!(
            self.application_path.exists(),
            "Application path does not exist: {}",
            self.application_path.display()
        );
        ensure!(
            self.application_path.is_file(),
            "Application path is not a file: {}",
            self.application_path.display()
        );
        ensure!(!self.extensions.is_empty(), "No extensions specified for file type.");
        ensure!(
            self.extensions.iter().all(|ext| ext.starts_with('.')),
            "Extensions must start with a dot."
        );

        let classes = RegKey::predef(HKEY_CURRENT_USER)
            .open_subkey_with_flags(r"Software\Classes", KEY_READ | KEY_WRITE)
            .with_context(|| {
                format!(r#"Failed to open HKEY_CURRENT_USER\Software\Classes key."#)
            })?;

        // Each file extension needs its own entry in the registry under `HKEY_CLASSES_ROOT/.ext`
        // that points to the Programmatic Identifier (ProgID) of the file type.
        for extension in &self.extensions {
            // https://learn.microsoft.com/en-us/windows/win32/shell/fa-file-types#setting-optional-subkeys-and-file-type-extension-attributes
            // Describe the file extension and its association with the ProgID.
            let (file_ext_key, _disposition) =
                classes.create_subkey(extension).with_context(|| {
                    format!("Failed to create file extension key for: {}", extension)
                })?;
            file_ext_key.set_value("", &self.prog_id)?;
            file_ext_key.set_value("Content Type", &"text/plain")?;
            file_ext_key.set_value("PerceivedType", &"text")?;

            let (open_with_prog_ids, _) =
                file_ext_key.create_subkey("OpenWithProgIds").with_context(|| {
                    format!("Failed to create OpenWithProgIds subkey for: {extension}")
                })?;
            open_with_prog_ids.set_value(&self.prog_id, &"")?;
        }

        // https://learn.microsoft.com/en-us/windows/win32/shell/fa-progids
        // Describe the Programmatic Identifier (ProgID) of the file type.
        let (prog_id_key, _disposition) = classes.create_subkey(&self.prog_id)?;
        prog_id_key.set_value("", &self.friendly_name)?;
        prog_id_key.set_value("FriendlyTypeName", &self.friendly_name)?;
        prog_id_key.set_value("InfoTip", &"Enso source file")?;
        prog_id_key
            .create_subkey("DefaultIcon")?
            .0
            .set_value("", &format!(r#""{},""#, self.application_path.display()))?;
        prog_id_key
            .create_subkey(r"shell\open\command")?
            .0
            .set_value("", &format!(r#""{}" "%1""#, self.application_path.display()))?;


        Ok(())
    }
}

pub fn register_file_association(executable_path: impl AsRef<Path>) -> Result {
    let enso_file_type = FileType {
        extensions:       vec![".enso".to_string()],
        application_path: executable_path.as_ref().to_path_buf(),
        prog_id:          enso_install::config::SOURCE_FILE_PROG_ID.to_string(),
        friendly_name:    "Enso Source File".to_string(),
    };
    enso_file_type.register()?;

    let enso_project_type = FileType {
        extensions:       vec![".enso-project".to_string()],
        application_path: executable_path.as_ref().to_path_buf(),
        prog_id:          enso_install::config::PROJECT_BUNDLE_PROG_ID.to_string(),
        friendly_name:    "Enso Project Bundle".to_string(),
    };
    enso_project_type.register()?;

    info!("Refreshing file associations in the shell.");
    enso_install::win::refresh_file_associations();

    Ok(())
}

pub struct ApplicationInfo {
    /// The absolute path to the application executable.
    pub executable_path: PathBuf,
    // TODO: more
}

pub struct ProtocolInfo {}

/// Set application as the default handler for the given URL protocol, e.g. `enso://`.
///
/// This is necessary for the deep linking to work.
pub fn register_url_protocol(executable_path: &Path, protocol: &str) -> Result {
    // See https://learn.microsoft.com/en-us/windows/win32/shell/app-registration

    // Register the URL protocol.
    let (url_key, _) = RegKey::predef(HKEY_CURRENT_USER)
        .open_subkey_with_flags(r"Software\Classes", KEY_READ | KEY_WRITE)
        .with_context(|| format!(r#"Failed to open `HKEY_CURRENT_USER\Software\Classes` key."#))?
        .create_subkey(&protocol)
        .with_context(|| format!(r#"Failed to create subkey for protocol `{protocol}`"#))?;

    url_key.set_value("", &format!("URL:{protocol}"))?;
    url_key.set_value("URL Protocol", &"")?;

    let (command_key, _) = url_key
        .create_subkey(r"shell\open\command")
        .with_context(|| format!(r#"Failed to create subkey for protocol: {protocol}"#))?;
    command_key.set_value("", &format!(r#""{}" "%1""#, executable_path.display()))?;

    Ok(())
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
    uninstall_info.display_version = Some(enso_install::config::VERSION.to_string());
    uninstall_info.write_to_registry(APPLICATION_UNINSTALL_KEY)?;
    Ok(())
}

pub fn register_application(executable_path: &Path) -> Result {
    // See https://learn.microsoft.com/en-us/windows/win32/shell/app-registration

    let executable_name = executable_path.try_file_name()?.as_str();
    // let executable_dir = executable_path.try_parent()?.as_str();
    let executable_dir = executable_path.try_parent()?;
    let executable_dir = executable_dir.as_str();


    // Use App Paths registry key to register the application.
    let (exe_key, _) = RegKey::predef(HKEY_CURRENT_USER)
        .open_subkey_with_flags(
            r"Software\Microsoft\Windows\CurrentVersion\App Paths",
            KEY_READ | KEY_WRITE,
        )
        .with_context(|| {
            format!(
                r#"Failed to open
`HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\App Paths` key."#
            )
        })?
        .create_subkey(&executable_name)
        .with_context(|| {
            format!(r#"Failed to create subkey for application: {}"#, executable_name)
        })?;

    exe_key.set_value("", &executable_path.as_str())?;
    exe_key.set_value("Path", &executable_dir)?;
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
    let executable_location = install_location.join("Enso.exe");

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
    register_application(&executable_location)?;

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

/// Get binary resource from the executable.
///
/// The resource must be compiled into the current executable as `RCDATA`.
pub fn get_binary_resource(name: &str) -> &'static [u8] {
    unsafe {
        // Clear error, so any `GetLastError` call after this one will return actual error from the
        // subsequent calls.
        windows::Win32::Foundation::SetLastError(WIN32_ERROR::default());
        let resource = windows::Win32::System::LibraryLoader::FindResourceW(
            None,
            &HSTRING::from(name),
            enso_install::win::RT_RCDATA,
        );
        windows::Win32::Foundation::GetLastError()
            .with_context(|| format!("Failed to find resource: {:?}", name,))
            .unwrap();
        let global = windows::Win32::System::LibraryLoader::LoadResource(None, resource).unwrap();
        let data = windows::Win32::System::LibraryLoader::LockResource(global);
        let size = windows::Win32::System::LibraryLoader::SizeofResource(None, resource);
        std::slice::from_raw_parts(data as *const u8, size as _)
    }
}



#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let archive = get_binary_resource("ARCHIVE_ID");
    let install_dir = ide_ci::env::known::win::LOCALAPPDATA.get()?.join("Programs").join("Enso");
    install(install_dir, archive)?;
    Ok(())
}
