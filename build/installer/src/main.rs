#![feature(core_intrinsics)]
#![feature(default_free_fn)]

use enso_build::prelude::*;
use std::intrinsics::black_box;
use winreg::enums::HKEY_CURRENT_USER;


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
        // Register the file type's ProgID.
        let (prog_id_key, _disposition) = classes.create_subkey(&self.prog_id)?;
        prog_id_key.set_value("", &self.friendly_name)?;
        prog_id_key.set_value("FriendlyTypeName", &self.friendly_name)?;
        prog_id_key.set_value("InfoTip", &"Enso source file")?;
        prog_id_key
            .create_subkey("DefaultIcon")?
            .0
            .set_value("", &format!(r#""{}",4"#, self.application_path.display()))?;
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
        prog_id:          "Enso.File".to_string(),
        friendly_name:    "Enso Source File".to_string(),
    };
    enso_file_type.register()?;

    let enso_project_type = FileType {
        extensions:       vec![".enso-project".to_string()],
        application_path: executable_path.as_ref().to_path_buf(),
        prog_id:          "Enso.Project".to_string(),
        friendly_name:    "Enso Project Bundle".to_string(),
    };
    enso_project_type.register()?;

    // All direct WinAPI calls are unsafe (Rust-wise), however this particular one should never be
    // able to cause any harm. It cannot even fail API-wise (it returns `void`).
    unsafe {
        // Notifying shell about the change, so it is catched without the need to restart the
        // system. As recommended by https://learn.microsoft.com/en-us/windows/win32/shell/fa-file-types#setting-optional-subkeys-and-file-type-extension-attributes
        // https://learn.microsoft.com/en-us/windows/win32/api/shlobj_core/nf-shlobj_core-shchangenotify
        windows::Win32::UI::Shell::SHChangeNotify(
            windows::Win32::UI::Shell::SHCNE_ASSOCCHANGED,
            windows::Win32::UI::Shell::SHCNF_FLAGS(0),
            None,
            None,
        );
    }
    Ok(())
}

pub struct ApplicationInfo {
    /// The absolute path to the application executable.
    pub executable_path: PathBuf,
    // TODO: more
}

pub fn register_application(info: ApplicationInfo) -> Result {
    // See https://learn.microsoft.com/en-us/windows/win32/shell/app-registration

    let executable_name = info.executable_path.try_file_name()?.as_str();
    let executable_dir = info.executable_path.try_parent()?.as_str();

    // Use App Paths registry key to register the application.
    let (exe_key, _) = RegKey::predef(HKEY_CURRENT_USER)
        .open_subkey_with_flags(r"Software\Microsoft\Windows\CurrentVersion\App Paths", KEY_READ | KEY_WRITE)
        .with_context(|| {
            format!(r#"Failed to open `HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\App Paths` key."#)
        })?
        .create_subkey(&executable_name)
        .with_context(|| {
            format!(r#"Failed to create subkey for application: {}"#, executable_name)
        })?;

    exe_key.set_value("", &info.executable_path)?;
    exe_key.set_value("Path", &executable_dir)?;
    Ok(())
}

/// Install Enso.
///
/// The archive payload is binary data of the tar.gz archive that contains the Enso app.
pub fn install(install_location: impl AsRef<Path>, archive_payload: &[u8]) -> Result {
    info!("Removing old installation (if present).");
    // ide_ci::fs::reset_dir(&install_location)?;

    let to_our_path = |path_in_archive: &Path| -> Option<PathBuf> {
        Some(install_location.as_ref().join(path_in_archive))
    };

    // Extract the files.
    let decoder = flate2::read::GzDecoder::new(archive_payload);
    let archive = tar::Archive::new(decoder);
    info!("Extracting files.");
    // ide_ci::archive::tar::extract_files_sync(archive, to_our_path)?;

    info!("Registering file types.");
    register_file_association(install_location.as_ref().join("Enso.exe"))?;

    info!("Installation complete.");
    Ok(())
}

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let archive = include_bytes!(r"C:\Users\mwurb\enso-win-2023.2.1-dev.tar.gz");
    black_box(archive);

    let local_app_data = ide_ci::env::known::win::LOCALAPPDATA.get()?;
    let install_location = local_app_data.join_iter(["Programs", "Enso"]);
    install(install_location, archive)?;

    println!("Hello, world!");
    // println!("{}", archive[666]);
    Ok(())
}
