//! Helpers for managing known application paths on Windows.
//!
//! The `App Paths` registry key is necessary for the Windows shell to find the
//! application executable when it is invoked by name.
//!
//! See https://docs.microsoft.com/en-us/windows/win32/shell/app-registration

use crate::prelude::*;
use winreg::enums::*;



/// The registry key path for the `App Paths` in Windows, relative to `HKEY_CURRENT_USER`.
pub const APP_PATHS: &str = r"Software\Microsoft\Windows\CurrentVersion\App Paths";


/// Information stored in the `App Paths` registry key for an application.
#[derive(Clone, Debug)]
pub struct AppPathInfo {
    /// The path to the application executable.
    ///
    /// It is used both for:
    /// * executable filename (which will allow shell to find the executable when it is invoked by
    ///   name, e.g. when using `Win+R`)
    /// * full qualified executable path that will be used to launch the application.
    pub executable_path: PathBuf,

    /// Additional directories that will be prepended to the `PATH` environment variable when
    /// launching the application through the `ShellExecuteEx` API.
    pub additional_directories: Vec<PathBuf>,
}

impl AppPathInfo {
    /// Create a new `AppPathInfo` instance.
    pub fn new(executable_path: impl Into<PathBuf>) -> Self {
        Self {
            executable_path:        executable_path.into(),
            // By default don't add anything to path, as we don't want to rely on any
            // Windows shell-specific behavior.
            additional_directories: vec![],
        }
    }

    /// Write the application path information to the registry.
    pub fn write_to_registry(&self) -> Result {
        let executable_name = self.executable_path.try_file_name()?.as_str();
        let app_paths = crate::win::registry::open_subkey_with_flags(
            &RegKey::predef(HKEY_CURRENT_USER),
            APP_PATHS,
            KEY_READ | KEY_WRITE,
        )?;
        let (exe_key, _) = crate::win::registry::create_subkey(&app_paths, executable_name)?;
        crate::win::registry::set_value(&exe_key, "", &self.executable_path.as_str())?;
        if !self.additional_directories.is_empty() {
            let path = self
                .additional_directories
                .iter()
                .map(|p| p.as_str())
                .collect::<Vec<_>>()
                .join(";");
            crate::win::registry::set_value(&exe_key, "Path", &path)?;
        }
        Ok(())
    }
}
