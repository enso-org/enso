// === Features ===
#![feature(lazy_cell)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]



pub mod prelude {
    pub use ide_ci::prelude::*;

    #[cfg(windows)]
    pub use winreg::types::ToRegValue;
    #[cfg(windows)]
    pub use winreg::RegKey;
    #[cfg(windows)]
    pub use winreg::RegValue;
}

use enso_install_config::electron_builder;
use prelude::*;

#[cfg(windows)]
pub mod win;

pub fn sanitized_electron_builder_config() -> &'static electron_builder::Config {
    static CONFIG: std::sync::LazyLock<electron_builder::Config> = std::sync::LazyLock::new(|| {
        let data = include_str!(env!("ENSO_INSTALL_ELECTRON_BUILDER_CONFIG"));
        serde_json::from_str(data).expect("Failed to parse the electron-builder config.")
    });
    &CONFIG
}

/// The name of the Windows registry key where uninstall information is stored.
///
/// The key is located under
/// `HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Uninstall`.
pub fn uninstall_key() -> &'static str {
    &sanitized_electron_builder_config().product_name
}

/// The full filename (not path!) of the application executable, including the extension.
pub fn executable_filename() -> PathBuf {
    sanitized_electron_builder_config().product_name.with_executable_extension()
}

/// The name of the shortcut.
///
/// Used on Windows for Start Menu and Desktop shortcuts.
pub fn shortcut_name() -> &'static str {
    &sanitized_electron_builder_config().product_name
}

// pub mod config {
//
//     pub const SOURCE_FILE_PROG_ID: &str = "Enso.Source";
//
//     /// The [programmatic identifier](https://docs.microsoft.com/en-us/windows/win32/shell/fa-progids) of the Enso Project Bundle.
//     pub const PROJECT_BUNDLE_PROG_ID: &str = "Enso.ProjectBundle";
//
//     /// The [programmatic identifiers](https://docs.microsoft.com/en-us/windows/win32/shell/fa-progids) registered by the Enso installer.
//     pub const PROG_IDS: &[&str] = &[SOURCE_FILE_PROG_ID, PROJECT_BUNDLE_PROG_ID];
//
//     /// The publisher name that will be displayed in the Add/Remove Programs dialog.
//     pub const PUBLISHER_NAME: &str = "New Byte Order sp. z o.o.";
// }

/// Acquire a named file lock.
///
/// The lock is to be used to ensure that only one instance of the (un)installer is running at a
/// time.
pub fn lock() -> Result<named_lock::NamedLock> {
    let name = env!("CARGO_PKG_NAME");
    let lock = named_lock::NamedLock::create(name)
        .with_context(|| format!("Failed to create a named file lock for '{name}'."))?;
    Ok(lock)
}
