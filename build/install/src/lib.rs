// === Features ===
#![feature(lazy_cell)]



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
use enso_install_config::ENSO_BUILD_ELECTRON_BUILDER_CONFIG;
use enso_install_config::ENSO_INSTALL_ARCHIVE_PATH;
use enso_install_config::ENSO_INSTALL_ELECTRON_BUILDER_CONFIG;
use enso_install_config::INSTALLER_PAYLOAD_ID;
use prelude::*;

#[cfg(windows)]
pub mod win;


/// Access compiled-in `electron builder`-based configuration.
///
/// # Panics
///
/// This function will panic if the path to the configuration was not set during the build process,
/// or if the configuration was invalid.
pub fn sanitized_electron_builder_config() -> &'static electron_builder::Config {
    static CONFIG: std::sync::LazyLock<electron_builder::Config> = std::sync::LazyLock::new(|| {
        let config_path = env!("ENSO_INSTALL_ELECTRON_BUILDER_CONFIG");
        let data = include_str!(env!("ENSO_INSTALL_ELECTRON_BUILDER_CONFIG"));
        if config_path.is_empty() {
            panic!("The path to the electron-builder config is empty. The installer was built without `{ENSO_INSTALL_ELECTRON_BUILDER_CONFIG}` environment variable set.");
        }
        if data.is_empty() {
            panic!("The electron-builder config is empty. Probably the installer was built without `{ENSO_BUILD_ELECTRON_BUILDER_CONFIG}` environment variable set.");
        }
        serde_json::from_str(data).expect("Failed to parse the electron-builder config.")
    });
    &CONFIG
}

/// Get the binary payload of the installer that was compiled into the executable.
pub fn get_package_payload() -> Result<&'static [u8]> {
    win::resource::get_binary(INSTALLER_PAYLOAD_ID).with_context(|| format!("Failed to get the installer payload. Was {ENSO_INSTALL_ARCHIVE_PATH} defined during the build?"))
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

/// Acquire the named file lock and return the guard.
pub fn locked_lock() -> Result<named_lock::NamedLockGuard> {
    lock()?.lock().with_context(|| "Failed to acquire the named file lock. Is there another instance of the installer or uninstaller running?")
}
