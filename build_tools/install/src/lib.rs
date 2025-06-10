//! This crate is linked in both by the installer and the uninstaller.



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
use ide_ci::log::file_log_layer;
use ide_ci::log::stderr_log_layer;
use ide_ci::log::GlobalFilteringLayer;
use prelude::*;
use sysinfo::Pid;

#[cfg(windows)]
pub mod win;


/// A macro for accessing compiled-in JSON data.
///
/// # Parameters
/// - `$env` - the name of the environment variable that contains the path to the JSON file.
/// - `$typename` - the type of the data that is stored in the JSON file.
/// - `$pretty_name` - a human-readable name of the data type that will be used in error messages.
#[macro_export]
macro_rules! access_built_time_env {
    ($env:ident, $typename:ty, $pretty_name:expr) => {
        {
        static DATA: std::sync::LazyLock<$typename> = std::sync::LazyLock::new(|| {
            let crate_name = env!("CARGO_PKG_NAME");
            let pretty = $pretty_name;
            let path = env!(stringify!($env));
            let data = include_str!(env!(stringify!($env)));
            if path.is_empty() {
                panic!("The path to the {pretty} is empty. The {crate_name} was built without `{}` environment variable set.", stringify!($env));
            }
            if data.is_empty() {
                panic!("The {pretty} file is empty. Likely the stub was provided to enable compilation. Investigate the build logs warnings.");
            }
            serde_json::from_str(data).expect(&format!("Failed to parse the {pretty}."))
        });
        &DATA
        }
    };
}

/// Access compiled-in `electron builder`-based configuration.
///
/// # Panics
///
/// This function will panic if the path to the configuration was not set during the build process,
/// or if the configuration was invalid.
pub fn sanitized_electron_builder_config() -> &'static electron_builder::Config {
    access_built_time_env!(
        ENSO_INSTALL_ELECTRON_BUILDER_CONFIG,
        electron_builder::Config,
        "Electron Builder configuration"
    )
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
pub fn installation_file_lock() -> Result<named_lock::NamedLock> {
    let name = env!("CARGO_PKG_NAME");
    let lock = named_lock::NamedLock::create(name)
        .with_context(|| format!("Failed to create a named file lock for '{name}'."))?;
    Ok(lock)
}

/// Acquire the named file lock and return the guard.
pub fn locked_installation_lock() -> Result<named_lock::NamedLockGuard> {
    installation_file_lock()?.try_lock().with_context(|| "Failed to acquire the named file lock. Is there another instance of the installer or uninstaller running?")
}

/// Check if the application is already running.
///
/// If there is any process running from the installation directory, returns an error message as
/// `Some`. If there are no such processes, returns `None`. If the necessary information cannot be
/// obtained, returns an error.
///
/// The processes are matched using their executable paths. Processes for which the path cannot be
/// obtained are ignored.
pub fn is_already_running(install_dir: &Path, ignored_pids: &[Pid]) -> Result<Option<String>> {
    let install_dir = install_dir.canonicalize()?;
    let mut offending_processes = vec![];

    // First get process list.
    let mut sys = sysinfo::System::new();
    sys.refresh_processes();
    for (pid, process) in sys.processes() {
        if ignored_pids.contains(pid) {
            trace!("Process {} ({}) is ignored.", process.name(), pid);
            continue;
        }
        let Some(path) = process.exe() else {
            warn!("Process {} ({}) has no path.", process.name(), pid);
            continue;
        };
        let Ok(path) = path.canonicalize() else {
            warn!("Failed to canonicalize process path: {}", path.display());
            continue;
        };

        if path.starts_with(&install_dir) {
            offending_processes.push(process);
            info!("Process {} ({}) is in the installation directory.", process.name(), pid);
        } else {
            trace!("Process {} ({}) is not in the installation directory.", process.name(), pid);
        }
    }

    if !offending_processes.is_empty() {
        let processes_list = offending_processes
            .iter()
            .map(|p| format!(" * {} (pid {})", p.name(), p.pid()))
            .join("\n");
        let message = format!("It seems that the application is currently running. Please close it before running the installer.\n\nThe following processes are running from the installation directory:\n{}", processes_list);
        Ok(Some(message))
    } else {
        Ok(None)
    }
}

/// Setup logging for the installer. It logs both to stderr and to a file in the temp directory.
///
/// Returns the path to the generated log file.
pub fn setup_logging(app_name: &str) -> Result<PathBuf> {
    use tracing_subscriber::prelude::*;
    // Generate filename based on the current time.
    let timestamp = chrono::Local::now().format("%Y-%m-%d %H-%M-%S");
    let filename = format!("{app_name}-{timestamp}.log");
    let temp_location = std::env::temp_dir();
    let log_file = temp_location.join(filename);
    let file = ide_ci::fs::create(&log_file)?;
    let registry = tracing_subscriber::Registry::default()
        .with(GlobalFilteringLayer)
        .with(stderr_log_layer())
        .with(file_log_layer(file));

    tracing::subscriber::set_global_default(registry)
        .context("Failed to set global default subscriber.")?;
    Ok(log_file)
}
