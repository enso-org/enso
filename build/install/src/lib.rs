#![feature(default_free_fn)]

pub mod prelude {
    pub use enso_build::prelude::*;

    pub use winreg::enums::*;
    pub use winreg::RegKey;
}

// use prelude::*;
pub mod win;

pub mod config {
    /// The full filename of the application executable, including the extension.
    pub const APPLICATION_EXECUTABLE: &str = "Enso.exe";

    /// The name of the registry key where uninstall information is stored.
    ///
    /// The key is located under
    /// `HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Uninstall`.
    pub const APPLICATION_UNINSTALL_KEY: &str = "Enso";

    pub const APPLICATION_PRETTY_NAME: &str = "Enso";



    pub const APPLICATION_SHORTCUT_NAME: &str = APPLICATION_PRETTY_NAME;

    /// The [programmatic identifier](https://docs.microsoft.com/en-us/windows/win32/shell/fa-progids) of the Enso Source File.
    pub const SOURCE_FILE_PROG_ID: &str = "Enso.Source";

    /// The [programmatic identifier](https://docs.microsoft.com/en-us/windows/win32/shell/fa-progids) of the Enso Project Bundle.
    pub const PROJECT_BUNDLE_PROG_ID: &str = "Enso.ProjectBundle";

    /// The [programmatic identifiers](https://docs.microsoft.com/en-us/windows/win32/shell/fa-progids) registered by the Enso installer.
    pub const PROG_IDS: &[&str] = &[SOURCE_FILE_PROG_ID, PROJECT_BUNDLE_PROG_ID];

    /// The publisher name that will be displayed in the Add/Remove Programs dialog.
    pub const PUBLISHER_NAME: &str = "New Byte Order sp. z o.o.";

    /// The version of the installed application.
    pub const VERSION: &str = "2023.2.1-dev";
}
