//! Universally known environment variables.

use crate::prelude::*;

use crate::define_env_var;
use crate::env::accessor::PathBufVariable;
use crate::env::accessor::PathLike;


// ==============
// === Export ===
// ==============

pub mod cargo;
pub mod electron_builder;



/// PATH environment variable.
///
/// It is a special variable that contains a list of paths, that define the search path for
/// executable files.
pub const PATH: PathLike = PathLike("PATH");

/// Windows-specific environment variables.
pub mod win {
    use super::*;

    define_env_var! {
        /// Per-user custom settings and other information needed by applications.
        ///
        /// Example: `C:\Users\{username}\AppData\Roaming`
        APPDATA, PathBuf;

        /// Per-user custom settings and other information needed by applications that do not apply
        /// when the user roams.
        LOCALAPPDATA, PathBuf;

        /// Directory where all programs can store their global data.
        PROGRAMDATA, PathBuf;

        /// Directory where programs are installed (native architecture).
        PROGRAMFILES, PathBuf;

        /// The user's home directory.
        USERPROFILE, PathBuf;
    }

    /// Directory where 32-bit programs are installed.
    pub const PROGRAMFILES_X86: PathBufVariable = PathBufVariable("ProgramFiles(x86)");



    /// Directory containing user's Start menu programs shortcuts.
    pub fn start_menu_programs() -> Result<PathBuf> {
        Ok(APPDATA.get()?.join_iter(["Microsoft", "Windows", "Start Menu", "Programs"]))
    }
}

define_env_var! {
    /// Overrides individual `LC_*` settings for consistent locale-specific behavior across programs.
    /// - [`LC_TIME`]: Defines formatting for dates and times.
    /// - [`LC_COLLATE`]: Determines the sorting order of strings, influencing string comparison operations.
    /// - [`LC_MONETARY`]: Sets the format for monetary values, including currency symbols and decimal separators.
    /// Use `LC_ALL` to uniformly apply these settings, which is especially useful in scripts or when debugging
    /// to avoid locale-related inconsistencies.
    LC_ALL, String;

    /// Defines formatting for dates and times.
    LC_TIME, String;

    /// Determines the sorting order of strings.
    LC_COLLATE, String;

    /// Sets the format for monetary values.
    LC_MONETARY, String;
}

/// The `C.UTF-8` locale, when used as a value for [`LC_ALL`] or other `LC_*` environment variables
/// in Unix-like systems, combines the minimalistic behavior of the default C locale with UTF-8
/// character encoding.
pub const C_UTF8: &str = "C.UTF-8";
