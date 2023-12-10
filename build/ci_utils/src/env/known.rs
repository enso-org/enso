//! Universally known environment variables.

use crate::env::accessor::PathLike;



/// PATH environment variable.
///
/// It is a special variable that contains a list of paths, that define the search path for
/// executable files.
pub const PATH: PathLike = PathLike("PATH");

/// Windows-specific environment variables.
pub mod win {
    use crate::define_env_var;
    define_env_var! {
        /// Per-user custom settings and other information needed by applications.
        ///
        /// Example: `C:\Users\{username}\AppData\Roaming`
        APPDATA, PathBuf;

        /// Per-user custom settings and other information needed by applications that do not apply
        /// when the user roams.
        LOCALAPPDATA, PathBuf;

        /// The user's home directory.
        USERPROFILE, PathBuf;
    }
}
