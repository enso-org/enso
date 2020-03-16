#![feature(option_unwrap_none)]
#![feature(trait_alias)]

use std::{fs, path};
use std::io::ErrorKind;

/// Types that can yield a reference to std::path::Path.
pub trait PathRef = AsRef<path::Path>;

/// A structure describing a concrete release package on GitHub.
pub struct GithubRelease<Str> {
    pub project_url : Str,
    pub version     : Str,
    pub filename    : Str,
}

impl<Str> GithubRelease<Str> {
    /// Download the release package from GitHub.
    ///
    /// The project_url should be a project's main page on GitHub.
    pub fn download(&self, destination_dir:&path::Path) where Str:AsRef<str> {
        let url = format!(
            "{project}/releases/download/{version}/{filename}",
            project  = self.project_url.as_ref(),
            version  = self.version.as_ref(),
            filename = self.filename.as_ref());
        let destination_dir_str = destination_dir.to_str().unwrap();
        let destination_file    = destination_dir.join(self.filename.as_ref());

        Self::remove_old_file(&destination_file);
        download_lp::download(url.as_str(),destination_dir_str).unwrap();
    }

    fn remove_old_file(file:&path::Path) {
        let result      = fs::remove_file(&file);
        let error       = result.err();
        let fatal_error = error.filter(|err| err.kind() != ErrorKind::NotFound);
        fatal_error.unwrap_none();
    }
}

/// Converts path to an absolute form.
pub fn absolute_path(path: impl PathRef) -> std::io::Result<path::PathBuf> {
    use path_clean::PathClean;
    let path = path.as_ref();
    if path.is_absolute() {
        Ok(path.to_path_buf().clean())
    } else {
        Ok(std::env::current_dir()?.join(path).clean())
    }
}

/// Get the environment variable or panic if not available.
pub fn env_var_or_panic(var_name:&str) -> String {
    match std::env::var(var_name) {
        Ok(var) => var,
        Err(e)  =>
            panic!("failed to read environment variable {}: {}", var_name, e),
    }
}

/// Checks if the current build is targeting wasm32.
///
/// Relies on `TARGET` environment variable set by cargo for build scripts.
pub fn targeting_wasm() -> bool {
    let target = env_var_or_panic("TARGET");
    target.contains("wasm32")
}
