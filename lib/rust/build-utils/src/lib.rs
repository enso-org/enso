//! A crate with many utilities for build scripts, for example downloading packages form GitHub or
//! easier management of env vars and paths.

// === Features ===
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use std::fmt::Display;
use std::io::ErrorKind;
use std::path;



// =====================
// === GithubRelease ===
// =====================

/// Types that can yield a reference to std::path::Path.
pub trait PathRef = AsRef<path::Path>;

/// A structure describing a concrete release package on GitHub. The project_url should be a
/// project's main page on GitHub.
pub struct GithubRelease<T> {
    pub project_url: T,
    pub version:     T,
    pub filename:    T,
}

impl<T: AsRef<str> + Display> GithubRelease<T> {
    /// Download the release package from GitHub. If the target file already exists, it will be
    /// removed first.
    pub fn download(&self, destination_dir: &path::Path) {
        let url =
            format!("{}/releases/download/{}/{}", self.project_url, self.version, self.filename);
        let destination_file = destination_dir.join(self.filename.as_ref());
        Self::remove_old_file(&destination_file);
        let mut resp = reqwest::blocking::get(&url).expect("Download failed.");
        let mut out = std::fs::File::create(destination_file).expect("Failed to create file.");
        std::io::copy(&mut resp, &mut out).expect("Failed to copy file content.");
    }

    fn remove_old_file(file: &path::Path) {
        let result = std::fs::remove_file(&file);
        let error = result.err();
        let fatal_error = error.filter(|err| err.kind() != ErrorKind::NotFound);
        assert!(fatal_error.is_none());
    }
}



// =====================
// === GithubRelease ===
// =====================

#[derive(Debug, Clone, serde::Deserialize)]
pub struct GithubFile {
    pub name: String,
}

/// A structure describing a concrete release package on GitHub.
pub struct GoogleFontsRelease {
    pub name: String,
}

impl GoogleFontsRelease {
    /// Download the release package from GitHub. If the target file already exists, it will be
    /// removed first.
    pub fn download(&self, destination_dir: &path::Path) -> Vec<GithubFile> {
        let url = format!("https://api.github.com/repos/google/fonts/contents/ofl/{}", self.name);
        // let destination_file = destination_dir.join(self.filename.as_ref());
        // Self::remove_old_file(&destination_file);
        let request = reqwest::blocking::Client::builder().user_agent("request").build().unwrap();
        let resp = request.get(&url).send().expect("Failed to get GitHub response.");
        let files: Vec<GithubFile> = resp.json().expect("Failed to parse JSON.");
        let font_files: Vec<_> =
            files.into_iter().filter(|file| file.name.ends_with(".ttf")).collect();
        for file in &font_files {
            let url = format!(
                "https://raw.githubusercontent.com/google/fonts/master/ofl/{}/{}",
                self.name, file.name
            );
            let destination_file = destination_dir.join(&file.name);
            Self::remove_old_file(&destination_file);
            let mut resp = reqwest::blocking::get(&url).expect("Download failed.");
            let mut out = std::fs::File::create(destination_file).expect("Failed to create file.");
            std::io::copy(&mut resp, &mut out).expect("Failed to copy file content.");
        }
        font_files
    }

    fn remove_old_file(file: &path::Path) {
        let result = std::fs::remove_file(&file);
        let error = result.err();
        let fatal_error = error.filter(|err| err.kind() != ErrorKind::NotFound);
        assert!(fatal_error.is_none());
    }
}



// =======================
// === Path Conversion ===
// =======================

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



// ======================
// === Env Management ===
// ======================

/// Get the environment variable or panic if not available.
pub fn env_var_or_panic(var_name: &str) -> String {
    match std::env::var(var_name) {
        Ok(var) => var,
        Err(e) => panic!("Failed to read environment variable {}: {}.", var_name, e),
    }
}



// ==========================
// === Build Target Utils ===
// ==========================

/// Checks if the current build is targeting wasm32.
///
/// Relies on `TARGET` environment variable set by cargo for build scripts.
pub fn targeting_wasm() -> bool {
    let target = env_var_or_panic("TARGET");
    target.contains("wasm32")
}
