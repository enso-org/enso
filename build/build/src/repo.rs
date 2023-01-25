//! Module for dealing with repositories owned by our project.

use crate::prelude::*;


// ==============
// === Export ===
// ==============

pub mod cloud;



/// Heuristic that checks if given path can be plausibly considered to be the root of the Enso
/// repository.
///
/// Current heuristic is: contains Cargo workspace root and SBT build configuration file.
#[instrument(level = "trace", fields(path = %path.as_ref().display()), ret)]
pub fn looks_like_enso_repository_root(path: impl AsRef<Path>) -> bool {
    (move || -> Result<bool> {
        let cargo_toml = path.as_ref().join("Cargo.toml");
        if !ide_ci::fs::read_to_string(cargo_toml)?.contains("[workspace]") {
            return Ok(false);
        }

        Ok(path.as_ref().join("build.sbt").exists())
    })()
    .unwrap_or(false)
}

/// Deduce the path to the root of the Enso repository.
///
/// This function will traverse the filesystem upwards from the binary location until it finds a
/// directory that looks like the root of the Enso repository.
#[instrument(ret, err)]
pub fn deduce_repository_path() -> Result<PathBuf> {
    let mut path = ide_ci::env::current_exe()?;
    while !looks_like_enso_repository_root(&path) {
        ensure!(path.pop(), "Failed to deduce repository path.");
    }
    Ok(path)
}
