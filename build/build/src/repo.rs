use crate::prelude::*;


// ==============
// === Export ===
// ==============

pub mod cloud;



/// Heuristic that checks if given path can be plausibly considered to be the root of the Enso
/// repository.
///
/// Current heuristic is: contains Cargo workspace root and SBT build configuration file.
#[instrument(fields(path = %path.as_ref().display()), ret)]
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

#[instrument(ret, err)]
pub fn deduce_repository_path() -> Result<PathBuf> {
    let candidate_paths = [
        std::env::current_dir().ok(),
        std::env::current_dir().ok().and_then(|p| p.parent().map(ToOwned::to_owned)),
        std::env::current_dir().ok().and_then(|p| p.parent().map(|p| p.join("enso5"))),
        std::env::current_dir().ok().and_then(|p| p.parent().map(|p| p.join("enso"))),
    ];
    for candidate in candidate_paths {
        if let Some(path) = candidate && looks_like_enso_repository_root(&path) {
            return Ok(path)
        }
    }
    bail!("Could not deduce repository path.")
}
