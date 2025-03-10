//! Environment variables that Cargo sets for build scripts.
//!
//! See: https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-build-scripts

use crate::prelude::*;



crate::define_env_var! {
    /// Checks if the current build is targeting wasm32.
    ///
    /// Relies on `TARGET` environment variable set by cargo for build scripts.
    TARGET, String;

     /// The folder in which all output and intermediate artifacts should be placed. This folder is
     /// inside the build directory for the package being built, and it is unique for the package in
     /// question.
     OUT_DIR, PathBuf;
}

pub fn targeting_wasm() -> bool {
    TARGET.get().map_or(false, |target| target.contains("wasm32"))
}
