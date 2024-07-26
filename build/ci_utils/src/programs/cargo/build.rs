//! Directives for Cargo build scripts.
//!
//! Should be used in `build.rs` files to communicate with Cargo.

use crate::prelude::*;



/// Sets an environment variable.
pub fn expose_env_var(var_name: impl AsRef<str>, var_value: impl AsRef<str>) {
    println!("cargo:rustc-env={}={}", var_name.as_ref(), var_value.as_ref());
}

/// Tells Cargo to rerun the build script if the given file changes.
pub fn rerun_if_file_changed(file_path: impl AsRef<Path>) {
    println!("cargo:rerun-if-changed={}", file_path.as_ref().display());
}

/// Tells Cargo to rerun the build script if the given environment variable changes.
pub fn rerun_if_env_changed(var_name: impl AsRef<str>) {
    println!("cargo:rerun-if-env-changed={}", var_name.as_ref());
}
