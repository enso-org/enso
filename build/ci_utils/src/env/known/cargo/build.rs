//! Environment variables that are set by Cargo for the build script run.
//!
//! See [Cargo's documentation](https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-build-scripts).



crate::define_env_var! {
    /// The folder in which all output and intermediate artifacts should be placed. This folder is
    /// inside the build directory for the package being built, and it is unique for the package in
    /// question.
    OUT_DIR, PathBuf;
}
