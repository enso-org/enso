//! A crate with many utilities for build scripts, for example downloading packages form GitHub or
//! easier management of env vars and paths.

// === Features ===
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ide_ci::prelude::*;



// =====================
// === GithubRelease ===
// =====================

/// A structure describing a concrete release package on GitHub. The [`project_url`] should be a
/// project's main page on GitHub.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct GithubRelease<T> {
    pub project_url: T,
    pub version:     T,
    pub filename:    T,
}

impl<T: AsRef<str> + Display> GithubRelease<T> {
    /// URL that can be used to download this asset from a GitHub release.
    pub fn url(&self) -> Result<Url> {
        format!("{}/releases/download/{}/{}", self.project_url, self.version, self.filename)
            .parse2()
    }
}
