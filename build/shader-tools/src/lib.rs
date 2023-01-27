//! This crate deals with packaging and shader-tools.
//!
//! Shader tools package contains all the tools that we use for shader precompilation. We release it
//! through https://github.com/enso-org/shader-tools/, so build script can download binaries from
//! there, rather than requiring the user to install them manually.

#![recursion_limit = "1024"]
// === Features ===
#![feature(default_free_fn)]
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(hash_drain_filter)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use prelude::*;

use ide_ci::define_env_var;
use ide_ci::github::release;
use ide_ci::github::repo;
use ide_ci::github::setup_octocrab;
use ide_ci::github::RepoRef;
use ide_ci::goodies::shader_tools::SHADER_TOOLS_REPO;


// ==============
// === Export ===
// ==============

pub mod ci;
pub mod shaderc;
pub mod spirv_cross;

pub use ide_ci::prelude;



define_env_var! {
    /// ID of the release that we are deploying built assets to.
    ENSO_RELEASE_ID, octocrab::models::ReleaseId;
}

/// Create a package with shader tools for the current platform.
///
/// The package can be later uploaded as a release asset.
#[context("Failed to create a package with shader tools.")]
pub async fn create_package(output_archive: &Path) -> Result {
    let package = tempfile::tempdir()?;
    shaderc::generate_stripped_package(package.path()).await?;
    spirv_cross::generate_spirv_cross_package(package.path()).await?;
    ide_ci::archive::compress_directory_contents(&output_archive, package.path()).await?;
    Ok(())
}

/// Get the handle to shader tools repository using system environment.
///
/// Note that currently the repository is hardcoded to `enso-org/shader-tools`.
#[context("Failed to get the handle to shader tools repository.")]
pub async fn repo_handle_from_env() -> Result<repo::Handle<RepoRef<'static>>> {
    let octo = setup_octocrab().await?;
    let handle = SHADER_TOOLS_REPO.into_handle(&octo);
    Ok(handle)
}

/// Get the handle to the release that we are deploying assets to.
///
/// Requires some environment setup, done by [`create_release`].
#[context("Failed to get the handle to the release that we are deploying assets to.")]
pub async fn release_handle_from_env() -> Result<release::Handle> {
    let repo::Handle { octocrab, repo } = repo_handle_from_env().await?;
    let handle = release::Handle::new(&octocrab, repo, ENSO_RELEASE_ID.get()?);
    Ok(handle)
}

/// Create a draft release for the current version of the shader tools.
#[context("Failed to create a new draft release of the shader tools.")]
pub async fn create_release(
    handle: repo::Handle<impl IsRepo>,
    tag: impl AsRef<str>,
) -> Result<octocrab::models::repos::Release> {
    let release = handle.repos().releases().create(&tag).draft(true).send().await?;
    ide_ci::actions::workflow::set_output(ENSO_RELEASE_ID.name, &release.id).await?;
    Ok(release)
}
