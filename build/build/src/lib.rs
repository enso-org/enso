// === Features ===
#![feature(hash_set_entry)]
#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(let_chains)]
#![feature(exit_status_error)]
#![feature(async_closure)]
#![feature(associated_type_bounds)]
#![feature(option_result_contains)]
#![feature(result_flattening)]
#![feature(default_free_fn)]
#![feature(map_first_last)]
#![feature(result_option_inspect)]
#![feature(associated_type_defaults)]
#![feature(once_cell)]
#![feature(duration_constants)]
#![feature(slice_take)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use crate::prelude::*;

use anyhow::Context;
use ide_ci::programs::java;
use regex::Regex;



pub mod prelude {
    pub use ide_ci::prelude::*;
}

pub mod aws;
pub mod bump_version;
pub mod changelog;
pub mod ci;
pub mod config;
pub mod context;
pub mod engine;
pub mod enso;
pub mod env;
pub mod httpbin;
pub mod ide;
pub mod paths;
pub mod postgres;
pub mod prettier;
pub mod programs;
pub mod project;
pub mod project_manager;
pub mod release;
pub mod repo;
pub mod rust;
pub mod source;
pub mod version;

/// Get version of Enso from the `build.sbt` file contents.
pub fn get_enso_version(build_sbt_contents: &str) -> Result<Version> {
    let version_regex = Regex::new(r#"(?m)^val *ensoVersion *= *"([^"]*)".*$"#)?;
    let version_string = version_regex
        .captures(build_sbt_contents)
        .context("Failed to find line with version string.")?
        .get(1)
        // The `expect` below will not fail due to the regex definition, as is ensured by unit test.
        .expect("Missing subcapture #1 with version despite matching the regex.")
        .as_str();
    Version::parse(version_string).anyhow_err()
}

pub fn get_string_assignment_value(
    build_sbt_contents: &str,
    variable_name: &str,
) -> Result<String> {
    let regex_text = format!(r#"(?m)^val *{variable_name} *= *"([^"]*)".*$"#);
    let regex = Regex::new(&regex_text)?;
    Ok(regex
        .captures(build_sbt_contents)
        .context(format!(
                "Failed to find line with assignment to `{variable_name}`. Does it match the following regex?   {regex_text}  "
            )
        )?
        .get(1)
        // The below denotes an internal error in our regex syntax, we do want panic.
        .expect("Missing subcapture #1 with version despite matching the regex.")
        .as_str()
        .to_string())
}

/// Get version of Enso from the `build.sbt` file contents.
pub fn get_graal_version(build_sbt_contents: &str) -> Result<Version> {
    get_string_assignment_value(build_sbt_contents, "graalVersion")?.parse2()
}

/// Get version of Enso from the `build.sbt` file contents.
pub fn get_java_major_version(build_sbt_contents: &str) -> Result<java::LanguageVersion> {
    get_string_assignment_value(build_sbt_contents, "javaVersion")?.parse2()
}

pub fn retrieve_github_access_token() -> Result<String> {
    fn get_token_from_file() -> Result<String> {
        let path =
            dirs::home_dir().context("Failed to locate home directory.")?.join("GITHUB_TOKEN");
        let content = ide_ci::fs::read_to_string(path)?;
        Ok(content.trim().into())
    }

    ide_ci::env::expect_var("GITHUB_TOKEN")
        .inspect(|_| debug!("Will use GITHUB_TOKEN environment variable."))
        .or_else(|_| get_token_from_file())
}

#[context("Failed to setup GitHub API client.")]
pub async fn setup_octocrab() -> Result<Octocrab> {
    let mut builder = octocrab::OctocrabBuilder::new();
    if let Ok(access_token) = retrieve_github_access_token() {
        builder = builder.personal_token(access_token);
        let octocrab = builder.build()?;
        match octocrab.ratelimit().get().await {
            Ok(rate) => info!(
                "GitHub API rate limit: {}/{}.",
                rate.resources.core.used, rate.resources.core.limit
            ),
            Err(e) => bail!(
                "Failed to get rate limit info: {e}. GitHub Personal Access Token might be invalid."
            ),
        }
        Ok(octocrab)
    } else {
        builder.build().anyhow_err()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore]
    async fn setup_octocrab_test() -> Result {
        let _client = setup_octocrab().await?;
        Ok(())
    }


    #[test]
    pub fn get_enso_version_test() -> Result {
        let contents = r#"
val scalacVersion  = "2.13.6"
val rustVersion    = "1.58.0-nightly"
val graalVersion   = "21.1.0"
val javaVersion    = "11"
val ensoVersion    = "0.2.32-SNAPSHOT"  // Note [Engine And Launcher Version]
val currentEdition = "2021.20-SNAPSHOT" // Note [Default Editions]
val stdLibVersion  = ensoVersion
"#;
        let version = get_enso_version(contents)?;
        assert_eq!(version.major, 0);
        assert_eq!(version.minor, 2);
        assert_eq!(version.patch, 32);
        assert_eq!(version.pre.as_str(), "SNAPSHOT");

        debug!("{}\n{:?}", version, version);
        Ok(())
    }

    #[test]
    pub fn get_graal_version_test() -> Result {
        let contents = r#"
val scalacVersion         = "2.13.7"
val graalVersion          = "21.1.0"
val javaVersion           = "11"
val defaultDevEnsoVersion = "0.0.0-dev"
val ensoVersion = sys.env.getOrElse(
  "ENSO_VERSION",
  defaultDevEnsoVersion
) // Note [Engine And Launcher Version]
val currentEdition = sys.env.getOrElse(
  "ENSO_EDITION",
  defaultDevEnsoVersion
) // Note [Default Editions]

// Note [Stdlib Version]
val stdLibVersion       = defaultDevEnsoVersion
"#;
        let version = get_graal_version(contents)?;
        assert_eq!(version.major, 21);
        assert_eq!(version.minor, 1);
        assert_eq!(version.patch, 0);
        Ok(())
    }
}
