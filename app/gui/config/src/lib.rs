//! Startup arguments definition.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use enso_prelude::*;

use enso_json_to_struct::json_to_struct;



// ==============
// === Errors ===
// ==============

/// A wrapper for an error with information that the Engine version does not meet the requirements.
#[derive(Clone, Debug, thiserror::Error)]
#[error("Unsupported Engine version: required {required} (or newer), found {found}.")]
pub struct UnsupportedEngineVersion {
    /// The version of the Engine that is required.
    pub required: semver::Version,
    /// The version of the Engine that was found.
    pub found:    semver::Version,
}

// ===============
// === Version ===
// ===============

include!(concat!(env!("OUT_DIR"), "/config.rs"));

pub use generated::*;

/// The minimum supported engine version.
pub fn engine_version_required() -> semver::Version {
    // Safe to unwrap, as `engine_version_supported` compile-time and is validated by the test.
    semver::Version::parse(engine_version_supported).unwrap()
}

/// Check if the given Engine version meets the requirements.
///
/// Effectively, this checks if the given version is greater or equal to the minimum supported.
/// "Greater or equal" is defined by the [Semantic Versioning specification](https://semver.org/)
/// term of precedence.
pub fn check_engine_version(
    engine_version: &semver::Version,
) -> Result<(), UnsupportedEngineVersion> {
    // We don't want to rely on the `semver::VersionReq` semantics here. Unfortunately the
    // [Semantic Versioning specification](https://semver.org/) does not define the semantics of
    // of the version requirement operators, so different implementations may behave differently.
    //
    // The `semver::VersionReq` implementation follows the Cargo's implementation, namely:
    // ```
    // In particular, in order for any VersionReq to match a pre-release version, the VersionReq
    // must contain at least one Comparator that has an explicit major, minor, and patch version
    // identical to the pre-release being matched, and that has a nonempty pre-release component.
    // ```
    // This leads to counter-intuitive behavior, where `2023.0.0-dev` does not fulfill the
    // `>= 2022.0.0-dev` requirement.
    if engine_version < &engine_version_required() {
        Err(UnsupportedEngineVersion {
            required: engine_version_required(),
            found:    engine_version.clone(),
        })
    } else {
        Ok(())
    }
}


// ============
// === Args ===
// ============

json_to_struct!(
    "../../../../lib/rust/ensogl/pack/js/src/runner/config.json",
    "../../../../app/ide-desktop/lib/content-config/src/config.json"
);

pub fn read_args() -> Args {
    debug_span!("Reading application arguments from JS.").in_scope(|| {
        let mut args = Args::default();
        if let Ok(js_app) = ensogl::system::js::app::app() {
            for param in js_app.config().params() {
                if let Some(value) = param.value() {
                    let path = format!("{}.value", param.structural_name());
                    if let Some(err) = args.set(&path, value) {
                        error!("{}", err.display())
                    }
                }
            }
        } else {
            error!("Could not connect to JS application. Using default configuration.")
        }
        args
    })
}

lazy_static! {
    pub static ref ARGS: Args = read_args();
}

// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_that_version_requirement_parses() {
        // We just expect that it won't panic.
        let _ = engine_version_required();
    }

    #[test]
    fn new_project_engine_version_fills_requirements() {
        // Sanity check: required version must be supported.
        assert!(check_engine_version(&engine_version_required()).is_ok());
    }
}
