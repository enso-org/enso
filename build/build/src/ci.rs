// use crate::prelude::*;

/// Labels used in the repositories where this library is used for CI.
/// They should be defined in the `.github/settings.yml` file.
pub mod labels {
    /// Name of the label that is used to mark the PRs that should not require changelog entry.
    pub const NO_CHANGELOG_CHECK: &str = "CI: No changelog needed";

    /// Name of the label that is used to mark the PRs that require clean builds.
    pub const CLEAN_BUILD_REQUIRED: &str = "CI: Clean build required";
}
