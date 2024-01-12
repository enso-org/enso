// use crate::prelude::*;

use sysinfo::SystemExt;



/// Labels used in the repositories where this library is used for CI.
/// They should be defined in the `.github/settings.yml` file.
pub mod labels {
    /// Name of the label that is used to mark the PRs that should not require changelog entry.
    pub const NO_CHANGELOG_CHECK: &str = "CI: No changelog needed";

    /// Name of the label that is used to mark the PRs that require clean builds.
    pub const CLEAN_BUILD_REQUIRED: &str = "CI: Clean build required";
}

/// Check if this is a "big memory" machine.
///
/// Our self-hosted runners are big memory machines, but the GitHub-hosted ones are not.
///
/// Certain CI operations are only performed on big machines, as they require a lot of memory.
pub fn big_memory_machine() -> bool {
    let github_hosted_macos_memory = 15_032_385;
    let mut system = sysinfo::System::new();
    system.refresh_memory();
    system.total_memory() > github_hosted_macos_memory
}
