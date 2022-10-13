#[allow(unused_imports)]
use crate::prelude::*;

use ide_ci::define_env_var;
use ide_ci::programs::docker::ContainerId;



define_env_var! {
    ENSO_RELEASE_ID, octocrab::models::ReleaseId;

    /// Name of the container that is running the current build.
    ENSO_RUNNER_CONTAINER_NAME, ContainerId;

    ENSO_NIGHTLY_EDITIONS_LIMIT, usize;
}
