#[allow(unused_imports)]
use crate::prelude::*;

use ide_ci::env::Variable;
use ide_ci::programs::docker::ContainerId;



#[derive(Clone, Copy, Debug)]
pub struct ReleaseId;
impl Variable for ReleaseId {
    const NAME: &'static str = "ENSO_RELEASE_ID";
    type Value = octocrab::models::ReleaseId;
}

#[derive(Clone, Copy, Debug)]
pub struct RunnerContainerName;
impl Variable for RunnerContainerName {
    const NAME: &'static str = "ENSO_RUNNER_CONTAINER_NAME";
    type Value = ContainerId;
}

#[derive(Clone, Copy, Debug)]
pub struct NightlyEditionsLimit;
impl Variable for NightlyEditionsLimit {
    const NAME: &'static str = "ENSO_NIGHTLY_EDITIONS_LIMIT";
    type Value = usize;
}
