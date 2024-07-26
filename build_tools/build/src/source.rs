use crate::prelude::*;

use crate::project::IsTarget;
use crate::project::IsWatchable;

use derivative::Derivative;
use ide_ci::github::Repo;
use octocrab::models::AssetId;
use octocrab::models::RunId;



/// Denotes an external source from which a target artifact can be obtained.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub enum ExternalSource {
    #[derivative(Debug = "transparent")]
    OngoingCiRun(OngoingCiRunSource),
    #[derivative(Debug = "transparent")]
    CiRun(CiRunSource),
    #[derivative(Debug = "transparent")]
    LocalFile(PathBuf),
    #[derivative(Debug = "transparent")]
    Release(ReleaseSource),
}

impl ExternalSource {
    pub fn new_ongoing_ci_run(artifact_name: impl Into<String>) -> Self {
        Self::OngoingCiRun(OngoingCiRunSource { artifact_name: artifact_name.into() })
    }
}

/// Describes how to build a target and whether to upload the resulting artifact.
#[derive(Clone, Debug)]
pub struct BuildSource<Target: IsTarget> {
    /// Data needed to build the target.
    pub input:                  Target::BuildInput,
    /// Whether to upload the resulting artifact as CI artifact.
    pub should_upload_artifact: bool,
}

/// Describes how to get a target.
#[derive(Derivative)]
#[derivative(Debug)]
pub enum Source<Target: IsTarget> {
    /// Build the target locally from the sources.
    #[derivative(Debug = "transparent")]
    BuildLocally(BuildSource<Target>),
    /// Download the target from an external source.
    #[derivative(Debug = "transparent")]
    External(ExternalSource),
}

#[derive(Clone, Debug)]
pub struct OngoingCiRunSource {
    pub artifact_name: String,
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct CiRunSource {
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub repository:    Repo,
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub run_id:        RunId,
    pub artifact_name: String,
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct ReleaseSource {
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub repository: Repo,
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub asset_id:   AssetId,
}

#[derive(Clone, Debug, derive_more::Deref, derive_more::DerefMut)]
pub struct WithDestination<T> {
    #[deref]
    #[deref_mut]
    pub inner:       T,
    pub destination: PathBuf,
}

impl<T: IsTarget> WithDestination<Source<T>> {
    pub fn to_external(&self) -> Option<FetchTargetJob> {
        match &self.inner {
            Source::BuildLocally(_) => None,
            Source::External(external) => Some(WithDestination {
                inner:       external.clone(),
                destination: self.destination.clone(),
            }),
        }
    }
}

impl<T> WithDestination<T> {
    pub fn new(inner: T, destination: impl Into<PathBuf>) -> Self {
        Self { inner, destination: destination.into() }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithDestination<U> {
        WithDestination { inner: f(self.inner), destination: self.destination }
    }
}

pub type GetTargetJob<Target> = WithDestination<Source<Target>>;
pub type FetchTargetJob = WithDestination<ExternalSource>;
pub type BuildTargetJob<Target> = WithDestination<BuildSource<Target>>;

#[derive(Debug)]
pub struct WatchTargetJob<Target: IsWatchable> {
    pub build:       BuildTargetJob<Target>,
    pub watch_input: Target::WatchInput,
}

#[derive(Debug)]
pub enum FetchOrWatch<Target: IsWatchable> {
    Fetch(FetchTargetJob),
    Watch(WatchTargetJob<Target>),
}
