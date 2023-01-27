use enso_build::prelude::*;

use clap::Arg;
use clap::ArgEnum;
use clap::Args;
use clap::Parser;
use clap::Subcommand;
use derivative::Derivative;
use enso_build_base::extensions::path::display_fmt;
use ide_ci::cache;
use ide_ci::github::Repo;
use octocrab::models::RunId;


// ==============
// === Export ===
// ==============

pub mod backend;
pub mod engine;
pub mod git_clean;
pub mod gui;
pub mod ide;
pub mod java_gen;
pub mod project_manager;
pub mod release;
pub mod runtime;
pub mod wasm;



/// The prefix that will be used when reading the build script arguments from environment.
pub const ENVIRONMENT_VARIABLE_NAME_PREFIX: &str = "ENSO_BUILD";

pub const DEFAULT_REMOTE_REPOSITORY_FALLBACK: &str = "enso-org/enso";

pub fn default_repo_path() -> Option<PathBuf> {
    enso_build::repo::deduce_repository_path().ok()
}

pub fn default_repo_remote() -> Repo {
    ide_ci::actions::env::GITHUB_REPOSITORY
        .get()
        .unwrap_or_else(|_| Repo::from_str(DEFAULT_REMOTE_REPOSITORY_FALLBACK).unwrap())
}

pub fn default_cache_path() -> Option<PathBuf> {
    cache::default_path().ok()
}

/// Extensions to the `clap::Arg`, intended to be used as argument attributes.
pub trait ArgExt<'h>: Sized + 'h {
    /// Allow setting argument through an environment variable prefixed with Enso Build name.
    fn enso_env(self) -> Self;
}

impl<'h> ArgExt<'h> for Arg<'h> {
    fn enso_env(self) -> Self {
        self.prefixed_env(ENVIRONMENT_VARIABLE_NAME_PREFIX)
    }
}

/// We pass CLI paths through this to make sure that they are resolved against the initial
/// working directory, not whatever it will be set to later.
pub fn normalize_path(path: &str) -> Result<PathBuf> {
    let ret = PathBuf::from(path);
    let ret = ret.absolutize()?;
    Ok(ret.to_path_buf())
}

/// Collection of strings used by CLI that are specific to a given target.
///
/// Having a common interface to them allows reusing code for `clap`-based structures.
pub trait IsTargetSource {
    const SOURCE_NAME: &'static str;
    const PATH_NAME: &'static str;
    const OUTPUT_PATH_NAME: &'static str;
    // const UPLOAD_ASSET_NAME: &'static str;
    const RUN_ID_NAME: &'static str;
    const RELEASE_DESIGNATOR_NAME: &'static str;
    const ARTIFACT_NAME_NAME: &'static str;
    const DEFAULT_OUTPUT_PATH: &'static str;

    type BuildInput: Clone + Debug + PartialEq + Args + Send + Sync;
}

pub trait IsWatchableSource: IsTargetSource {
    type WatchInput: Clone + Debug + PartialEq + Args + Send + Sync;
}

#[macro_export]
macro_rules! source_args_hlp {
    ($target:ty, $prefix:literal, $inputs:ty) => {
        impl $crate::arg::IsTargetSource for $target {
            const SOURCE_NAME: &'static str = concat!($prefix, "-", "source");
            const PATH_NAME: &'static str = concat!($prefix, "-", "path");
            const OUTPUT_PATH_NAME: &'static str = concat!($prefix, "-", "output-path");
            // const UPLOAD_ASSET_NAME: &'static str = concat!($prefix, "-", "upload-asset");
            const RUN_ID_NAME: &'static str = concat!($prefix, "-", "run-id");
            const RELEASE_DESIGNATOR_NAME: &'static str = concat!($prefix, "-", "release");
            const ARTIFACT_NAME_NAME: &'static str = concat!($prefix, "-", "artifact-name");
            const DEFAULT_OUTPUT_PATH: &'static str = concat!("dist/", $prefix);

            type BuildInput = $inputs;
        }
    };
}

#[allow(clippy::large_enum_variant)]
#[derive(Subcommand, Clone, Debug)]
pub enum Target {
    /// Build/Test the Rust part of the GUI.
    Wasm(wasm::Target),
    /// Build/Run GUI that consists of WASM and JS parts. This is what we deploy to cloud.
    Gui(gui::Target),
    /// Enso Engine Runtime.
    Runtime(runtime::Target),
    // /// Project Manager package (just the binary, no Engine)
    // ProjectManager(project_manager::Target),
    // /// Enso Engine distribution.
    // Engine(engine::Target),
    /// Build/Get Project Manager bundle (includes Enso Engine with GraalVM Runtime).
    Backend(backend::Target),
    /// Build/Run/Test IDE bundle (includes GUI and Project Manager).
    Ide(ide::Target),
    /// Clean the repository. Keeps the IntelliJ's .idea directory intact. WARNING: This removes
    /// files that are not under version control in the repository subtree.
    GitClean(git_clean::Options),
    /// Lint the codebase.
    Lint,
    /// Apply automatic formatters on the repository.
    #[clap(alias = "format")]
    Fmt,
    /// Release-related subcommand.
    Release(release::Target),
    /// Regenerate `syntax2` library (new parser).
    JavaGen(java_gen::Target),
    /// Check if the changelog has been updated. Requires CI environment.
    ChangelogCheck,
}

/// Build, test and package Enso Engine.
#[derive(Clone, Debug, Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
    /// Path to the directory with sources to be built, typically the root of the 'enso'
    /// repository's working copy.
    #[clap(long, global = true, maybe_default_os = default_repo_path(), enso_env())]
    pub repo_path: PathBuf,

    /// Where build script will cache some of the third-party artifacts (like network downloads).
    #[clap(long, global = true, maybe_default_os = default_cache_path(), enso_env())]
    pub cache_path: PathBuf,

    /// The GitHub repository with the project. This is mainly used to manage releases (checking
    /// released versions to generate a new one, or uploading release assets).
    /// The argument should follow the format `owner/repo_name`.
    #[clap(long, global = true, default_value_t = default_repo_remote(), enso_env())]
    pub repo_remote: Repo,

    /// Platform to target. Currently cross-compilation is enabled only for GUI/IDE (without
    /// Project Manager) on platforms where Electron Builder supports this.
    #[clap(long, global = true, default_value_t = TARGET_OS, enso_env(), possible_values=[OS::Windows.as_str(), OS::Linux.as_str(), OS::MacOS.as_str()])]
    pub target_os: OS,

    /// Does not check the program version requirements defined in the build-config.yaml.
    #[clap(long, global = true, enso_env())]
    pub skip_version_check: bool,

    /// Whether built artifacts should be uploaded as part of CI run. Ignored in non-CI
    /// environment.
    #[clap(long, global = true, hide = !ide_ci::actions::workflow::is_in_env(), parse(try_from_str), default_value_t = true, enso_env())]
    pub upload_artifacts: bool,

    #[clap(subcommand)]
    pub target: Target,
}

/// Describe where to get a target artifacts from.
///
/// This is the CLI representation of a [crate::source::Source] for a given target.
#[derive(Args, Clone, Debug, PartialEq)]
pub struct Source<Target: IsTargetSource> {
    /// How the given target should be acquired.
    #[clap(name = Target::SOURCE_NAME, arg_enum, long, default_value_t= SourceKind::Build,
    enso_env(),
    default_value_if(Target::RUN_ID_NAME, None, Some("ci-run")),
    default_value_if(Target::PATH_NAME, None, Some("local")),
    default_value_if(Target::RELEASE_DESIGNATOR_NAME, None, Some("release")))]
    pub source: SourceKind,

    /// If source is `local`, this argument is used to give the path with the component.
    /// If missing, the default would-be output directory for this component shall be used.
    #[clap(name = Target::PATH_NAME, long, default_value=Target::DEFAULT_OUTPUT_PATH, enso_env())]
    pub path: PathBuf,

    /// If source is `run`, this argument is required to provide CI run ID.
    ///
    /// `GITHUB_TOKEN` environment variable with "repo" access is required to download CI run
    /// artifacts.
    #[clap(name = Target::RUN_ID_NAME, long, required_if_eq(Target::SOURCE_NAME, "ci-run"), enso_env())]
    pub run_id: Option<RunId>,

    /// Artifact name to be used when downloading a run artifact. If not set, the default name for
    /// given target will be used.
    #[clap(name = Target::ARTIFACT_NAME_NAME, long, enso_env())]
    pub artifact_name: Option<String>,

    /// If source is `release`, this argument is required to identify a release with asset to
    /// download. This can be either the release tag or a predefined placeholder (currently
    /// supported one is only 'latest').
    #[clap(name = Target::RELEASE_DESIGNATOR_NAME, long, required_if_eq(Target::SOURCE_NAME, "release"), enso_env())]
    pub release: Option<String>,

    /// Used when `SourceKind::Build` is used.
    #[clap(flatten)]
    pub build_args: Target::BuildInput,

    #[clap(flatten)]
    pub output_path: OutputPath<Target>,
    //
    // #[clap(name = Target::UPLOAD_ASSET_NAME, long)]
    // pub upload_asset: bool,
}

/// Discriminator denoting how some target artifact should be obtained.
#[derive(ArgEnum, Clone, Copy, Debug, PartialEq, Eq)]
pub enum SourceKind {
    /// Target will be built from the target repository's sources.
    Build,
    /// Already built target will be copied from the local path.
    Local,
    /// Target will be downloaded from a completed CI run artifact.
    CiRun,
    /// Target will be downloaded from the CI run that is currently executing this script.
    CurrentCiRun,
    /// Target will be downloaded from a release asset.
    Release,
}

/// Strongly typed argument for an output directory of a given build target.
#[derive(Args, Clone, Derivative)]
#[derivative(Debug, PartialEq)]
pub struct OutputPath<Target: IsTargetSource> {
    /// Directory where artifacts should be placed.
    #[derivative(Debug(format_with = "display_fmt"))]
    #[clap(name = Target::OUTPUT_PATH_NAME, long, parse(try_from_str=normalize_path), default_value = Target::DEFAULT_OUTPUT_PATH, enso_env())]
    pub output_path: PathBuf,
    #[derivative(Debug = "ignore", PartialEq(bound = ""))]
    #[allow(missing_docs)]
    #[clap(skip)]
    pub phantom:     PhantomData<Target>,
}

impl<Target: IsTargetSource> AsRef<Path> for OutputPath<Target> {
    fn as_ref(&self) -> &Path {
        self.output_path.as_path()
    }
}

#[derive(Args, Clone, PartialEq, Derivative)]
#[derivative(Debug)]
pub struct BuildJob<Target: IsTargetSource> {
    #[clap(flatten)]
    pub input:       Target::BuildInput,
    #[clap(flatten)]
    pub output_path: OutputPath<Target>,
}

#[derive(Args, Clone, PartialEq, Derivative)]
#[derivative(Debug)]
pub struct WatchJob<Target: IsWatchableSource> {
    #[clap(flatten)]
    pub build:       BuildJob<Target>,
    #[clap(flatten)]
    pub watch_input: Target::WatchInput,
}
