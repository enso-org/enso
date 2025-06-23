//! Top-level module for dealing with Enso Engine and its components.
//!
//! The reasonable entry point is the [`context::RunContext`] type.

use crate::prelude::*;

use crate::engine::bundle::GraalVmVersion;
use crate::get_flatbuffers_version;
use crate::get_graal_packages_version;
use crate::get_graal_version;
use crate::paths::generated;

use artifact::IsArtifact;
use bundle::IsBundle;
use ide_ci::cache::goodie::graalvm::Edition;
use ide_ci::future::AsyncPolicy;
use ide_ci::github::Repo;
use package::IsPackage;


// ==============
// === Export ===
// ==============

pub mod artifact;
pub mod bundle;
pub mod context;
pub mod env;
pub mod package;
pub mod sbt;

pub use context::RunContext;

/// Whether pure Enso tests should be run in parallel.
const PARALLEL_ENSO_TESTS: AsyncPolicy = AsyncPolicy::Sequential;

/// Describe, which benchmarks should be run.
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum, Default)]
pub enum BenchmarkType {
    /// Run all SBT-exposed benchmarks. Does *not* including pure [`Benchmarks::Enso`] benchmarks.
    #[default]
    All,
    /// Run the runtime benchmark (from `sbt`).
    Runtime,
    /// Run benchmarks written in pure Enso.
    Enso,
    /// Run Enso benchmarks via JMH
    EnsoJMH,
}

#[derive(Clone, Debug, Default)]
pub struct Benchmarks {
    /// Name of a single benchmark, if a single benchmark should be run.
    /// If None, all the benchmarks of the given type are executed
    pub bench_name: Option<String>,
    pub bench_type: BenchmarkType,
}

impl Benchmarks {
    fn sbt_task(&self) -> Option<String> {
        match &self.bench_type {
            BenchmarkType::All => Some("bench".to_string()),
            BenchmarkType::Runtime => match &self.bench_name {
                Some(name) if !name.is_empty() =>
                    Some(format!("runtime-benchmarks/benchOnly {}", name)),
                _ => Some("runtime-benchmarks/bench".to_string()),
            },
            BenchmarkType::Enso => None,
            BenchmarkType::EnsoJMH => match &self.bench_name {
                Some(name) if !name.is_empty() =>
                    Some(format!("std-benchmarks/benchOnly {}", name)),
                _ => Some("std-benchmarks/bench".to_string()),
            },
        }
    }
}


#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum)]
pub enum Tests {
    /// Run the JVM tests.
    Jvm,

    /// Run all Standard Library tests.
    #[clap(alias = "stdlib")]
    StandardLibrary,

    /// Run all Standard Library tests with native-image.
    #[clap(alias = "stdlib-native")]
    StandardLibraryInNative,

    /// Run the Snowflake tests.
    StdSnowflake,

    /// Run a subset of Standard Library tests that deals with Cloud-related functionality.
    StdCloudRelated,

    /// Run Microsoft tests.
    StdMicrosoft,
}

/// Configuration for how the binary inside the engine distribution should be built.
#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub enum EngineLauncher {
    /// The binary inside the engine distribution will be built as an optimized native image
    Native,
    /// The binary inside the engine distribution will be built as native image with assertions
    /// enabled but no debug information
    TestNative,
    /// The binary inside the engine distribution will be built as native image with assertions
    /// enabled and debug information
    TestDebugNative,
    /// The binary inside the engine distribution will be a shell script
    #[default]
    Shell,
}

impl FromStr for EngineLauncher {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        bail!("Parsing of ENSO_LAUNCHER isn't needed: {}", s)
    }
}

impl Display for EngineLauncher {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let str = match self {
            EngineLauncher::Native => "native".to_string(),
            EngineLauncher::TestNative => "native,test".to_string(),
            EngineLauncher::TestDebugNative => "native,test,debug".to_string(),
            EngineLauncher::Shell => "shell".to_string(),
        };

        write!(f, "{}", str)
    }
}

impl From<EngineLauncher> for String {
    fn from(value: EngineLauncher) -> Self {
        value.to_string()
    }
}

/// Describes what should be done with the backend.
///
/// Basically a recipe of what to do with `sbt` and its artifacts.
#[derive(Clone, Debug)]
pub struct BuildConfigurationFlags {
    /// Run JVM tests.
    pub test_jvm: bool,
    /// Whether the Enso standard library should be tested.
    pub test_standard_library: Option<StandardLibraryTestsSelection>,
    pub extra_engine_runner_args: Option<Vec<String>>,
    /// Whether benchmarks are compiled.
    ///
    /// Note that this does not run the benchmarks, only ensures that they are buildable.
    pub build_benchmarks: bool,
    /// Whether the small JDK should be build with `jlink` before running
    /// any tests.
    pub build_small_jdk: bool,
    /// If `build_small_jdk` is true, this is the directory where the small JDK will be placed.
    pub small_jdk_dir: Option<PathBuf>,
    /// Whether the Enso-written benchmarks should be checked whether they compile.
    ///
    /// Note that this does not run benchmark, only ensures that they are buildable.
    /// Also, this does nothing if `execute_benchmarks` contains `Benchmarks::Enso`.
    pub check_enso_benchmarks: bool,
    /// Which benchmarks should be run.
    pub execute_benchmarks: Option<Benchmarks>,
    /// Used to check that benchmarks do not fail on runtime, rather than obtaining the results.
    pub execute_benchmarks_once: bool,
    pub build_engine_package: bool,
    /// Use the NI Engine Runner during the build.
    pub use_native_runner: bool,
    /// Build the NI Engine Runner.
    pub build_native_runner: bool,
    /// Build the Ydoc Native Image
    pub build_native_ydoc: bool,
    pub build_launcher_package: bool,
    pub build_project_manager_package: bool,
    pub build_launcher_bundle: bool,
    pub build_project_manager_bundle: bool,
    pub generate_java_from_rust: bool,
    pub test_java_generated_from_rust: bool,
    /// Verify License Packages in Distributions.
    pub verify_packages: bool,
    pub stdlib_api_check: bool,
    pub run_enso_lint: bool,
}

#[derive(Clone, Debug)]
pub enum Filter<T> {
    Whitelist(HashSet<T>),
    Blacklist(HashSet<T>),
}

impl<T> Filter<T>
where T: Eq + Hash
{
    pub fn whitelist(items: impl IntoIterator<Item = T>) -> Self {
        Self::Whitelist(items.into_iter().collect())
    }

    pub fn blacklist(items: impl IntoIterator<Item = T>) -> Self {
        Self::Blacklist(items.into_iter().collect())
    }

    pub fn allow(&mut self, item: T) {
        match self {
            Self::Whitelist(ref mut set) => {
                set.insert(item);
            }
            Self::Blacklist(ref mut set) => {
                set.remove(&item);
            }
        }
    }

    pub fn deny(&mut self, item: T) {
        match self {
            Self::Whitelist(ref mut set) => {
                set.remove(&item);
            }
            Self::Blacklist(ref mut set) => {
                set.insert(item);
            }
        }
    }

    pub fn is_allowed(&self, item: &T) -> bool {
        match self {
            Self::Whitelist(set) => set.contains(item),
            Self::Blacklist(set) => !set.contains(item),
        }
    }
}

pub type StandardLibraryTestsSelection = Filter<String>;

impl From<BuildConfigurationFlags> for BuildConfigurationResolved {
    fn from(value: BuildConfigurationFlags) -> Self {
        Self::new(value)
    }
}

#[derive(Clone, Debug, Deref)]
pub struct BuildConfigurationResolved(BuildConfigurationFlags);

impl BuildConfigurationResolved {
    pub fn new(mut config: BuildConfigurationFlags) -> Self {
        if config.build_launcher_bundle {
            config.build_launcher_package = true;
            config.build_engine_package = true;
        }

        if config.build_project_manager_bundle {
            config.build_project_manager_package = true;
            config.build_engine_package = true;
        }

        // If we are about to run pure Enso benchmarks, there is no reason to try them in dry run.
        if Self::should_run_enso_benchmarks(&config) {
            config.check_enso_benchmarks = false;
        }

        if config.test_java_generated_from_rust {
            config.generate_java_from_rust = true;
        }

        Self(config)
    }

    fn should_run_enso_benchmarks(config: &BuildConfigurationFlags) -> bool {
        match &config.execute_benchmarks {
            Some(benchmark) => benchmark.bench_type == BenchmarkType::Enso,
            None => false,
        }
    }
}

impl BuildConfigurationFlags {
    pub fn has_native_runner(&self) -> bool {
        self.build_native_runner || self.use_native_runner
    }

    pub fn build_project_manager_package(&self) -> bool {
        self.build_project_manager_package || self.build_project_manager_bundle
    }

    pub fn build_launcher_package(&self) -> bool {
        self.build_launcher_package || self.build_launcher_bundle
    }

    pub fn add_engine_runner_arg(&mut self, flag: &str) {
        if let Some(engine_runner_args) = &mut self.extra_engine_runner_args {
            engine_runner_args.push(flag.into());
        } else {
            self.extra_engine_runner_args = Some(vec![flag.into()]);
        }
    }
}

impl Default for BuildConfigurationFlags {
    fn default() -> Self {
        Self {
            test_jvm: false,
            test_standard_library: None,
            extra_engine_runner_args: None,
            build_small_jdk: false,
            small_jdk_dir: None,
            build_benchmarks: false,
            check_enso_benchmarks: false,
            execute_benchmarks: default(),
            execute_benchmarks_once: false,
            build_engine_package: false,
            build_launcher_package: false,
            use_native_runner: false,
            build_native_runner: false,
            build_native_ydoc: false,
            build_project_manager_package: false,
            build_launcher_bundle: false,
            build_project_manager_bundle: false,
            generate_java_from_rust: false,
            test_java_generated_from_rust: false,
            verify_packages: false,
            stdlib_api_check: false,
            run_enso_lint: false,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ReleaseCommand {
    Upload,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ReleaseOperation {
    pub command: ReleaseCommand,
    pub repo:    Repo,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RunOperation {
    pub command_pieces: Vec<OsString>,
}

impl RunOperation {}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Operation {
    Release(ReleaseOperation),
    Run(RunOperation),
    Sbt(Vec<String>),
    Build,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct BuiltArtifacts {
    pub engine_package:          Option<generated::EnginePackage>,
    pub launcher_package:        Option<generated::LauncherPackage>,
    pub project_manager_package: Option<generated::ProjectManagerPackage>,
    pub launcher_bundle:         Option<generated::LauncherBundle>,
    pub project_manager_bundle:  Option<generated::ProjectManagerBundle>,
}

impl BuiltArtifacts {
    pub fn packages(&self) -> Vec<&dyn IsPackage> {
        let mut packages = Vec::<&dyn IsPackage>::new();
        if let Some(engine) = &self.engine_package {
            packages.push(engine);
        }
        if let Some(launcher) = &self.launcher_package {
            packages.push(launcher);
        }
        if let Some(project_manager) = &self.project_manager_package {
            packages.push(project_manager);
        }
        packages
    }

    pub fn bundles(&self) -> Vec<&dyn IsBundle> {
        let mut bundles = Vec::<&dyn IsBundle>::new();
        if let Some(launcher) = &self.launcher_bundle {
            bundles.push(launcher);
        }
        if let Some(project_manager) = &self.project_manager_bundle {
            bundles.push(project_manager);
        }
        bundles
    }

    pub fn artifacts(&self) -> Vec<&dyn IsArtifact> {
        let mut artifacts = Vec::<&dyn IsArtifact>::new();
        for package in self.packages() {
            artifacts.push(package.as_dyn_artifact());
        }
        for bundle in self.bundles() {
            artifacts.push(bundle.as_dyn_artifact());
        }
        artifacts
    }
}

pub async fn deduce_graal(
    client: Octocrab,
    build_sbt: &generated::RepoRootBuildSbt,
) -> Result<ide_ci::cache::goodie::graalvm::GraalVM> {
    let build_sbt_content = ide_ci::fs::tokio::read_to_string(build_sbt).await?;
    let graal_edition = env::GRAAL_EDITION.get().map_or(Edition::default(), |e| e);

    Ok(ide_ci::cache::goodie::graalvm::GraalVM {
        client,
        graal_version: get_graal_version(&build_sbt_content)?,
        edition: graal_edition,
        os: TARGET_OS,
        arch: TARGET_ARCH,
    })
}

pub async fn deduce_graal_bundle(
    build_sbt: &generated::RepoRootBuildSbt,
) -> Result<GraalVmVersion> {
    let build_sbt_content = ide_ci::fs::tokio::read_to_string(build_sbt).await?;
    Ok(GraalVmVersion {
        graal:    get_graal_version(&build_sbt_content)?,
        packages: get_graal_packages_version(&build_sbt_content)?,
    })
}

/// Version of `flatc` (the FlatBuffers compiler) that Engine requires.
pub async fn deduce_flatbuffers(build_sbt: &generated::RepoRootBuildSbt) -> Result<Version> {
    let build_sbt_content = ide_ci::fs::tokio::read_to_string(build_sbt).await?;
    get_flatbuffers_version(&build_sbt_content)
}
