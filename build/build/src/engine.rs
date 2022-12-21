//! Top-level module for dealing with Enso Engine and its components.
//!
//! The reasonable entry point is the [`context::RunContext`] type.

use crate::prelude::*;

use crate::get_graal_version;
use crate::get_java_major_version;
use crate::paths::generated;
use crate::paths::ComponentPaths;
use crate::paths::Paths;

use ide_ci::future::AsyncPolicy;
use ide_ci::github::Repo;
use std::collections::BTreeSet;


// ==============
// === Export ===
// ==============

pub mod bundle;
pub mod context;
pub mod env;
pub mod sbt;

pub use context::RunContext;



/// Version of `flatc` (the FlatBuffers compiler) that we require.
const FLATC_VERSION: Version = Version::new(1, 12, 0);

/// Whether pure Enso tests should be run in parallel.
const PARALLEL_ENSO_TESTS: AsyncPolicy = AsyncPolicy::Sequential;

/// Download template projects from GitHub.
pub async fn download_project_templates(client: reqwest::Client, enso_root: PathBuf) -> Result {
    // Download Project Template Files
    let output_base = enso_root.join("lib/scala/pkg/src/main/resources/");
    let url_base = Url::parse("https://github.com/enso-org/project-templates/raw/main/")?;
    let to_handle = [
        ("Orders", vec!["data/store_data.xlsx", "src/Main.enso"]),
        ("Restaurants", vec!["data/la_districts.csv", "data/restaurants.csv", "src/Main.enso"]),
        ("Stargazers", vec!["src/Main.enso"]),
    ];

    let mut futures = Vec::<BoxFuture<'static, Result>>::new();
    for (project_name, relative_paths) in to_handle {
        for relative_path in relative_paths {
            let relative_url_base = url_base.join(&format!("{}/", project_name))?;
            let relative_output_base = output_base.join(project_name.to_lowercase());
            let client = client.clone();
            let future = async move {
                ide_ci::io::web::client::download_relative(
                    &client,
                    &relative_url_base,
                    &relative_output_base,
                    &PathBuf::from(relative_path),
                )
                .await?;
                Ok(())
            };
            futures.push(future.boxed());
        }
    }

    let _result = ide_ci::future::try_join_all(futures, AsyncPolicy::FutureParallelism).await?;
    debug!("Completed downloading templates");
    Ok(())
}

/// Describe, which benchmarks should be run.
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, PartialOrd, Ord, clap::ArgEnum)]
pub enum Benchmarks {
    /// Run all SBT-exposed benchmarks. Does *not* including pure [`Benchmarks::Enso`] benchmarks.
    All,
    /// Run the runtime benchmark (from `sbt`).
    Runtime,
    /// Run benchmarks written in pure Enso.
    Enso,
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, PartialOrd, Ord, clap::ArgEnum)]
pub enum Tests {
    Scala,
    #[clap(alias = "stdlib")]
    StandardLibrary,
}

impl Benchmarks {
    pub fn sbt_task(self) -> Option<&'static str> {
        match self {
            Benchmarks::All => Some("bench"),
            Benchmarks::Runtime => Some("runtime/bench"),
            Benchmarks::Enso => None,
        }
    }
}

/// Describes what should be done with the backend.
///
/// Basically a recipe of what to do with `sbt` and its artifacts.
#[derive(Clone, Debug)]
pub struct BuildConfigurationFlags {
    /// If true, repository shall be cleaned at the build start.
    ///
    /// Makes sense given that incremental builds with SBT are currently broken.
    pub test_scala:                    bool,
    /// Whether the Enso standard library should be tested.
    pub test_standard_library:         bool,
    /// Whether benchmarks are compiled.
    ///
    /// Note that this does not run the benchmarks, only ensures that they are buildable.
    pub build_benchmarks:              bool,
    /// Whether the Enso-written benchmarks should be checked whether they compile.
    ///
    /// Note that this does not benchmark, only ensures that they are buildable.
    /// Also, this does nothing if `execute_benchmarks` contains `Benchmarks::Enso`.
    pub check_enso_benchmarks:         bool,
    /// Which benchmarks should be run.
    pub execute_benchmarks:            BTreeSet<Benchmarks>,
    /// Used to check that benchmarks do not fail on runtime, rather than obtaining the results.
    pub execute_benchmarks_once:       bool,
    /// Whether the Scala-based parser should be compiled into JS.
    pub build_js_parser:               bool,
    pub build_engine_package:          bool,
    pub build_launcher_package:        bool,
    pub build_project_manager_package: bool,
    pub build_launcher_bundle:         bool,
    pub build_project_manager_bundle:  bool,
    pub generate_java_from_rust:       bool,
    pub test_java_generated_from_rust: bool,
    pub generate_documentation:        bool,
    /// Verify License Packages in Distributions.
    pub verify_packages:               bool,
}

impl From<BuildConfigurationFlags> for BuildConfigurationResolved {
    fn from(value: BuildConfigurationFlags) -> Self {
        Self::new(value)
    }
}

#[derive(Clone, Debug, Shrinkwrap)]
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

        // Check for components that require Enso Engine runner. Basically everything that needs to
        // run pure Enso code.
        if config.test_standard_library
            || config.execute_benchmarks.contains(&Benchmarks::Enso)
            || config.check_enso_benchmarks
        {
            config.build_engine_package = true;
        }

        // If we are about to run pure Enso benchmarks, there is no reason to try them in dry run.
        if config.execute_benchmarks.contains(&Benchmarks::Enso) {
            config.check_enso_benchmarks = false;
        }

        if config.test_java_generated_from_rust {
            config.generate_java_from_rust = true;
        }

        Self(config)
    }
}

impl BuildConfigurationFlags {
    pub fn build_engine_package(&self) -> bool {
        self.build_engine_package
            || self.build_launcher_bundle
            || self.build_project_manager_bundle
            || self.test_standard_library
    }

    pub fn build_project_manager_package(&self) -> bool {
        self.build_project_manager_package || self.build_project_manager_bundle
    }

    pub fn build_launcher_package(&self) -> bool {
        self.build_launcher_package || self.build_launcher_bundle
    }
}

impl Default for BuildConfigurationFlags {
    fn default() -> Self {
        Self {
            test_scala:                    false,
            test_standard_library:         false,
            build_benchmarks:              false,
            check_enso_benchmarks:         false,
            execute_benchmarks:            default(),
            execute_benchmarks_once:       false,
            build_js_parser:               false,
            build_engine_package:          false,
            build_launcher_package:        false,
            build_project_manager_package: false,
            build_launcher_bundle:         false,
            build_project_manager_bundle:  false,
            generate_java_from_rust:       true,
            test_java_generated_from_rust: false,
            generate_documentation:        false,
            verify_packages:               false,
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
    Build,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct BuiltArtifacts {
    pub packages: BuiltPackageArtifacts,
    pub bundles:  BuiltBundleArtifacts,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct BuiltPackageArtifacts {
    pub engine:          Option<ComponentPaths>,
    pub launcher:        Option<ComponentPaths>,
    pub project_manager: Option<ComponentPaths>,
}

impl BuiltPackageArtifacts {
    pub fn iter(&self) -> impl IntoIterator<Item = &ComponentPaths> {
        [&self.engine, &self.launcher, &self.project_manager].into_iter().flat_map(|b| b.iter())
    }
}

impl IntoIterator for BuiltPackageArtifacts {
    type Item = ComponentPaths;
    type IntoIter = std::iter::Flatten<std::array::IntoIter<Option<ComponentPaths>, 3_usize>>;

    fn into_iter(self) -> Self::IntoIter {
        [self.engine, self.launcher, self.project_manager].into_iter().flatten()
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct BuiltBundleArtifacts {
    pub launcher:        Option<ComponentPaths>,
    pub project_manager: Option<ComponentPaths>,
}

impl BuiltBundleArtifacts {
    pub fn iter(&self) -> impl IntoIterator<Item = &ComponentPaths> {
        [&self.project_manager, &self.launcher].into_iter().flat_map(|b| b.iter())
    }
}

impl IntoIterator for BuiltBundleArtifacts {
    type Item = ComponentPaths;
    type IntoIter = std::iter::Flatten<std::array::IntoIter<Option<ComponentPaths>, 2_usize>>;

    fn into_iter(self) -> Self::IntoIter {
        [self.launcher, self.project_manager].into_iter().flatten()
    }
}

pub async fn create_packages(paths: &Paths) -> Result<Vec<PathBuf>> {
    let mut ret = Vec::new();
    if paths.launcher.root.exists() {
        debug!("Packaging launcher.");
        ret.push(package_component(&paths.launcher).await?);
    }
    Ok(ret)
}

#[async_trait]
trait ComponentPathExt {
    async fn pack(&self) -> Result;
    fn clear(&self) -> Result;
}

#[async_trait]
impl ComponentPathExt for ComponentPaths {
    async fn pack(&self) -> Result {
        ide_ci::archive::create(&self.artifact_archive, [&self.dir]).await
    }
    fn clear(&self) -> Result {
        ide_ci::fs::remove_dir_if_exists(&self.root)?;
        ide_ci::fs::remove_file_if_exists(&self.artifact_archive)
    }
}

pub async fn package_component(paths: &ComponentPaths) -> Result<PathBuf> {
    #[cfg(not(target_os = "windows"))]
    {
        let pattern = paths
            .dir
            .join_iter(["bin", "*"])
            .with_extension(std::env::consts::EXE_EXTENSION)
            .display()
            .to_string();
        for binary in glob::glob(&pattern)? {
            ide_ci::fs::allow_owner_execute(binary?)?;
        }
    }

    ide_ci::archive::create(&paths.artifact_archive, [&paths.root]).await?;
    Ok(paths.artifact_archive.clone())
}

//////////////////////////////////


pub async fn deduce_graal(
    client: Octocrab,
    build_sbt: &generated::RepoRootBuildSbt,
) -> Result<ide_ci::cache::goodie::graalvm::GraalVM> {
    let build_sbt_content = ide_ci::fs::tokio::read_to_string(build_sbt).await?;
    Ok(ide_ci::cache::goodie::graalvm::GraalVM {
        client,
        graal_version: get_graal_version(&build_sbt_content)?,
        java_version: get_java_major_version(&build_sbt_content)?,
        os: TARGET_OS,
        arch: TARGET_ARCH,
    })
}
