use crate::prelude::*;

use crate::version::Versions;

use std::env::consts::EXE_EXTENSION;
use std::env::consts::EXE_SUFFIX;
use std::fmt::Formatter;



#[allow(clippy::all)] // [mwu] Little reason to bother in the generated code.
pub mod generated {
    include!(concat!(env!("OUT_DIR"), "/paths.rs"));
}

ide_ci::define_env_var! {
    /// Directory where JUnit-format test run results are stored.
    /// These are generated as part of the standard library test suite run.
    ENSO_TEST_JUNIT_DIR, PathBuf;

    /// Used to overwrite the default location of data directory. See:
    /// <https://enso.org/docs/developer/enso/distribution/distribution.html#installed-enso-distribution-layout>.
    ENSO_DATA_DIRECTORY, PathBuf;

    /// Path to the engine runner executable.
    /// See: <https://github.com/diab0l/enso/blob/feature/test_with_clue/test/Meta_Test_Suite_Tests/README.md>
    ENSO_META_TEST_COMMAND, PathBuf;

    /// Arguments to the engine runner.
    /// See: <https://github.com/diab0l/enso/blob/feature/test_with_clue/test/Meta_Test_Suite_Tests/README.md>
    ENSO_META_TEST_ARGS, String;

    /// If Enso-specific assertions should be enabled.
    ENSO_ENABLE_ASSERTIONS, String;

    /// Can be set to `"espresso"` to enable Espresso interpreter support.
    ENSO_JAVA, String;
}

pub const EDITION_FILE_ARTIFACT_NAME: &str = "Edition File";

pub const LIBRARIES_TO_TEST: [&str; 7] = [
    "Examples_Tests",
    "Geo_Tests",
    "Image_Tests",
    // Temporarily disabled due to https://www.pivotaltracker.com/story/show/184042416
    // "Meta_Test_Suite_Tests",
    "Table_Tests",
    "Base_Tests",
    "AWS_Tests",
    "Visualization_Tests",
];

pub fn new_repo_root(repo_root: impl Into<PathBuf>, triple: &TargetTriple) -> generated::RepoRoot {
    generated::RepoRoot::new_root(
        repo_root,
        triple.versions.edition_name(),
        EXE_SUFFIX,
        triple.to_string(),
        triple.versions.version.to_string(),
    )
}

pub fn pretty_print_arch(arch: Arch) -> &'static str {
    match arch {
        Arch::X86_64 => "amd64",
        Arch::AArch64 => "aarch64",
        _ => panic!("Unrecognized architecture {arch}"),
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct TargetTriple {
    pub os:       OS,
    pub arch:     Arch,
    pub versions: Versions,
}

impl TargetTriple {
    /// Create a new triple with OS and architecture are inferred from the hosting system.
    pub fn new(versions: Versions) -> Self {
        Self { os: TARGET_OS, arch: TARGET_ARCH, versions }
    }

    /// Get the triple effectively used by the Engine build.
    ///
    /// As the GraalVM we use does not support native Aarch64 builds, it should be treated as amd64
    /// there.
    pub fn engine(&self) -> Self {
        let mut ret = self.clone();
        ret.arch = if self.arch == Arch::AArch64 && self.os == OS::MacOS {
            Arch::X86_64
        } else {
            self.arch
        };
        ret
    }

    /// Pretty prints architecture for our packages. Conform to GraalVM scheme as well.
    pub fn arch(&self) -> &'static str {
        pretty_print_arch(self.arch)
    }
}

impl Display for TargetTriple {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}-{}", self.versions.version, self.os, self.arch())
    }
}

#[derive(Clone, Debug)]
pub struct Paths {
    pub repo_root: generated::RepoRoot,
    // pub launcher:        ComponentPaths,
    // pub engine:          ComponentPaths,
    // pub project_manager: ComponentPaths,
    pub triple:    TargetTriple,
}

impl Paths {
    pub fn distribution(&self) -> PathBuf {
        self.repo_root.join("distribution")
    }

    /// Create a new set of paths for building the Enso with a given version number.
    pub fn new_triple(repo_root: impl Into<PathBuf>, triple: TargetTriple) -> Result<Self> {
        let repo_root: PathBuf = repo_root.into().absolutize()?.into();
        let repo_root = new_repo_root(repo_root, &triple);
        Ok(Paths { repo_root, triple })
    }

    /// Create a new set of paths for building the Enso with a given version number.
    pub fn new_versions(repo_root: impl Into<PathBuf>, versions: Versions) -> Result<Self> {
        let triple = TargetTriple::new(versions);
        Self::new_triple(repo_root, triple)
    }

    /// Create a new set of paths for building the Enso with a given version number.
    pub fn new_version(repo_root: impl Into<PathBuf>, version: Version) -> Result<Self> {
        let versions = Versions::new(version);
        Self::new_versions(repo_root, versions)
    }

    /// Sets the environment variables in the current process and in GitHub Actions Runner (if being
    /// run in its environment), so future steps of the job also have access to them.
    pub async fn emit_env_to_actions(&self) -> Result {
        // TODO [mwu]: Check if TARGET_DIR is needed. If so, create a strongly typed wrapper.
        // ide_ci::actions::workflow::set_env("TARGET_DIR", &self.repo_root.target).await?;
        ENSO_TEST_JUNIT_DIR.set_workflow_env(self.repo_root.target.test_results.as_ref()).await?;
        Ok(())
    }

    pub fn stdlib_tests(&self) -> PathBuf {
        self.repo_root.join("test")
    }

    pub fn stdlib_test(&self, test_name: impl AsRef<Path>) -> PathBuf {
        self.stdlib_tests().join(test_name)
    }

    pub fn changelog(&self) -> PathBuf {
        root_to_changelog(&self.repo_root)
    }

    pub fn edition_name(&self) -> String {
        self.triple.versions.edition_name()
    }

    pub fn manifest_file(&self) -> PathBuf {
        self.repo_root
            .built_distribution
            .enso_engine_triple
            .engine_package
            .manifest_yaml
            .to_path_buf()
    }

    pub fn launcher_manifest_file(&self) -> PathBuf {
        self.repo_root.distribution.launcher_manifest_yaml.to_path_buf()
    }

    // e.g. enso2\distribution\editions\2021.20-SNAPSHOT.yaml
    pub fn edition_file(&self) -> PathBuf {
        self.repo_root.distribution.editions.edition_yaml.to_path_buf()
    }

    pub async fn upload_edition_file_artifact(&self) -> Result {
        ide_ci::actions::artifacts::upload_single_file(
            self.edition_file(),
            EDITION_FILE_ARTIFACT_NAME,
        )
        .await
    }

    pub async fn download_edition_file_artifact(&self) -> Result {
        ide_ci::actions::artifacts::download_single_file_artifact(
            EDITION_FILE_ARTIFACT_NAME,
            self.edition_file(),
        )
        .await
    }

    pub fn version(&self) -> &Version {
        &self.triple.versions.version
    }
}

pub fn root_to_changelog(root: impl AsRef<Path>) -> PathBuf {
    let changelog_filename = "CHANGELOG.md";
    let root_path = root.as_ref().join(changelog_filename);
    // TODO: transitional code to support both locations of the changelog
    //       only the root one should prevail
    if root_path.exists() {
        root_path
    } else {
        root.as_ref().join_iter(["app", "gui", changelog_filename])
    }
}

/// The default value of `ENSO_DATA_DIRECTORY`.
/// See: <https://enso.org/docs/developer/enso/distribution/distribution.html#installed-enso-distribution-layout>
///
/// We use it as a fallback when the environment variable is not set.
pub fn default_data_directory() -> PathBuf {
    let project_path = match TARGET_OS {
        OS::MacOS => "org.enso",
        _ => "enso",
    };
    // We can unwrap, because all systems we target define data local directory.
    // This is enforced by the unit test below.
    dirs::data_local_dir().unwrap().join(project_path)
}

/// Get the `ENSO_DATA_DIRECTORY` path.
pub fn data_directory() -> PathBuf {
    ENSO_DATA_DIRECTORY.get().unwrap_or_else(|_| default_data_directory())
}

/// Get the place where global IR caches are stored.
pub fn cache_directory() -> PathBuf {
    data_directory().join("cache")
}

pub fn project_manager(base_path: impl AsRef<Path>) -> PathBuf {
    base_path
        .as_ref()
        .join_iter(["enso", "bin", "project-manager"])
        .with_appended_extension(EXE_EXTENSION)
}

/// The path to the first `Cargo.toml` above the given path.
pub fn parent_cargo_toml(initial_path: impl AsRef<Path>) -> Result<PathBuf> {
    let mut path = initial_path.as_ref().to_path_buf();
    loop {
        path.push("Cargo.toml");
        if path.exists() {
            return Ok(path);
        }
        path.pop();
        ensure!(path.pop(), "No Cargo.toml found for {}", initial_path.as_ref().display());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_data_directory_is_present() {
        // We just check that the function does not panic, as it has unwrap.
        default_data_directory();
    }
}
