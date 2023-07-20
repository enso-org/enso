use crate::prelude::*;

use crate::cache::goodie;
use crate::cache::goodie::Goodie;
use crate::cache::Cache;
use crate::env::known::PATH;
use crate::github::RepoRef;
use crate::programs::java::JAVA_HOME;
use crate::programs::Java;



const PACKAGE_PREFIX_URL: &str = "graalvm-community";

pub const CE_BUILDS_REPOSITORY: RepoRef = RepoRef { owner: "graalvm", name: "graalvm-ce-builds" };

crate::define_env_var! {
    /// Should be the same as `JAVA_HOME` for Graal-based Java distribution.
    ///
    /// Note that this is not the root directory of the GraalVM installation (at least on macOS),
    /// but the directory where the `bin` directory is located.
    GRAALVM_HOME, PathBuf;
}

pub fn graal_version_from_version_string(version_string: &str) -> Result<Version> {
    let line = version_string.lines().find(|line| line.contains("GraalVM CE")).context(
        "There is a Java environment available but it is not recognizable as GraalVM one.",
    )?;
    Version::find_in_text(line)
}

pub async fn find_graal_version() -> Result<Version> {
    let text = Java.version_string().await?;
    graal_version_from_version_string(&text)
}

/// Description necessary to download and install GraalVM.
#[derive(Clone, Debug)]
pub struct GraalVM {
    /// Used to query GitHub about releases.
    pub client:        Octocrab,
    pub graal_version: Version,
    pub os:            OS,
    pub arch:          Arch,
}

impl Goodie for GraalVM {
    fn get(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        goodie::download_try_future_url(self.url(), cache)
    }


    fn is_active(&self) -> BoxFuture<'static, Result<bool>> {
        let expected_graal_version = self.graal_version.clone();
        async move {
            let found_version = find_graal_version().await?;
            ensure!(found_version == expected_graal_version, "GraalVM version mismatch. Expected {expected_graal_version}, found {found_version}.");
            Result::Ok(true)
        }
        .boxed()
    }

    fn activation_env_changes(&self, package_path: &Path) -> Result<Vec<crate::env::Modification>> {
        let dir_entries = package_path
            .read_dir()
            .context("Failed to read GraalVM cache directory")?
            .collect_vec();
        let [graalvm_dir] = dir_entries.as_slice() else {
            bail!("GraalVM cache directory should contain exactly one directory");
        };
        let graalvm_dir = match graalvm_dir {
            Ok(dir_entry) => dir_entry,
            Err(err) => bail!("Failed to read GraalVM cache directory: {}", err),
        };
        let dir_name = graalvm_dir.file_name();
        let dir_name = dir_name.as_str();
        ensure!(dir_name.contains("graalvm"));
        ensure!(dir_name.contains(self.graal_version.to_string_core().as_str()));
        let root = match TARGET_OS {
            OS::MacOS => graalvm_dir.path().join_iter(["Contents", "Home"]),
            _ => graalvm_dir.path(),
        };
        Ok(vec![
            crate::env::Modification::set(&JAVA_HOME, &root)?,
            crate::env::Modification::set(&GRAALVM_HOME, &root)?,
            crate::env::Modification::prepend_path(&PATH, root.join("bin")),
        ])
    }
}

impl GraalVM {
    pub fn url(&self) -> BoxFuture<'static, Result<Url>> {
        let platform_string = self.platform_string();
        let graal_version_tag = self.graal_version.to_string_core();
        let client = self.client.clone();
        async move {
            let repo = CE_BUILDS_REPOSITORY.handle(&client);
            let release = repo.find_release_by_text(&graal_version_tag).await?;
            crate::github::find_asset_url_by_text(&release, &platform_string).cloned()
        }
        .boxed()
    }

    pub fn platform_string(&self) -> String {
        let Self { graal_version: _graal_version, arch, os, client: _client } = &self;
        let os_name = match *os {
            OS::Linux => "linux",
            OS::Windows => "windows",
            OS::MacOS => "darwin",
            other_os => unimplemented!("System `{}` is not supported!", other_os),
        };
        let arch_name = match *arch {
            Arch::X86_64 => "x64",
            // No Graal packages for Apple Silicon.
            Arch::AArch64 if TARGET_OS == OS::MacOS => "x64",
            Arch::AArch64 => "aarch64",
            other_arch => unimplemented!("Architecture `{}` is not supported!", other_arch),
        };
        let java_version = format!("jdk-{}", _graal_version.to_string_core());
        format!("{PACKAGE_PREFIX_URL}-{java_version}_{os_name}-{arch_name}")
    }
}

/// Locate the directory with GraalVM installation.
///
/// It is deduced based on [`JAVA_HOME`] environment variable. Exact logic is os-specific.
#[context("Failed to locate GraalVM installation.")]
pub fn locate_graal() -> Result<PathBuf> {
    let java_home = JAVA_HOME.get()?;
    Ok(if TARGET_OS == OS::MacOS {
        // On macOS we need to drop trailing `/Contents/Home` from the path.
        java_home.try_parent()?.try_parent()?.to_path_buf()
    } else {
        java_home
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cache;
    use crate::log::setup_logging;
    use crate::programs::graal::Gu;
    use crate::programs::Java;
    use semver::BuildMetadata;
    use semver::Prerelease;

    #[tokio::test]
    #[ignore]
    async fn test_is_enabled() -> Result {
        setup_logging()?;
        let graal_version = Version::parse("17.0.7+7.1").unwrap();
        let os = TARGET_OS;
        let arch = Arch::X86_64;
        let client = Octocrab::default();
        let graalvm = GraalVM { graal_version, os, arch, client };

        graalvm.install_if_missing(&cache::Cache::new_default().await?).await?;

        Gu.require_present().await?;

        // let graalvm = graalvm.is_active().await?;
        // assert!(graalvm);
        Ok(())
    }

    /// Check that we correctly recognize both the GraalVM version and the Java version.
    #[test]
    fn version_recognize() {
        let version_string = r#"openjdk version "17.0.7" 2023-04-18
OpenJDK Runtime Environment GraalVM CE 17.0.7+7.1 (build 17.0.7+7-jvmci-23.0-b12)
OpenJDK 64-Bit Server VM GraalVM CE 17.0.7+7.1 (build 17.0.7+7-jvmci-23.0-b12, mixed mode, sharing)"#;

        let found_graal = graal_version_from_version_string(version_string).unwrap();
        let expected_graal_version = Version {
            major: 17,
            minor: 0,
            patch: 7,
            pre:   Prerelease::EMPTY,
            build: BuildMetadata::new("7.1").unwrap(),
        };
        assert_eq!(found_graal, expected_graal_version);

        let found_java = Java.parse_version(version_string).unwrap();
        assert_eq!(found_java, Version::new(17, 0, 7));
    }

    #[test]
    fn recognize_oneline_version() {
        let version_line =
            "OpenJDK Runtime Environment GraalVM CE 17.0.7+7.1 (build 17.0.7+7-jvmci-23.0-b12)";
        let graal_version = Version::find_in_text(version_line).unwrap();
        let expected_graal_version = Version {
            major: 17,
            minor: 0,
            patch: 7,
            pre:   Prerelease::EMPTY,
            build: BuildMetadata::new("7.1").unwrap(),
        };
        assert_eq!(graal_version, expected_graal_version);
    }
}
