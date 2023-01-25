use crate::prelude::*;

use crate::cache::goodie;
use crate::cache::goodie::Goodie;
use crate::cache::Cache;
use crate::env::known::PATH;
use crate::github::RepoRef;
use crate::programs::java;
use crate::programs::java::JAVA_HOME;
use crate::programs::Java;



const PACKAGE_PREFIX: &str = "graalvm-ce";

pub const CE_BUILDS_REPOSITORY: RepoRef = RepoRef { owner: "graalvm", name: "graalvm-ce-builds" };

crate::define_env_var! {
    /// Should be the same as `JAVA_HOME` for Graal-based Java distribution.
    ///
    /// Note that this is not the root directory of the GraalVM installation (at least on macOS),
    /// but the directory where the `bin` directory is located.
    GRAALVM_HOME, PathBuf;
}

pub fn graal_version_from_version_string(version_string: &str) -> Result<Version> {
    let line = version_string.lines().find(|line| line.contains("GraalVM")).context(
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
    pub java_version:  java::LanguageVersion,
    pub os:            OS,
    pub arch:          Arch,
}

impl Goodie for GraalVM {
    fn get(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        goodie::download_try_future_url(self.url(), cache)
    }


    fn is_active(&self) -> BoxFuture<'static, Result<bool>> {
        let expected_graal_version = self.graal_version.clone();
        let expected_java_language_version = self.java_version;
        async move {
            let found_version = find_graal_version().await?;
            ensure!(found_version == expected_graal_version, "GraalVM version mismatch. Expected {expected_graal_version}, found {found_version}.");

            let found_java_version = Java.check_language_version().await?;
            ensure!(
                found_java_version == expected_java_language_version,
                "Java language version mismatch. Expected {expected_java_language_version}, found {found_java_version}."
            );

            Result::Ok(true)
        }
        .boxed()
    }

    fn activation_env_changes(&self, package_path: &Path) -> Result<Vec<crate::env::Modification>> {
        let package_path = package_path.join(self.root_directory_name());
        let root = match TARGET_OS {
            OS::MacOS => package_path.join_iter(["Contents", "Home"]),
            _ => package_path,
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
        let graal_version = self.graal_version.clone();
        let client = self.client.clone();
        async move {
            let repo = CE_BUILDS_REPOSITORY.handle(&client);
            let release = repo.find_release_by_text(&graal_version.to_string()).await?;
            crate::github::find_asset_url_by_text(&release, &platform_string).cloned()
        }
        .boxed()
    }

    pub fn platform_string(&self) -> String {
        let Self { graal_version: _graal_version, java_version, arch, os, client: _client } = &self;
        let os_name = match *os {
            OS::Linux => "linux",
            OS::Windows => "windows",
            OS::MacOS => "darwin",
            other_os => unimplemented!("System `{}` is not supported!", other_os),
        };
        let arch_name = match *arch {
            Arch::X86_64 => "amd64",
            // No Graal packages for Apple Silicon.
            Arch::AArch64 if TARGET_OS == OS::MacOS => "amd64",
            Arch::AArch64 => "aarch64",
            other_arch => unimplemented!("Architecture `{}` is not supported!", other_arch),
        };
        let java_version = format!("java{}", java_version.0);
        format!("{}-{}-{}-{}", PACKAGE_PREFIX, java_version, os_name, arch_name)
    }

    pub fn root_directory_name(&self) -> PathBuf {
        PathBuf::from(format!("{}-{}-{}", PACKAGE_PREFIX, self.java_version, self.graal_version))
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

    #[tokio::test]
    #[ignore]
    async fn test_is_enabled() -> Result {
        setup_logging()?;
        let graal_version = Version::parse("22.3.0").unwrap();
        let java_version = java::LanguageVersion(11);
        let os = TARGET_OS;
        let arch = Arch::X86_64;
        let client = Octocrab::default();
        let graalvm = GraalVM { graal_version, java_version, os, arch, client };

        graalvm.install_if_missing(&cache::Cache::new_default().await?).await?;

        Gu.require_present().await?;

        // let graalvm = graalvm.is_active().await?;
        // assert!(graalvm);
        Ok(())
    }

    /// Check that we correctly recognize both the GraalVM version and the Java version.
    #[test]
    fn version_recognize() {
        let version_string = r"openjdk 11.0.17 2022-10-18
OpenJDK Runtime Environment GraalVM CE 22.3.0 (build 11.0.17+8-jvmci-22.3-b08)
OpenJDK 64-Bit Server VM GraalVM CE 22.3.0 (build 11.0.17+8-jvmci-22.3-b08, mixed mode, sharing)";

        let found_graal = graal_version_from_version_string(version_string).unwrap();
        assert_eq!(found_graal, Version::new(22, 3, 0));

        let found_java = Java.parse_version(version_string).unwrap();
        assert_eq!(found_java, Version::new(11, 0, 17));
    }
}
