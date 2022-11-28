use crate::prelude::*;

use crate::cache::goodie::Goodie;
use crate::github::Repo;
use crate::programs::java;
use crate::programs::java::JAVA_HOME;
use crate::programs::Java;



const PACKAGE_PREFIX: &str = "graalvm-ce";

const GITHUB_ORGANIZATION: &str = "graalvm";

const CE_BUILDS_REPOSITORY: &str = "graalvm-ce-builds";


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

/// The repository that contains the GraalVM CE releases for download.
pub fn ce_build_repository() -> Repo {
    Repo { owner: GITHUB_ORGANIZATION.into(), name: CE_BUILDS_REPOSITORY.into() }
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
    fn url(&self) -> BoxFuture<'static, Result<Url>> {
        let platform_string = self.platform_string();
        let graal_version = self.graal_version.clone();
        let client = self.client.clone();
        let repo = ce_build_repository();
        async move {
            let repo = repo.handle(&client);
            let release = repo.find_release_by_text(&graal_version.to_string()).await?;
            crate::github::find_asset_url_by_text(&release, &platform_string).cloned()
        }
        .boxed()
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

    fn activate(&self, package_path: PathBuf) -> Result {
        let package_path = package_path.join(self.root_directory_name());
        let root = match TARGET_OS {
            OS::MacOS => package_path.join_iter(["Contents", "Home"]),
            _ => package_path,
        };

        JAVA_HOME.set(&root)?;
        GRAALVM_HOME.set(&root)?;
        crate::env::prepend_to_path(root.join("bin"))?;
        Ok(())
    }
}

impl GraalVM {
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
