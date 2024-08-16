use crate::prelude::*;

use crate::cache::goodie;
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

const CE_JAVA_VENDOR: &str = "GraalVM CE";
const EE_JAVA_VENDOR: &str = "Oracle GraalVM";

#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub enum Edition {
    /// GraalVM CE (Community Edition).
    #[default]
    Community,
    /// Oracle GraalVM (Formerly GraalVM EE, Enterprise Edition)
    Enterprise,
}

impl FromStr for Edition {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            CE_JAVA_VENDOR => Ok(Edition::Community),
            EE_JAVA_VENDOR => Ok(Edition::Enterprise),
            _ => bail!("Unknown GraalVM edition: {}", s),
        }
    }
}

impl From<Edition> for String {
    fn from(edition: Edition) -> String {
        match edition {
            Edition::Community => CE_JAVA_VENDOR.to_string(),
            Edition::Enterprise => EE_JAVA_VENDOR.to_string(),
        }
    }
}

impl Display for Edition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match *self {
            Edition::Community => write!(f, "{CE_JAVA_VENDOR}"),
            Edition::Enterprise => write!(f, "{EE_JAVA_VENDOR}"),
        }
    }
}


pub fn graal_version_from_version_string(version_string: &str) -> Result<(Version, Edition)> {
    let line = version_string
        .lines()
        .find(|line| line.contains(CE_JAVA_VENDOR) || line.contains(EE_JAVA_VENDOR))
        .context(
            "There is a Java environment available but it is not recognizable as GraalVM one.",
        )?;
    let edition = if line.contains(CE_JAVA_VENDOR) {
        Edition::Community
    } else if line.contains(EE_JAVA_VENDOR) {
        Edition::Enterprise
    } else {
        bail!("Unknown GraalVM edition")
    };
    let version = Version::find_in_text(line);
    version.map(|version| (version, edition)).context("Failed to find GraalVM version.")
}

pub async fn find_graal_version() -> Result<(Version, Edition)> {
    let text = Java.version_string().await?;
    graal_version_from_version_string(&text)
}


/// Description necessary to download and install GraalVM.
#[derive(Clone, Debug)]
pub struct GraalVM {
    /// Used to query GitHub about releases.
    pub client:        Octocrab,
    pub graal_version: Version,
    pub edition:       Edition,
    pub os:            OS,
    pub arch:          Arch,
}

impl Goodie for GraalVM {
    fn get(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        goodie::download_try_future_url(self.url(), cache)
    }


    fn is_active(&self) -> BoxFuture<'static, Result<bool>> {
        let expected_graal_version = self.graal_version.clone();
        let expected_graal_edition = self.edition;
        async move {
            let (found_version, found_edition) = find_graal_version().await?;
            ensure!(found_version == expected_graal_version, "GraalVM version mismatch. Expected {expected_graal_version}, found {found_version}.");
            ensure!(found_edition == expected_graal_edition, "GraalVM edition mismatch. Expected {expected_graal_edition}, found {found_edition}.");
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
        match self.edition {
            Edition::Community => {
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
            Edition::Enterprise => {
                let graal_version_tag = self.graal_version.to_string_core();
                let url = format!(
                    "https://download.oracle.com/graalvm/{}/archive/graalvm-jdk-{}_{}_bin.tar.gz",
                    self.graal_version.major,
                    graal_version_tag,
                    self.os_arch_string(),
                );
                Box::pin(ready(Url::parse(&url).context("Failed to parse URL.")))
            }
        }
    }

    fn os_arch_string(&self) -> String {
        let os_name = match self.os {
            OS::Linux => "linux",
            OS::Windows => "windows",
            OS::MacOS => "macos",
        };
        let arch_name = match self.arch {
            Arch::X86_64 => "x64",
            Arch::AArch64 => "aarch64",
            other_arch => unimplemented!("Architecture `{}` is not supported!", other_arch),
        };
        format!("{os_name}-{arch_name}")
    }

    pub fn platform_string(&self) -> String {
        let os_arch = self.os_arch_string();
        let java_version = format!("jdk-{}", self.graal_version.to_string_core());
        format!("{PACKAGE_PREFIX_URL}-{java_version}_{os_arch}")
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
    use semver::BuildMetadata;
    use semver::Prerelease;

    /// Check that we correctly recognize both the GraalVM version and the Java version.
    #[test]
    fn version_recognize() {
        let version_string = r#"openjdk version "17.0.7" 2023-04-18
OpenJDK Runtime Environment GraalVM CE 17.0.7+7.1 (build 17.0.7+7-jvmci-23.0-b12)
OpenJDK 64-Bit Server VM GraalVM CE 17.0.7+7.1 (build 17.0.7+7-jvmci-23.0-b12, mixed mode, sharing)"#;

        let (found_graal_version, found_graal_edition) =
            graal_version_from_version_string(version_string).unwrap();
        let expected_graal_version = Version {
            major: 17,
            minor: 0,
            patch: 7,
            pre:   Prerelease::EMPTY,
            build: BuildMetadata::new("7.1").unwrap(),
        };
        assert_eq!(found_graal_version, expected_graal_version);
        assert_eq!(found_graal_edition, Edition::Community);

        let found_java = Java.parse_version(version_string).unwrap();
        assert_eq!(found_java, Version::new(17, 0, 7));
    }

    #[test]
    fn enterprise_version_recognize() {
        let version_string = r#"java version "21.0.2" 2024-01-16 LTS
Java(TM) SE Runtime Environment Oracle GraalVM 21.0.2+13.1 (build 21.0.2+13-LTS-jvmci-23.1-b30)
Java HotSpot(TM) 64-Bit Server VM Oracle GraalVM 21.0.2+13.1 (build 21.0.2+13-LTS-jvmci-23.1-b30, mixed mode, sharing)"#;

        let (found_graal_version, found_graal_edition) =
            graal_version_from_version_string(version_string).unwrap();
        let expected_graal_version = Version {
            major: 21,
            minor: 0,
            patch: 2,
            pre:   Prerelease::EMPTY,
            build: BuildMetadata::new("13.1").unwrap(),
        };
        assert_eq!(found_graal_version, expected_graal_version);
        assert_eq!(found_graal_edition, Edition::Enterprise);

        let found_java = Java.parse_version(version_string).unwrap();
        assert_eq!(found_java, Version::new(21, 0, 2));
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

    #[tokio::test]
    async fn correct_url_for_enterprise_edition_21() {
        let graalvm = GraalVM {
            client:        Octocrab::default(),
            graal_version: Version::new(21, 0, 2),
            edition:       Edition::Enterprise,
            os:            OS::Linux,
            arch:          Arch::X86_64,
        };
        let url = graalvm.url().await.unwrap();
        assert_eq!(url.to_string(), "https://download.oracle.com/graalvm/21/archive/graalvm-jdk-21.0.2_linux-x64_bin.tar.gz");
    }

    #[tokio::test]
    async fn correct_url_for_enterprise_edition_17() {
        let graalvm = GraalVM {
            client:        Octocrab::default(),
            graal_version: Version::new(17, 0, 7),
            edition:       Edition::Enterprise,
            os:            OS::Linux,
            arch:          Arch::X86_64,
        };
        let url = graalvm.url().await.unwrap();
        assert_eq!(url.to_string(), "https://download.oracle.com/graalvm/17/archive/graalvm-jdk-17.0.7_linux-x64_bin.tar.gz");
    }

    #[tokio::test]
    async fn correct_url_for_community_edition() {
        let graalvm = GraalVM {
            client:        Octocrab::default(),
            graal_version: Version::new(21, 0, 2),
            edition:       Edition::Community,
            os:            OS::Linux,
            arch:          Arch::X86_64,
        };
        let url = graalvm.url().await.unwrap();
        assert_eq!(url.to_string(), "https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-21.0.2/graalvm-community-jdk-21.0.2_linux-x64_bin.tar.gz");
    }
}
