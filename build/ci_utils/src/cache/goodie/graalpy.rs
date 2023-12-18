use crate::prelude::*;

use crate::cache::goodie;
use crate::cache::goodie::Goodie;
use crate::cache::Cache;
use crate::env::known::PATH;
use crate::github::RepoRef;
use crate::programs::graalpy::GraalPy as GraalPyProgram;

use regex::Regex;



pub const CE_BUILDS_REPOSITORY: RepoRef = RepoRef { owner: "oracle", name: "graalpython" };

#[derive(Clone, Debug)]
pub struct GraalPy {
    pub client:  Octocrab,
    pub version: Version,
    pub os:      OS,
    pub arch:    Arch,
}

fn graalpy_version_from_str(version_string: &str) -> Result<Version> {
    let line = version_string.lines().find(|line| line.contains("GraalVM CE")).context(
        "There is a Java environment available but it is not recognizable as GraalVM one.",
    )?;
    let re = Regex::new(r"GraalPy.*\((.+)\)").unwrap();
    let caps = re.captures(line).unwrap();
    Version::find_in_text(caps.get(1).context("graalpy wrong version text").unwrap().as_str())
}

async fn find_graalpy_version() -> Result<Version> {
    let text = GraalPyProgram.version_string().await?;
    graalpy_version_from_str(&text)
}

impl Goodie for GraalPy {
    fn get(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        goodie::download_try_future_url(self.url(), cache)
    }

    fn is_active(&self) -> BoxFuture<'static, Result<bool>> {
        let expected_graalpy_version = self.version.clone();
        async move {
            let found_version = find_graalpy_version().await?;
            ensure!(found_version == expected_graalpy_version, "GraalPy version mismatch. Expected {expected_graalpy_version}, found {found_version}.");
            Ok(true)
        }
            .boxed()
    }

    fn activation_env_changes(&self, package_path: &Path) -> Result<Vec<crate::env::Modification>> {
        let dir_entries = package_path
            .read_dir()
            .context("Failed to read GraalPy cache directory")?
            .collect_vec();
        let [graalpy_dir] = dir_entries.as_slice() else {
            bail!("GraalPy cache directory should contain exactly one directory");
        };
        let graalpy_dir = match graalpy_dir {
            Ok(dir_entry) => dir_entry,
            Err(err) => bail!("Failed to read GraalPy cache directory: {}", err),
        };
        let dir_name = graalpy_dir.file_name();
        let dir_name = dir_name.as_str();
        ensure!(dir_name.contains("graalpy"));
        ensure!(dir_name.contains(self.version.to_string_core().as_str()));
        Ok(vec![crate::env::Modification::prepend_path(&PATH, graalpy_dir.path().join("bin"))])
    }
}

impl GraalPy {
    pub fn url(&self) -> BoxFuture<'static, Result<Url>> {
        let this = self.clone();
        let client = self.client.clone();
        let arch_name = match self.arch {
            Arch::X86_64 => "amd64",
            Arch::AArch64 => "aarch64",
            _ => unimplemented!("Unsupported architecture: {}", self.arch.to_string()),
        };
        async move {
            let repo = CE_BUILDS_REPOSITORY.handle(&client);
            let tag = format!("graal-{}", this.version);
            let release = repo.find_release_by_tag(tag.as_str()).await?;
            let asset_name =
                format!("graalpy-community-{}-{}-{}", this.version, this.os, arch_name);
            let asset =
                crate::github::find_asset_url_by_text(&release, asset_name.as_str()).cloned();
            asset
        }
        .boxed()
    }
}

#[cfg(test)]
mod tests {
    use crate::cache::goodie::graalpy::graalpy_version_from_str;
    use crate::cache::goodie::graalpy::GraalPy;
    use octocrab::Octocrab;
    use platforms::Arch;
    use platforms::OS;
    use semver::Version;

    #[test]
    fn version_recognize() {
        let expected_version = Version::new(23, 1, 0);
        let version_string = "GraalPy 3.10.8 (GraalVM CE Native 23.1.0)";
        let found_version = graalpy_version_from_str(version_string).unwrap();
        assert_eq!(found_version, expected_version);
    }

    #[test]
    fn fetch_correct_url() {
        let version = Version::new(23, 1, 0);
        let client = Octocrab::builder().build().unwrap();
        let graalpy = GraalPy { client, version, os: OS::Linux, arch: Arch::X86_64 };
        let found_url_opt =
            tokio::runtime::Runtime::new().unwrap().block_on(async { graalpy.url().await });
        let found_url = match found_url_opt {
            Ok(url) => url,
            Err(err) => {
                unreachable!("URL not found: {}", err);
            }
        };

        let expected_url = "https://github.com/oracle/graalpython/releases/download/graal-23.1.0/graalpy-community-23.1.0-linux-amd64.tar.gz";
        assert_eq!(found_url.as_str(), expected_url);
    }
}
