use crate::prelude::*;

use crate::cache::goodie;
use crate::cache::Cache;
use crate::env::known::PATH;
use crate::env::Modification;
use crate::github::RepoRef;



/// The repository of the FlatBuffers project.
pub const REPO: RepoRef = RepoRef { owner: "google", name: "flatbuffers" };

#[derive(Clone, Debug)]
pub struct Flatc {
    /// The version of the `flatc` binary.
    pub version:  Version,
    /// Operating system for which the binary is built.
    pub platform: OS,
}

impl Flatc {
    /// Get the download URL.
    ///
    /// The package is a zip file containing the `flatc` binary.
    ///
    /// ```
    /// use ide_ci::prelude::*;
    /// use ide_ci::cache::goodie::flatc::Flatc;
    ///
    /// # fn main() -> Result {
    /// let version = Version::from_str("24.3.25")?;
    /// let flatc = Flatc { version, platform: OS::Linux };
    /// let url = flatc.url()?;
    /// assert_eq!(url.as_str(), "https://github.com/google/flatbuffers/releases/download/v24.3.25/Linux.flatc.binary.clang++-15.zip");
    /// # Ok(())
    /// # }
    pub fn url(&self) -> Result<Url> {
        let filename = match (&self.version, self.platform) {
            (version, OS::Windows) if version >= &Version::new(2, 0, 0) =>
                "Windows.flatc.binary.zip",
            (_, OS::Windows) => "flatc_windows.zip",
            (_, OS::MacOS) => "Mac.flatc.binary.zip",
            (version, OS::Linux) if version >= &Version::new(24, 3, 6) =>
                "Linux.flatc.binary.clang++-15.zip",
            (version, OS::Linux) if version >= &Version::new(2, 0, 7) =>
                "Linux.flatc.binary.clang++-12.zip",
            (version, OS::Linux) if version >= &Version::new(2, 0, 0) =>
                "Linux.flatc.binary.clang++-9.zip",
            (version, os) => bail!("Unsupported version {version} for {os}."),
        };
        let tag = release_tag(&self.version);
        Ok(crate::github::release::download_asset(&REPO, &tag, filename))
    }
}

impl Goodie for Flatc {
    fn get(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        goodie::download_try_url(self.url(), cache)
    }

    fn is_active(&self) -> BoxFuture<'static, Result<bool>> {
        let version = self.version.clone();
        async move {
            let flatc = crate::programs::flatc::Flatc;
            flatc.require_present_at(&version).await?;
            Ok(true)
        }
        .boxed()
    }

    fn activation_env_changes(&self, package_path: &Path) -> Result<Vec<Modification>> {
        Ok(vec![Modification::prepend_path(&PATH, package_path)])
    }
}

/// Get the release tag for the given FlatBuffers version.
///
/// The FlatBuffers release tag is prefixed with `v`, so we cannot use the `Version` directly.
pub fn release_tag(version: &Version) -> String {
    format!("v{}", version)
}
