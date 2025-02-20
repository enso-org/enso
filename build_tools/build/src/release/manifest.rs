//! Description of the release assets.
//!
//! See [Assets].

use crate::prelude::*;

use crate::paths::TargetTriple;
use crate::project;
use crate::version::Versions;



/// Name of the assets manifest file.
///
/// The website uses this name to find the assets manifest, so it should be kept in sync.
pub const ASSETS_MANIFEST_FILENAME: &str = "assets.json";

/// A platform-specific asset being part of the release, see [Assets] for the purpose.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Asset {
    pub os:            OS,
    pub arch:          Arch,
    pub url:           Url,
    /// User-friendly description of the target platform.
    pub target_pretty: String,
}

impl Asset {
    pub fn new(url: Url, triple: &TargetTriple) -> Self {
        let target_pretty = match (triple.os, triple.arch) {
            (OS::Windows, Arch::X86_64) => "Windows".into(),
            (OS::Linux, Arch::X86_64) => "Linux".into(),
            (OS::MacOS, Arch::X86_64) => "macOS (Intel)".into(),
            (OS::MacOS, Arch::AArch64) => "macOS (Apple silicon)".into(),
            (os, arch) => format!("{os} {arch}"),
        };
        Self { os: triple.os, arch: triple.arch, url, target_pretty }
    }

    /// Description od the asset with IDE image.
    pub fn new_ide(repo: &impl IsRepo, triple: &TargetTriple) -> Self {
        let filename =
            project::ide::electron_image_filename(triple.os, triple.arch, &triple.versions.version);
        let url = ide_ci::github::release::download_asset(repo, &triple.versions.version, filename);
        Self::new(url, triple)
    }

    /// Description od the asset with Engine bundle.
    pub fn new_engine(repo: &impl IsRepo, triple: &TargetTriple) -> Self {
        use crate::paths::generated::RepoRootBuiltDistributionEnsoBundleTriple;
        let stem = RepoRootBuiltDistributionEnsoBundleTriple::segment_name(triple.to_string());
        let ext = ide_ci::github::release::archive_extension_for(triple.os);
        let filename = format!("{stem}.{ext}");
        let url = ide_ci::github::release::download_asset(repo, &triple.versions.version, filename);
        Self::new(url, triple)
    }
}


/// Describes the assets that are part of the release.
///
/// The information is used to:
/// * create `assets.json` file used by the project's website,
/// * fill information in the release description template.
// When changing the structure, make sure that it does not break the website.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Assets {
    /// IDE packages. The exact format (e.g. installer vs AppImage) depends on the platform.
    pub ide:     Vec<Asset>,
    /// Engine bundles.
    pub engine:  Vec<Asset>,
    /// Version of the release.
    pub version: Version,
}

impl Assets {
    pub fn new(repo: &impl IsRepo, version: &Version) -> Self {
        let mut ret = Self { ide: vec![], engine: vec![], version: version.clone() };
        for (os, arch) in crate::ci_gen::RELEASE_TARGETS {
            let triple = TargetTriple { os, arch, versions: Versions::new(version.clone()) };
            ret.ide.push(Asset::new_ide(repo, &triple));
            ret.engine.push(Asset::new_engine(repo, &triple));
        }
        ret
    }

    /// Iterate all the assets described in this manifest.
    pub fn assets(&self) -> impl Iterator<Item = &Asset> {
        self.ide.iter().chain(&self.engine)
    }
}
