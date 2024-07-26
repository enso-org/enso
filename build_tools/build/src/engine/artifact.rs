use crate::prelude::*;

use ide_ci::github::release;
use octocrab::models::repos::Asset;



#[derive(Clone, Copy, Debug)]
pub enum ArtifactKind {
    EnginePackage,
    ProjectManagerPackage,
    LauncherPackage,
    ProjectManagerBundle,
    LauncherBundle,
}

/// A standalone SBT-generated artifact.
///
/// Either a package or a bundle with one of our backend components.
pub trait IsArtifact: AsRef<Path> + Send + Sync {
    /// Get the kind of this artifact.
    fn kind(&self) -> ArtifactKind;

    /// Remove the artifact from the disk.
    fn clear(&self) -> Result {
        ide_ci::fs::remove_dir_if_exists(self)
    }

    /// Get a filename stem for the compressed artifact.
    ///
    /// It will be used for naming release assets, so this should include the target triple.
    fn asset_file_stem(&self) -> Result<OsString> {
        // By the convention, the parent directory to the artifact bears its asset name.
        Ok(self.as_ref().try_parent()?.try_file_name()?.to_os_string())
    }

    fn upload_as_asset(&self, release: release::Handle) -> BoxFuture<'static, Result<Asset>> {
        let path = self.as_ref().to_path_buf();
        let name = self.asset_file_stem();
        async move { release.upload_compressed_dir_as(path, name?).await }.boxed()
    }
}

impl IsArtifact for crate::paths::generated::EnginePackage {
    fn kind(&self) -> ArtifactKind {
        ArtifactKind::EnginePackage
    }
}

impl IsArtifact for crate::paths::generated::ProjectManagerPackage {
    fn kind(&self) -> ArtifactKind {
        ArtifactKind::ProjectManagerPackage
    }
}

impl IsArtifact for crate::paths::generated::ProjectManagerBundle {
    fn kind(&self) -> ArtifactKind {
        ArtifactKind::ProjectManagerBundle
    }
}

impl IsArtifact for crate::paths::generated::LauncherPackage {
    fn kind(&self) -> ArtifactKind {
        ArtifactKind::LauncherPackage
    }
}

impl IsArtifact for crate::paths::generated::LauncherBundle {
    fn kind(&self) -> ArtifactKind {
        ArtifactKind::LauncherBundle
    }
}
