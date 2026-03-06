use crate::prelude::*;

use ide_ci::github::release;

#[derive(Clone, Copy, Debug)]
pub enum ArtifactKind {
    EnginePackage,
    LauncherPackage,
    LauncherBundle,
    EngineBundle,
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

    /// Return `true` if the artifact should be published with the release.
    ///
    /// Note that the current release logic tries to attach all artifacts to the GitHub release.
    /// And if some artifact is required, the script looks for it in the release and fails if
    /// the artifact was not found.
    fn is_published(&self) -> bool;

    /// Get a filename stem for the compressed artifact.
    ///
    /// It will be used for naming release assets, so this should include the target triple.
    fn asset_file_stem(&self) -> Result<OsString> {
        // By the convention, the parent directory to the artifact bears its asset name.
        Ok(self.as_ref().try_parent()?.try_file_name()?.to_os_string())
    }

    fn upload_as_asset(&self, release: release::Handle) -> BoxFuture<'static, Result> {
        if self.is_published() {
            let path = self.as_ref().to_path_buf();
            let name = self.asset_file_stem();
            async move {
                release.upload_compressed_dir_as(path, name?).await?;
                Ok(())
            }
            .boxed()
        } else {
            async move { Ok(()) }.boxed()
        }
    }

    fn as_dyn_artifact(&self) -> &dyn IsArtifact;
}

impl IsArtifact for crate::paths::generated::EnginePackage {
    fn kind(&self) -> ArtifactKind {
        ArtifactKind::EnginePackage
    }
    fn is_published(&self) -> bool {
        true
    }
    fn as_dyn_artifact(&self) -> &dyn IsArtifact {
        self
    }
}

impl IsArtifact for crate::paths::generated::LauncherPackage {
    fn kind(&self) -> ArtifactKind {
        ArtifactKind::LauncherPackage
    }
    fn is_published(&self) -> bool {
        false
    }
    fn as_dyn_artifact(&self) -> &dyn IsArtifact {
        self
    }
}

impl IsArtifact for crate::paths::generated::LauncherBundle {
    fn kind(&self) -> ArtifactKind {
        ArtifactKind::LauncherBundle
    }
    fn is_published(&self) -> bool {
        true
    }
    fn as_dyn_artifact(&self) -> &dyn IsArtifact {
        self
    }
}

impl IsArtifact for crate::paths::generated::EngineBundle {
    fn kind(&self) -> ArtifactKind {
        ArtifactKind::EngineBundle
    }
    fn is_published(&self) -> bool {
        true
    }
    fn as_dyn_artifact(&self) -> &dyn IsArtifact {
        self
    }
}
