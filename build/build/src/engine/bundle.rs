use crate::prelude::*;

use crate::engine::artifact::IsArtifact;
use crate::paths::generated::RepoRoot;

use ide_ci::cache::goodie::graalvm::locate_graal;



/// Bundle is like a [package][crate::paths::IsPackage] but with additional components bundled to
/// make it redistributable.
///
/// See the [official docs](https://enso.org/docs/developer/enso/distribution/bundles.html).
#[async_trait]
pub trait IsBundle: AsRef<Path> + IsArtifact {
    fn clear(&self) -> Result {
        ide_ci::fs::remove_dir_if_exists(self.as_ref())
    }

    /// Path to the directory where GraalVM is placed.
    fn graalvm_dir(&self) -> PathBuf;

    /// Path to the directory where Engine package is placed.
    fn engine_dir(&self) -> PathBuf;

    /// Path to the component that will be used as a bundle base.
    fn base_component(&self, repo_root: &RepoRoot) -> PathBuf;

    /// Path to the bundle marker file.
    ///
    /// It is a file used by bundled executable to discern whether it is running from a bundle or
    /// from a regular installation.
    fn distribution_marker(&self) -> PathBuf;

    /// Creates a bundle for a given component. This requires already built:
    ///  * the base component package (e.g. launcher package for launcher bundle);
    ///  * the engine package;
    ///  * the GraalVM package.
    ///  *
    ///
    /// `bundle_dir` is like:
    /// ```text
    /// H:\NBO\enso\built-distribution\enso-engine-0.0.0-SNAPSHOT.2022-01-19-windows-amd64\enso-0.0.0-SNAPSHOT.2022-01-19
    /// ```
    fn create(&self, repo_root: &RepoRoot) -> BoxFuture<'static, Result> {
        let bundle_dir = self.as_ref().to_path_buf();
        let base_component = self.base_component(repo_root);
        let engine_src_path =
            repo_root.built_distribution.enso_engine_triple.engine_package.clone();
        let engine_target_dir = self.engine_dir();
        let graalvm_dir = self.graalvm_dir();
        let distribution_marker = self.distribution_marker();

        async move {
            ide_ci::fs::tokio::remove_dir_if_exists(&bundle_dir).await?;
            // Start with bundled component.
            ide_ci::fs::copy(&base_component, bundle_dir)?;
            // Add engine.
            ide_ci::fs::mirror_directory(&engine_src_path, &engine_target_dir).await?;
            // Add GraalVM runtime.
            place_graal_under(graalvm_dir).await?;
            // Add portable distribution marker.
            ide_ci::fs::create(distribution_marker)?;
            Ok(())
        }
        .boxed()
    }
}

impl IsBundle for crate::paths::generated::ProjectManagerBundle {
    fn graalvm_dir(&self) -> PathBuf {
        self.runtime.path.clone()
    }

    fn engine_dir(&self) -> PathBuf {
        self.dist.version.to_path_buf()
    }

    fn base_component(&self, repo_root: &RepoRoot) -> PathBuf {
        repo_root
            .built_distribution
            .enso_project_manager_triple
            .project_manager_package
            .to_path_buf()
    }

    fn distribution_marker(&self) -> PathBuf {
        self.enso_bundle.to_path_buf()
    }
}

impl IsBundle for crate::paths::generated::LauncherBundle {
    fn graalvm_dir(&self) -> PathBuf {
        self.runtime.path.clone()
    }

    fn engine_dir(&self) -> PathBuf {
        self.dist.version.to_path_buf()
    }

    fn base_component(&self, repo_root: &RepoRoot) -> PathBuf {
        repo_root.built_distribution.enso_launcher_triple.launcher_package.to_path_buf()
    }

    fn distribution_marker(&self) -> PathBuf {
        self.enso_portable.to_path_buf()
    }
}

/// Places a copy of the GraalVM's installation directory in the target directory.
///
/// The GraalVM installation will be located using [`locate_graal`] function.
#[context("Failed to place a GraalVM package under {}.", target_directory.as_ref().display())]
pub async fn place_graal_under(target_directory: impl AsRef<Path>) -> Result {
    let graal_path = locate_graal()?;
    let graal_dirname = graal_path.try_file_name()?;
    ide_ci::fs::mirror_directory(&graal_path, target_directory.as_ref().join(graal_dirname)).await
}
