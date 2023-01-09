use crate::prelude::*;

use crate::paths::generated::RepoRoot;
use crate::paths::ComponentPaths;
use crate::paths::IsArtifact;

use anyhow::Context;
use ide_ci::programs::java::JAVA_HOME;



//
// #[async_trait]
// pub trait Bundle {
//     const PREFIX: &'static str;
//     const DIRNAME: &'static str;
//
//     fn base_distribution(paths: &Paths) -> &ComponentPaths;
//
//     fn suggest_paths(paths: &Paths) -> ComponentPaths {
//         ComponentPaths::new(&paths.build_dist_root, Self::PREFIX, Self::DIRNAME, &paths.triple)
//     }
//
//     async fn create(paths: &Paths) -> Result<ComponentPaths> {
//         let bundle = Self::suggest_paths(paths);
//
//         bundle.clear()?;
//
//         let base_component = Self::base_distribution(paths);
//         ide_ci::fs::copy(&base_component.root, &bundle.root)?;
//
//         // Add engine.
//         let bundled_engine_dir = bundle.dir.join("dist").join(paths.version().to_string());
//         place_component_at(&paths.engine, &bundled_engine_dir).await?;
//
//         // Add GraalVM runtime.
//         place_graal_under(bundle.dir.join("runtime")).await?;
//
//         // Add portable distribution marker.
//         ide_ci::fs::copy(
//             &paths.repo_root.distribution.enso_bundle_template,
//             bundle.dir.join(".enso.bundle"),
//         )?;
//         Ok(bundle)
//     }
// }
//
// #[derive(Clone, Copy, Debug)]
// pub struct Launcher;
// impl Bundle for Launcher {
//     const PREFIX: &'static str = "enso-bundle";
//     const DIRNAME: &'static str = "enso";
//     fn base_distribution(paths: &Paths) -> &ComponentPaths {
//         &paths.launcher
//     }
// }
//
// #[derive(Clone, Copy, Debug)]
// pub struct ProjectManager;
// impl Bundle for ProjectManager {
//     const PREFIX: &'static str = "project-manager-bundle";
//     const DIRNAME: &'static str = "enso";
//     fn base_distribution(paths: &Paths) -> &ComponentPaths {
//         &paths.project_manager
//     }
// }

// #[context("Failed to locate GraalVM installation.")]
pub fn locate_graal() -> Result<PathBuf> {
    let java_home = JAVA_HOME.get()?;
    Ok(if TARGET_OS == OS::MacOS {
        // On macOS we need to drop trailing `/Contents/Home` from the path.
        java_home.try_parent()?.try_parent()?.to_path_buf()
    } else {
        java_home
    })
}

#[context("Placing a GraalVM package under {}", target_directory.as_ref().display())]
pub async fn place_graal_under(target_directory: impl AsRef<Path>) -> Result {
    let graal_path = {
        let java_home = JAVA_HOME.get()?;
        if TARGET_OS == OS::MacOS {
            // On macOS we need to drop trailing `/Contents/Home` from the path.
            java_home
                .parent()
                .and_then(|p| p.parent())
                .context(format!("Invalid Java home for macOS: {}", java_home.display()))?
                .to_path_buf()
        } else {
            java_home
        }
    };
    let graal_dirname = graal_path
        .file_name()
        .context(anyhow!("Invalid Graal Path deduced from JAVA_HOME: {}", graal_path.display()))?;
    ide_ci::fs::mirror_directory(&graal_path, target_directory.as_ref().join(graal_dirname)).await
}

#[context("Placing a Enso Engine package in {}", target_engine_dir.as_ref().display())]
pub async fn place_component_at(
    engine_paths: &ComponentPaths,
    target_engine_dir: impl AsRef<Path>,
) -> Result {
    ide_ci::fs::mirror_directory(&engine_paths.dir, &target_engine_dir).await
}

#[async_trait]
pub trait IsBundle: AsRef<Path> + IsArtifact {
    fn clear(&self) -> Result {
        ide_ci::fs::remove_dir_if_exists(self.as_ref())
    }

    /// Path to the directory where GraalVM is placed.
    fn graalvm_dir(&self) -> PathBuf;

    /// Path to the directory where Engine package is placed.
    fn engine_dir(&self) -> PathBuf;

    fn base_component(&self, repo_root: &RepoRoot) -> PathBuf;

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

            // let bundle = Self::suggest_paths(paths);
            //
            // bundle.clear()?;
            //
            // let base_component = Self::base_distribution(paths);
            // ide_ci::fs::copy(&base_component.root, &bundle.root)?;
            //
            // // Add engine.
            // let bundled_engine_dir = bundle.dir.join("dist").join(paths.version().to_string());
            // place_component_at(&paths.engine, &bundled_engine_dir).await?;
            //
            // // Add GraalVM runtime.
            // place_graal_under(bundle.dir.join("runtime")).await?;
            //
            // // Add portable distribution marker.
            // ide_ci::fs::copy(
            //     &paths.repo_root.distribution.enso_bundle_template,
            //     bundle.dir.join(".enso.bundle"),
            // )?;
            // Ok(bundle)
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
