//! Build logic for the GUI project.
//!
//! The GUI is Vue.js-based and located under `app/gui`.

use crate::prelude::*;

use crate::ide::web::env as ide_env;
use crate::ide::web::IdeDesktop;
use crate::paths::generated::RepoRootAppGuiDist;
use crate::paths::generated::RepoRootDistGuiAssets;
use crate::project::Context;
use crate::project::IsArtifact;
use crate::project::IsTarget;
use crate::source::WithDestination;
use ide_ci::ok_ready_boxed;
use ide_ci::programs::Pnpm;



// ================
// === Artifact ===
// ================

/// The [artifact](IsArtifact) for the new GUI.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deref)]
pub struct Artifact(pub RepoRootAppGuiDist);

impl AsRef<Path> for Artifact {
    fn as_ref(&self) -> &Path {
        self.0.as_path()
    }
}

impl IsArtifact for Artifact {}

impl Artifact {
    pub fn new(path: impl AsRef<Path>) -> Self {
        Artifact(RepoRootAppGuiDist::new_root(path.as_ref()))
    }
}

// ==============
// === Target ===
// ==============

/// The [target](IsTarget) for the new GUI.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Gui;

#[derive_where(Debug)]
pub struct BuildInput {
    pub version:     Version,
    #[derive_where(skip)]
    pub commit_hash: BoxFuture<'static, Result<String>>,
}

impl IsTarget for Gui {
    type BuildInput = BuildInput;
    type Artifact = Artifact;

    fn artifact_name(&self) -> String {
        "gui".to_owned()
    }

    fn adapt_artifact(self, path: impl AsRef<Path>) -> BoxFuture<'static, Result<Self::Artifact>> {
        ok_ready_boxed(Artifact::new(path))
    }

    fn build_internal(
        &self,
        context: Context,
        job: WithDestination<Self::BuildInput>,
    ) -> BoxFuture<'static, Result<Self::Artifact>> {
        let WithDestination { inner: BuildInput { version, commit_hash }, destination } = job;
        async move {
            let repo_root = &context.repo_root;
            let version_string = version.to_string();
            crate::web::install(repo_root).await?;
            let commit_hash = commit_hash.await?;
            Pnpm.cmd()?
                .current_dir(repo_root)
                .set_env(ide_env::ENSO_IDE_COMMIT_HASH, &commit_hash)?
                .set_env(ide_env::ENSO_IDE_VERSION, &version_string)?
                .run("build:gui")
                .run_ok()
                .await?;

            ide_ci::fs::mirror_directory(
                &repo_root.app.gui.dist,
                &destination.join(RepoRootDistGuiAssets::segment_name()),
            )
            .await?;
            Ok(Artifact::new(destination))
        }
        .boxed()
    }
}

pub fn ide_desktop_from_context(context: &Context) -> IdeDesktop {
    IdeDesktop::new(&context.repo_root, context.octocrab.clone(), context.cache.clone())
}
