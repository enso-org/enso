//! Build logic for the GUI project.
//!
//! The GUI is Vue.js-based and located under `app/gui`.

use crate::prelude::*;

use crate::ide::web::IdeDesktop;
use crate::paths::generated::RepoRootAppGuiDist;
use crate::paths::generated::RepoRootDistGuiAssets;
use crate::project::Context;
use crate::project::IsArtifact;
use crate::project::IsTarget;
use crate::source::WithDestination;

use ide_ci::ok_ready_boxed;

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

impl IsTarget for Gui {
    type BuildInput = ();
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
        let WithDestination { inner: _, destination } = job;
        async move {
            let repo_root = &context.repo_root;
            crate::web::install(repo_root).await?;
            crate::web::run_script(repo_root, crate::web::Script::Build).await?;
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

// =================
// === BuildInfo ===
// =================
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BuildInfo {
    pub commit:         String,
    pub version:        Version,
    pub engine_version: Version,
    pub name:           String,
}

pub fn ide_desktop_from_context(context: &Context) -> IdeDesktop {
    IdeDesktop::new(&context.repo_root, context.octocrab.clone(), context.cache.clone())
}
