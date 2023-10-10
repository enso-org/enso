//! Build logic for the "new GUI" (gui2) project.
//!
//! The new GUI is Vue.js-based and located under `app/gui2`.

use crate::prelude::*;

use crate::paths::generated::RepoRootAppGui2Dist;
use crate::paths::generated::RepoRootDistGui2Assets;
use crate::project::Context;
use crate::project::IsArtifact;
use crate::project::IsTarget;
use crate::source::WithDestination;

use ide_ci::ok_ready_boxed;
use ide_ci::programs::node::NpmCommand;
use ide_ci::programs::Npm;



// ===============
// === Scripts ===
// ===============

/// The scripts defined in `package.json`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum Scripts {
    Dev,
    Build,
    Preview,
    #[strum(serialize = "test:unit")]
    TestUnit,
    #[strum(serialize = "test:e2e")]
    TestE2e,
    BuildOnly,
    TypeCheck,
    Lint,
    Format,
    BuildRustFfi,
}

pub fn script(repo_root: impl AsRef<Path>, script: Scripts) -> Result<NpmCommand> {
    let mut ret = Npm.cmd()?;
    ret.current_dir(repo_root).workspace(crate::web::Workspace::EnsoGui2).run(script.as_ref());
    Ok(ret)
}



// ================
// === Commands ===
// ================

/// Run steps that should be done along with the "lint"
pub fn lint(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    let repo_root = repo_root.as_ref().to_owned();
    async move {
        crate::web::install(&repo_root).await?;
        script(&repo_root, Scripts::Lint)?.run_ok().await
    }
    .boxed()
}

/// Run unit tests.
pub fn unit_tests(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    let repo_root = repo_root.as_ref().to_owned();
    async move {
        crate::web::install(&repo_root).await?;
        script(&repo_root, Scripts::TestUnit)?.arg("run").run_ok().await
    }
    .boxed()
}



// ================
// === Artifact ===
// ================

/// The [artifact](IsArtifact) for the new GUI.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deref)]
pub struct Artifact(pub RepoRootAppGui2Dist);

impl AsRef<Path> for Artifact {
    fn as_ref(&self) -> &Path {
        self.0.as_path()
    }
}

impl IsArtifact for Artifact {}

impl Artifact {
    pub fn new(path: impl AsRef<Path>) -> Self {
        Artifact(RepoRootAppGui2Dist::new_root(path.as_ref()))
    }
}



// ==============
// === Target ===
// ==============

/// The [target](IsTarget) for the new GUI.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Gui2;

impl IsTarget for Gui2 {
    type BuildInput = ();
    type Artifact = Artifact;

    fn artifact_name(&self) -> String {
        "gui2".to_owned()
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
            script(repo_root, Scripts::Build)?.run_ok().await?;
            ide_ci::fs::mirror_directory(
                &repo_root.app.gui_2.dist,
                &destination.join(RepoRootDistGui2Assets::segment_name()),
            )
            .await?;
            Ok(Artifact::new(destination))
        }
        .boxed()
    }
}
