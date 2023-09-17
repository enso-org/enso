use crate::prelude::*;
use ide_ci::ok_ready_boxed;

use crate::paths::generated::RepoRootAppGui2;
use crate::paths::generated::RepoRootAppGui2Dist;
use crate::paths::generated::RepoRootDistGui2Assets;
use crate::project::Context;
use crate::project::IsArtifact;
use crate::project::IsTarget;

use crate::source::BuildTargetJob;
use crate::source::WithDestination;
use ide_ci::program::EMPTY_ARGS;
use ide_ci::programs::node::NpmCommand;
use ide_ci::programs::Npm;


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

#[async_trait]
pub trait LocateGui2 {
    fn package_dir(&self) -> &Path;

    fn npm(&self) -> Result<NpmCommand> {
        Npm.cmd().map(|c| c.with_current_dir(self.package_dir()))
    }

    fn run_cmd(&self, adjust_cmd: impl FnOnce(&mut NpmCommand)) -> BoxFuture<Result> {
        self.npm()
            .and_then_async(|mut cmd| {
                adjust_cmd(&mut cmd);
                cmd.run_ok()
            })
            .boxed()
    }

    fn install(&self) -> BoxFuture<Result> {
        self.run_cmd(|c| {
            c.install();
        })
    }

    fn run_script(&self, script: Scripts) -> BoxFuture<Result> {
        self.run_cmd(|c| {
            c.run(script.as_ref(), EMPTY_ARGS);
        })
    }
}


#[async_trait]
impl LocateGui2 for RepoRootAppGui2 {
    fn package_dir(&self) -> &Path {
        self.as_ref()
    }
}

impl LocateGui2 for crate::paths::generated::RepoRoot {
    fn package_dir(&self) -> &Path {
        self.app.gui_2.as_ref()
    }
}

impl LocateGui2 for Path {
    fn package_dir(&self) -> &Path {
        self
    }
}

pub async fn lint(path: &impl LocateGui2) -> Result {
    path.run_script(Scripts::Lint).await?;
    path.run_script(Scripts::TypeCheck).await
}

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
        job: BuildTargetJob<Self>,
    ) -> BoxFuture<'static, Result<Self::Artifact>> {
        let WithDestination { inner: _, destination } = job;
        async move {
            let gui2 = &context.repo_root.app.gui_2;
            gui2.install().await?;
            gui2.run_script(Scripts::Build).await?;
            ide_ci::fs::mirror_directory(
                &gui2.dist,
                &destination.join(RepoRootDistGui2Assets::segment_name()),
            )
            .await?;
            Ok(Artifact::new(destination))
        }
        .boxed()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::paths::generated::RepoRootAppGui2;
    use crate::repo::deduce_repository_path;

    #[tokio::test]
    async fn foo() -> Result {
        setup_logging()?;
        let repo_root = deduce_repository_path()?;
        info!("repo_root = {}", repo_root.display());
        let gui2 = RepoRootAppGui2::new(&repo_root);
        gui2.install().await?;
        gui2.run_script(Scripts::Build).await?;
        Ok(())
    }
}
