//! Build logic for the "new GUI" (gui2) project.
//!
//! The new GUI is Vue.js-based and located under `app/gui2`.

use crate::prelude::*;

use crate::paths::generated::RepoRootAppGui2;
use crate::paths::generated::RepoRootAppGui2Dist;
use crate::paths::generated::RepoRootDistGui2Assets;
use crate::project::Context;
use crate::project::IsArtifact;
use crate::project::IsTarget;
use crate::source::BuildTargetJob;
use crate::source::WithDestination;

use ide_ci::ok_ready_boxed;
use ide_ci::program::EMPTY_ARGS;
use ide_ci::programs::node::NpmCommand;
use ide_ci::programs::Npm;
use ide_ci::programs::Npx;
use std::process::Stdio;



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



// ================
// === Location ===
// ================

/// Convenience trait for types that allow us locating the new GUI sources.
///
/// Provides helper methods for calling gui2-related commands.
#[async_trait]
pub trait Locate {
    fn package_dir(&self) -> &Path;

    /// Prepare command that will run `npm` in the new GUI's directory.
    fn npm(&self) -> Result<NpmCommand> {
        Ok(self.adjust_cmd(Npm.cmd()?))
    }

    /// Set common data for commands run in the new GUI's directory.
    fn adjust_cmd<Cmd: IsCommandWrapper>(&self, cmd: Cmd) -> Cmd {
        cmd.with_current_dir(self.package_dir()).with_stdin(Stdio::null())
    }

    /// Run an NPM command in the new GUI's directory.
    fn run_cmd(&self, adjust_cmd: impl FnOnce(&mut NpmCommand)) -> BoxFuture<Result> {
        self.npm()
            .and_then_async(|mut cmd| {
                adjust_cmd(&mut cmd);
                cmd.run_ok()
            })
            .boxed()
    }

    /// Run `npm install` in the new GUI's directory.
    fn install(&self) -> BoxFuture<Result> {
        self.run_cmd(|c| {
            c.install();
        })
    }

    /// Run `npm run <script>` in the new GUI's directory.
    fn run_script(&self, script: Scripts) -> BoxFuture<Result> {
        self.run_script_with(script, EMPTY_ARGS)
    }

    /// Run `npm run <script> <args>` in the new GUI's directory.
    fn run_script_with(
        &self,
        script: Scripts,
        args: impl IntoIterator<Item: AsRef<OsStr>>,
    ) -> BoxFuture<Result> {
        self.run_cmd(|c| {
            c.run(script.as_ref(), args);
        })
    }
}


impl Locate for RepoRootAppGui2 {
    fn package_dir(&self) -> &Path {
        self.as_ref()
    }
}

impl Locate for crate::paths::generated::RepoRoot {
    fn package_dir(&self) -> &Path {
        self.app.gui_2.as_ref()
    }
}

impl Locate for Path {
    fn package_dir(&self) -> &Path {
        self
    }
}



// ================
// === Commands ===
// ================

/// Run steps that should be done along with the "lint"
pub fn lint(path: &impl Locate) -> BoxFuture<'static, Result> {
    let path = path.package_dir().to_owned();
    async move {
        path.install().await?;
        path.run_script(Scripts::TypeCheck).await?;
        path.run_script(Scripts::Lint).await
    }
    .boxed()
}

pub fn unit_tests(path: &impl Locate) -> BoxFuture<'static, Result> {
    let path = path.package_dir().to_owned();
    async move {
        path.install().await?;
        path.run_script_with(Scripts::TestUnit, ["run"]).await
    }
    .boxed()
}

pub fn end_to_end_tests(path: &impl Locate) -> BoxFuture<'static, Result> {
    let path = path.package_dir().to_owned();
    async move {
        path.install().await?;
        let install_playwright = ["playwright", "install"];
        path.adjust_cmd(Npx.cmd()?).args(install_playwright).run_ok().await?;
        path.run_script_with(Scripts::TestE2e, ["run"]).await
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
        job: BuildTargetJob<Self>,
    ) -> BoxFuture<'static, Result<Self::Artifact>> {
        let WithDestination { inner: _, destination } = job;
        async move {
            let gui2 = &context.repo_root.app.gui_2;
            gui2.install().await?;
            gui2.run_script(Scripts::BuildRustFfi).await?;
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



// ============
// === Test ===
// ============

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
        unit_tests(&gui2).await?;
        // gui2.run_script(Scripts::Build).await?;
        Ok(())
    }
}
