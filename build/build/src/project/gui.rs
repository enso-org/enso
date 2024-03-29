//! Build logic for the GUI project.
//!
//! The GUI is Vue.js-based and located under `app/gui2`.

use crate::prelude::*;

use crate::ide::web::IdeDesktop;
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
    Test,
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

pub fn dashboard_script(repo_root: impl AsRef<Path>, script: Scripts) -> Result<NpmCommand> {
    let mut ret = Npm.cmd()?;
    ret.current_dir(repo_root).workspace(crate::web::Workspace::EnsoDashboard).run(script.as_ref());
    Ok(ret)
}



// ================
// === Commands ===
// ================

/// Run steps that should be done along with the "lint"
pub fn lint(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    install_and_run_script(Scripts::Lint, repo_root)
}

pub fn tests(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    install_and_run_script(Scripts::Test, repo_root)
}

pub fn dashboard_tests(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    install_and_run_dashboard_script(Scripts::Test, repo_root)
}

/// Run unit tests.
pub fn unit_tests(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    install_and_run_script(Scripts::TestUnit, repo_root)
}

/// Run dashboard unit tests.
pub fn dashboard_unit_tests(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    install_and_run_dashboard_script(Scripts::TestUnit, repo_root)
}

/// Run E2E tests.
pub fn e2e_tests(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    install_and_run_script(Scripts::TestE2e, repo_root)
}

/// Run dashboard E2E tests.
pub fn dashboard_e2e_tests(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    install_and_run_dashboard_script(Scripts::TestE2e, repo_root)
}

pub fn watch(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    install_and_run_script(Scripts::Dev, repo_root)
}

fn install_and_run_script(
    script: Scripts,
    repo_root: impl AsRef<Path>,
) -> BoxFuture<'static, Result> {
    let repo_root = repo_root.as_ref().to_owned();
    async move {
        crate::web::install(&repo_root).await?;
        self::script(&repo_root, script)?.run_ok().await
    }
    .boxed()
}

fn install_and_run_dashboard_script(
    script: Scripts,
    repo_root: impl AsRef<Path>,
) -> BoxFuture<'static, Result> {
    let repo_root = repo_root.as_ref().to_owned();
    async move {
        crate::web::install(&repo_root).await?;
        dashboard_script(&repo_root, script)?.run_ok().await
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
            crate::ide::web::google_font::install_with_css(
                &context.cache,
                &context.octocrab,
                "mplus1",
                "M PLUS 1",
                "/font-mplus1",
                &repo_root.app.gui_2.public.font_mplus_1,
                &repo_root.app.gui_2.src.assets.font_mplus_1_css,
            )
            .await?;
            crate::ide::web::dejavu_font::install_sans_mono_with_css(
                &context.cache,
                &context.octocrab,
                "/font-dejavu",
                &repo_root.app.gui_2.public.font_dejavu,
                &repo_root.app.gui_2.src.assets.font_dejavu_css,
            )
            .await?;
            crate::ide::web::enso_font::install_with_css(
                &context.cache,
                &context.octocrab,
                "/font-enso",
                &repo_root.app.gui_2.public.font_enso,
                &repo_root.app.gui_2.src.assets.font_enso_css,
            )
            .await?;
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

// =================
// === BuildInfo ===
// =================
#[derive(Clone, Derivative, Serialize, Deserialize)]
#[derivative(Debug)]
#[serde(rename_all = "camelCase")]
pub struct BuildInfo {
    pub commit:         String,
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub version:        Version,
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub engine_version: Version,
    pub name:           String,
}

pub fn ide_desktop_from_context(context: &Context) -> IdeDesktop {
    IdeDesktop::new(&context.repo_root, context.octocrab.clone(), context.cache.clone())
}
