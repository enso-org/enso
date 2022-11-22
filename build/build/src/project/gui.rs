use crate::prelude::*;

use crate::ide::web::IdeDesktop;
use crate::project::Context;
use crate::project::IsArtifact;
use crate::project::IsTarget;
use crate::project::IsWatchable;
use crate::project::IsWatcher;
use crate::project::PerhapsWatched;
use crate::project::Wasm;
use crate::source::BuildTargetJob;
use crate::source::GetTargetJob;
use crate::source::Source;
use crate::source::WatchTargetJob;
use crate::source::WithDestination;
use crate::BoxFuture;

use derivative::Derivative;
use futures_util::future::try_join;
use ide_ci::ok_ready_boxed;



#[derive(Clone, Debug, PartialEq, Eq, Hash, Shrinkwrap)]
pub struct Artifact(crate::paths::generated::RepoRootDistGui);

impl AsRef<Path> for Artifact {
    fn as_ref(&self) -> &Path {
        self.0.as_path()
    }
}

impl IsArtifact for Artifact {}

impl Artifact {
    pub fn new(gui_path: impl AsRef<Path>) -> Self {
        // TODO: sanity check
        Self(crate::paths::generated::RepoRootDistGui::new_root(gui_path.as_ref()))
    }
}

#[derive(Clone, Derivative, derive_more::Deref)]
#[derivative(Debug)]
pub struct WatchInput {
    #[deref]
    pub wasm:  <Wasm as IsWatchable>::WatchInput,
    /// Rather than start web watcher, spawn an interactive shell.
    pub shell: bool,
}

#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub struct BuildInput {
    #[derivative(Debug = "ignore")]
    pub wasm:       GetTargetJob<Wasm>,
    /// BoxFuture<'static, Result<wasm::Artifact>>,
    #[derivative(Debug = "ignore")]
    pub build_info: BoxFuture<'static, Result<BuildInfo>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Gui;

#[async_trait]
impl IsTarget for Gui {
    type BuildInput = BuildInput;
    type Artifact = Artifact;

    fn artifact_name(&self) -> String {
        "gui".into()
    }

    fn adapt_artifact(self, path: impl AsRef<Path>) -> BoxFuture<'static, Result<Self::Artifact>> {
        ok_ready_boxed(Artifact::new(path))
    }

    fn build_internal(
        &self,
        context: Context,
        job: BuildTargetJob<Self>,
    ) -> BoxFuture<'static, Result<Self::Artifact>> {
        let WithDestination { inner, destination } = job;
        async move {
            let ide = ide_desktop_from_context(&context);
            let wasm = Wasm.get(context, inner.wasm);
            ide.build_content(wasm, &inner.build_info.await?, &destination).await?;
            Ok(Artifact::new(destination))
        }
        .boxed()
    }
}

#[derive(Debug)]
pub struct Watcher {
    pub wasm: PerhapsWatched<Wasm>,
    pub web:  crate::project::Watcher<Gui, crate::ide::web::Watcher>,
}

impl AsRef<Artifact> for Watcher {
    fn as_ref(&self) -> &Artifact {
        &self.web.artifact
    }
}

impl IsWatcher<Gui> for Watcher {
    fn wait_for_finish(&mut self) -> BoxFuture<Result> {
        let Self { web, wasm } = self;
        try_join(wasm.wait_ok(), IsWatcher::wait_for_finish(web)).void_ok().boxed()
    }
}

impl IsWatchable for Gui {
    type Watcher = Watcher;
    type WatchInput = WatchInput;

    // fn setup_watcher(
    //     &self,
    //     build_input: Self::BuildInput,
    //     watch_input: Self::WatchInput,
    //     output_path: impl AsRef<Path> + Send + Sync + 'static,
    // ) -> BoxFuture<'static, Result<Self::Watcher>> {
    //     async move {
    //         let BuildInput { build_info, repo_root, wasm } = build_input;
    //         let ide = IdeDesktop::new(&repo_root.app.ide_desktop);
    //         let watch_process = ide.watch_content(wasm, &build_info.await?).await?;
    //         let artifact = Self::Artifact::from_existing(output_path).await?;
    //         Ok(Self::Watcher { watch_process, artifact })
    //     }
    //     .boxed()
    // }

    fn watch(
        &self,
        context: Context,
        job: WatchTargetJob<Self>,
    ) -> BoxFuture<'static, Result<Self::Watcher>> {
        let WatchTargetJob { watch_input, build: WithDestination { inner, destination } } = job;
        let BuildInput { build_info, wasm } = inner;
        let perhaps_watched_wasm = perhaps_watch(Wasm, context.clone(), wasm, watch_input.wasm);
        let ide = ide_desktop_from_context(&context);
        async move {
            let perhaps_watched_wasm = perhaps_watched_wasm.await?;
            let wasm_artifacts = ok_ready_boxed(perhaps_watched_wasm.as_ref().clone());
            let watch_process =
                ide.watch_content(wasm_artifacts, &build_info.await?, watch_input.shell).await?;
            let artifact = Artifact::new(&destination);
            let web_watcher = crate::project::Watcher { watch_process, artifact };
            Ok(Self::Watcher { wasm: perhaps_watched_wasm, web: web_watcher })
        }
        .boxed()
    }
}

pub fn perhaps_watch<T: IsWatchable>(
    target: T,
    context: Context,
    job: GetTargetJob<T>,
    watch_input: T::WatchInput,
) -> BoxFuture<'static, Result<PerhapsWatched<T>>> {
    match job.inner {
        Source::BuildLocally(local) => target
            .watch(context, WatchTargetJob {
                watch_input,
                build: WithDestination { inner: local, destination: job.destination },
            })
            .map_ok(PerhapsWatched::Watched)
            .boxed(),
        Source::External(external) => target
            .get_external(context, WithDestination {
                inner:       external,
                destination: job.destination,
            })
            .map_ok(PerhapsWatched::Static)
            .boxed(),
    }
}

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
