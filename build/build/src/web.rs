//! Module for managing the web parts of our codebase.
//! This refers to JS/TS components, such as the Vue.js-based GUI, as well as the Electron client
//! application.

use crate::prelude::*;

use ide_ci::io::retry;
use ide_ci::programs::Pnpm;



/// Result of root-level `npm install` call. Should not be accessed directly.
static ONCE_INSTALL: tokio::sync::OnceCell<Result> = tokio::sync::OnceCell::const_new();

/// This method invokes `npm install` in the root repository.
///
/// It ensures that at most one such invocation is running at any given time.
/// If an invocation is already running, it waits for it to finish and then
/// shares its result.
///
/// This method relies on the internal global state. It must not be called multiple times
/// with different `repo_root` arguments. Also, `npm install` must not be invoked directly.
///
/// It is useful, because otherwise the build script might end up invoking `npm install` multiple
/// times, sometimes even in parallel. Unfortunately, in such cases, `npm` might fail with errors.
///
/// If the operation fails, which is possible due to network issues or other problems, the
/// installation will be retried up a few times.
pub fn install(repo_root: impl AsRef<Path>) -> BoxFuture<'static, Result> {
    let repo_root = repo_root.as_ref().to_owned();
    ONCE_INSTALL
        .get_or_init(move || retry(move || install_internal_run(&repo_root)))
        .map(|fut_res_ref| {
            fut_res_ref
                .as_ref()
                .map_err(|e| anyhow!("Failed to install NPM dependencies: {e:?}"))
                .copied()
        })
        .boxed()
}

/// Run `pnpm install` in the given directory.
fn install_internal_run(path: &Path) -> impl Future<Output = Result> + 'static {
    Pnpm.cmd().and_then_async(move |cmd| {
        let err = format!("Failed to install NPM dependencies in {}.", path.display());
        cmd.with_current_dir(path).install().run_ok().context(err)
    })
}

/// Mark the root repository's NPM as installed.
///
/// If invoked before `install`, any subsequent `install` call will return `Ok`.
/// If it was already installed and failed, the failure will still be returned.
pub fn assume_installed() {
    let _ = ONCE_INSTALL.set(Ok(())).inspect_err(|e| {
        warn!("Failed to mark NPM as installed, due to an error during a previous run: {e}");
    });
}

/// The scripts that can be invoked in the root repository's NPM.
///
/// The list should be kept in sync with the `scripts` section of the root repository's
/// `package.json`.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum Script {
    CiCheck,
    #[strum(serialize = "build:gui")]
    Build,
    Format,
    #[strum(serialize = "format:workflows")]
    FormatWorkflows,
}

/// Invoke the given script in the context of the root repository's NPM.
pub fn run_script(repo_root: impl AsRef<Path>, script: Script) -> BoxFuture<'static, Result> {
    let root_path = repo_root.as_ref().to_owned();
    async move { Pnpm.cmd()?.with_current_dir(&root_path).run(script.as_ref()).run_ok().await }
        .boxed()
}
