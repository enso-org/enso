use crate::prelude::*;

use crate::paths::generated::RepoRoot;

use ide_ci::programs::Npm;



pub fn install_and_run_prettier(repo_root: &RepoRoot, script: &str) -> BoxFuture<'static, Result> {
    let prettier_dir = repo_root.build.prettier.to_path_buf();
    let script = script.to_string();
    async move {
        let no_args: [&str; 0] = [];
        Npm.cmd()?.current_dir(&prettier_dir).install().run_ok().await?;
        Npm.cmd()?.current_dir(&prettier_dir).run(script, no_args).run_ok().await?;
        Ok(())
    }
    .boxed()
}

pub fn check(repo_root: &RepoRoot) -> BoxFuture<'static, Result> {
    install_and_run_prettier(repo_root, "check")
}

pub fn write(repo_root: &RepoRoot) -> BoxFuture<'static, Result> {
    install_and_run_prettier(repo_root, "write")
}
