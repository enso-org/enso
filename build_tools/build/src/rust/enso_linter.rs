use super::*;

use crate::paths::generated::RepoRoot;

use ide_ci::programs::cargo;
use ide_ci::programs::Cargo;



const LINTER_CRATE_NAME: &str = "enso-parser-debug";
const LINTER_BIN_NAME: &str = "check_syntax";
const ENSO_EXTENSION: &str = ".enso";

pub fn cargo_run_linter_cmd(repo_root: &Path) -> Result<Command> {
    let mut ret = Cargo.cmd()?;
    ret.current_dir(repo_root)
        .apply(&cargo::Command::Run)
        .apply(&cargo::Options::Package(LINTER_CRATE_NAME.into()))
        .apply(&cargo::RunOption::Bin(LINTER_BIN_NAME.into()))
        .apply(&cargo::RunOption::Release);
    Ok(ret)
}

fn collect_source_paths(repo_root: &RepoRoot) -> Result<Vec<PathBuf>> {
    let glob_pattern = format!("**/*{ENSO_EXTENSION}");
    let source_roots = [&repo_root.distribution.lib.path, &repo_root.test.path];
    let glob_paths: Vec<_> = source_roots
        .into_iter()
        .map(|path| glob::glob(path.join(&glob_pattern).as_str()))
        .collect::<std::result::Result<_, _>>()?;
    Ok(glob_paths.into_iter().flatten().collect::<std::result::Result<_, _>>()?)
}

pub async fn lint_all(repo_root: RepoRoot) -> Result<()> {
    let sources = collect_source_paths(&repo_root)?;
    cargo_run_linter_cmd(&repo_root)?.arg("--").args(sources).run_ok().await?;
    Ok(())
}
