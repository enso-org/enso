use crate::prelude::*;

use crate::path::trie::Trie;
use crate::program::command::Manipulator;
use crate::programs::git;

use std::path::Component;



#[derive(Clone, Debug)]
pub struct DirectoryToClear<'a> {
    pub prefix: Vec<Component<'a>>,
    pub trie:   &'a Trie<'a>,
}

/// Run `git clean -xfd` but preserve the given paths.
///
/// This may involve multiple git clean calls on different subtrees.
/// Given paths can be either absolute or relative. If relative, they are relative to the
/// repository root.
pub async fn clean_except_for(
    repo_root: impl AsRef<Path>,
    paths: impl IntoIterator<Item: AsRef<Path>>,
    dry_run: bool,
) -> Result {
    let root = repo_root.as_ref().canonicalize()?;

    let relative_exclusions: Vec<PathBuf> = paths
        .into_iter()
        // We use filter_map, so invalid (e.g. not existing) paths are ignored.
        .filter_map(|p| {
            let path: &Path = p.as_ref();
            // If we get a relative path, we treat it as relative to the repository root.
            let canonical_path =
                if path.is_relative() { root.join(path) } else { path.to_path_buf() }
                    .canonicalize()
                    .ok()?;

            canonical_path.strip_prefix(&root).ok().map(ToOwned::to_owned)
        })
        .collect_vec();

    let exclusions = relative_exclusions.into_iter().map(Clean::exclude).collect_vec();

    git::Context::new(root)
        .await?
        .cmd()?
        .nice_clean()
        .apply_iter(exclusions)
        .apply_opt(dry_run.then_some(&Clean::DryRun))
        .run_ok()
        .await
}

#[derive(Clone, Debug)]
pub enum Clean {
    /// Normally, when no path is specified, `git clean` will not recurse into untracked
    /// directories to avoid removing too much. Specify this option to have it recurse into such
    /// directories as well. If any paths are specified, this option is irrelevant; all untracked
    /// files matching the specified paths (with exceptions for nested git directories mentioned
    /// under `Force`) will be removed.
    UntrackedDirectories,

    /// If the Git configuration variable clean.requireForce is not set to false, git clean will
    /// refuse to delete files or directories unless given `Force` or `Interactive`. Git will
    /// refuse to modify untracked nested git repositories (directories with a .git subdirectory)
    /// unless a second `Force` is given.
    Force,

    /// Show what would be done and clean files interactively.
    Interactive,

    /// Don’t actually remove anything, just show what would be done.
    DryRun,

    /// Use the given exclude pattern in addition to the standard ignore rules.
    Exclude(String),

    /// Don’t use the standard ignore rules, but still use the ignore rules given with `Exclude`
    /// options from the command line. This allows removing all untracked files, including build
    /// products. This can be used (possibly in conjunction with git restore or git reset) to
    /// create a pristine working directory to test a clean build.
    Ignored,

    /// Remove only files ignored by Git. This may be useful to rebuild everything from scratch,
    /// but keep manually created files.
    OnlyIgnored,
}

impl Clean {
    pub fn exclude(path: impl AsRef<Path>) -> Self {
        let mut ret = String::new();
        for component in path.as_ref().components() {
            ret.push('/');
            ret.push_str(&component.as_os_str().to_string_lossy());
        }
        Clean::Exclude(ret)
    }
}

impl Manipulator for Clean {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        // fn apply<'a, C: IsCommandWrapper + ?Sized>(&self, c: &'a mut C) -> &'a mut C {
        let args: Vec<&str> = match self {
            Clean::UntrackedDirectories => vec!["-d"],
            Clean::Force => vec!["-f"],
            Clean::Interactive => vec!["-i"],
            Clean::DryRun => vec!["-n"],
            Clean::Exclude(pattern) => vec!["-e", pattern.as_ref()],
            Clean::Ignored => vec!["-x"],
            Clean::OnlyIgnored => vec!["-X"],
        };
        command.args(args);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::programs::Git;

    #[tokio::test]
    async fn test_cleaning() -> Result {
        setup_logging()?;
        let dir = PathBuf::from(r"C:\temp\test_cleaning");
        crate::fs::tokio::reset_dir(&dir).await?;
        Git.init(&dir)?.run_ok().await?;

        let foo = dir.join("foo");
        let foo_target = foo.join("target");
        crate::fs::tokio::write(&foo_target, "target in foo").await?;

        let target = dir.join("target");
        let target_foo = target.join("foo");
        crate::fs::tokio::write(&target_foo, "foo in target").await?;

        clean_except_for(&dir, vec!["target/foo"], false).await?;



        Ok(())
    }
}
