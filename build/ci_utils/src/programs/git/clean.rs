use crate::prelude::*;

use crate::path::trie::Trie;
use crate::program::command::Manipulator;
use crate::programs::Git;

use std::path::Component;



#[derive(Clone, Debug)]
pub struct DirectoryToClear<'a> {
    pub prefix: Vec<Component<'a>>,
    pub trie:   &'a Trie<'a>,
}

/// Run ``git clean -xfd`` but preserve the given paths.
///
/// This may involve multiple git clean calls on different subtrees.
/// Given paths can be either absolute or relative. If relative, they are relative to the
/// repository root.
pub async fn clean_except_for(
    repo_root: impl AsRef<Path>,
    paths: impl IntoIterator<Item: AsRef<Path>>,
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

    let trie = Trie::from_iter(relative_exclusions.iter());

    let mut directories_to_clear = vec![DirectoryToClear { prefix: vec![], trie: &trie }];
    while let Some(DirectoryToClear { prefix, trie }) = directories_to_clear.pop() {
        let current_dir = root.join_iter(&prefix);
        let exclusions_in_current_dir =
            trie.children.keys().map(|c| Clean::Exclude(c.as_os_str().to_string_lossy().into()));
        let git = Git::new(&current_dir).await?;
        git.cmd()?.clean().apply_iter(exclusions_in_current_dir).run_ok().await?;

        for (child_name, child_trie) in trie.children.iter() {
            if !child_trie.is_leaf() {
                let mut prefix = prefix.clone();
                prefix.push(*child_name);
                directories_to_clear.push(DirectoryToClear { prefix, trie: child_trie });
            }
        }
    }

    Ok(())
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
