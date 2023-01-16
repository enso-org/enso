//! Wrappers over git commands.
//!
//! As a reasonable starting points, please refer to:
//! * [`new`] to obtain a context for an existing repository.
//! * [`init`] to initialize a new repository.

use crate::prelude::*;

use crate::new_command_type;
use crate::programs::git::pretty_format::refs_from_decoration;
use crate::RECORD_SEPARATOR;


// ==============
// === Export ===
// ==============

pub mod clean;
pub mod pretty_format;



pub mod r#ref;

pub use clean::Clean;
pub use r#ref::Ref;

/// The default name of the remote repository.
///
/// If use case arises, we can make it configurable in the future.
const DEFAULT_REMOTE: &str = "origin";

/// Git version control system.
///
/// See: <https://git-scm.com/>
#[derive(Clone, Copy, Debug)]
pub struct Git;

impl Program for Git {
    type Command = GitCommand;
    fn executable_name(&self) -> &'static str {
        "git"
    }
}

impl Git {
    /// Create a new, empty git repository in the given directory.
    #[context("Failed to initialize git repository in {}", path.as_ref().display())]
    pub async fn init(&self, path: impl AsRef<Path>) -> Result<Context> {
        crate::fs::tokio::create_dir_if_missing(path.as_ref()).await?;
        self.cmd()?.arg(Command::Init).current_dir(path.as_ref()).run_ok().await?;
        Ok(Context {
            working_dir:     path.as_ref().to_path_buf(),
            repository_root: path.as_ref().to_path_buf(),
        })
    }

    /// Clone a repository into a new directory.
    #[context("Failed to clone git repository {} into {}.", url.as_str(), path.as_ref().display())]
    pub async fn clone(&self, path: impl AsRef<Path>, url: &Url) -> Result<Context> {
        let path = path.as_ref();
        crate::fs::tokio::create_dir_if_missing(path).await?;
        self.cmd()?.arg(Command::Clone).arg(url.as_str()).arg(path).run_ok().await?;
        Context::new(path).await
    }
}

/// The wrapper over `Git` program invocation context.
///
/// It is stateful (knowing both repository root and current directory locations), as they both are
/// needed to properly handle relative paths.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Context {
    /// The path to the repository root above the `working_dir`.
    ///
    /// Many paths that git returns are relative to the repository root.
    repository_root: PathBuf,
    /// Directory in which commands will be invoked.
    /// It might not be the repository root and it makes difference for many commands.
    /// If it is different from the repository root, it must be in its subdirectory.
    working_dir:     PathBuf,
}

impl Context {
    /// Initialize a new command invoking git.
    ///
    /// This should be used as a starting point for all git commands.
    /// The command comes with current working directory set, please do not override it.
    pub fn cmd(&self) -> Result<GitCommand> {
        Ok(Git.cmd()?.with_current_dir(&self.working_dir))
    }

    /// Create a wrapper with explicitly set repository root and working directory.
    ///
    /// The caller is responsible for ensuring that the `working_dir` is a subdirectory of the
    /// `repository_root`.
    pub fn new_unchecked(repository_root: impl AsRef<Path>, working_dir: impl AsRef<Path>) -> Self {
        Self {
            repository_root: repository_root.as_ref().to_path_buf(),
            working_dir:     working_dir.as_ref().to_path_buf(),
        }
    }

    /// Create a `git` invocation context within a given directory.
    ///
    /// The `working_dir` is the directory in which git commands will be invoked. It is expected to
    /// be a part of some git repository.
    pub async fn new(working_directory: impl Into<PathBuf>) -> Result<Self> {
        let working_directory = working_directory.into();
        // Faux `Git` instance to get the repository root.
        // TODO: should be nicer, likely instance should be separate from program.
        let temp_git = Context {
            working_dir:     working_directory.clone(),
            repository_root: working_directory,
        };
        let repo_root = temp_git.repository_root().await?;
        Ok(Context { repository_root: repo_root, working_dir: temp_git.working_dir })
    }

    /// Create a git context for the current working directory.
    pub async fn new_current() -> Result<Self> {
        Context::new(crate::env::current_dir()?).await
    }

    /// Get the hash of the current [HEAD commit](https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddefHEADaHEAD).
    #[context("Failed to get a HEAD hash.")]
    pub async fn head_hash(&self) -> Result<String> {
        self.cmd()?
            .arg(Command::RevParse)
            .args(["--verify", "HEAD"])
            .output_ok()
            .await?
            .single_line_stdout()
    }

    /// Fetch a branch from a remote repository.
    #[context("Failed to fetch branch {} from remote {}.", branch, remote)]
    pub async fn fetch_branch(&self, remote: &str, branch: &str) -> Result {
        self.cmd()?.arg(Command::Fetch).args([remote, branch]).run_ok().await
    }

    /// List of files that are different than the compared commit.
    #[context("Failed to list files that are different than {}.", compare_against.as_ref())]
    pub async fn diff_against(&self, compare_against: impl AsRef<str>) -> Result<Vec<PathBuf>> {
        let root = self.repository_root.as_path();
        Ok(self
            .cmd()?
            .arg(Command::Diff)
            .args(["--name-only", compare_against.as_ref()])
            .output_ok()
            .await?
            .into_stdout_string()?
            .lines()
            .map(|line| root.join(line.trim()).normalize())
            .collect_vec())
    }

    /// Get the repository root directory.
    pub async fn repository_root(&self) -> Result<PathBuf> {
        let output = self
            .cmd()?
            .arg(Command::RevParse)
            .args(["--show-toplevel"])
            .output_ok()
            .await?
            .single_line_stdout()?;
        let path = PathBuf::from(output).normalize();
        Ok(path)
    }

    /// Fetch the tags from the remote repository and prune the local tags that are no longer on the
    /// remote.
    pub async fn sync_tags(&self) -> Result {
        trace!("Fetching the remotes and pruning tags.");
        self.cmd()?.arg(Command::Fetch).args(["--prune", "--prune-tags", "--tags"]).run_ok().await
    }

    /// List the tags on the remote repository.
    ///
    /// Note that tag objects will appear twice: once for the tag object itself, and once in the
    /// dereferenced form (e.g. `refs/tags/enso-0.2.0^{}`), for the commit object that the tag
    /// points to.
    pub async fn list_remote_tags(&self) -> Result<Vec<RemoteLsEntry>> {
        let output = self
            .cmd()?
            .arg(Command::LsRemote)
            .args(["--tags", DEFAULT_REMOTE])
            .output_ok()
            .await?
            .into_stdout_string()?;
        output.lines().map(RemoteLsEntry::from_str).try_collect()
    }

    /// Get the commit history of the current branch.
    ///
    /// Fails if there are no commits in the repository.
    pub async fn log(&self) -> Result<Vec<LogEntry>> {
        use pretty_format::Placeholder::Hash;
        use pretty_format::Placeholder::RefNames;
        let fields = [Hash, RefNames];
        let history = self.log_fields(&fields).await?;

        history
            .into_iter()
            .map(|fields| {
                let mut fields = fields.into_iter();
                let hash = fields.next().context("Missing hash.")?;
                let refs = fields.next().context("Missing refs.")?;
                let refs = refs_from_decoration(&refs);
                let refs = refs.into_iter().map(|s| s.parse2()).try_collect()?;
                Ok(LogEntry { hash, refs })
            })
            .try_collect()
    }

    /// Get the commit history of the current branch with the given fields.
    pub async fn log_fields(
        &self,
        fields: &[pretty_format::Placeholder],
    ) -> Result<Vec<Vec<String>>> {
        let field_count = fields.len();
        let format = fields.iter().map(ToString::to_string).join(RECORD_SEPARATOR);
        let output = self
            .cmd()?
            .arg(Command::Log)
            .args(["--decorate=full", &format!("--pretty=format:{format}")])
            .output_ok()
            .await?
            .into_stdout_string()?;

        output
            .lines()
            .map(|line| {
                let fields = line.split(RECORD_SEPARATOR).map(ToString::to_string).collect_vec();
                ensure!(fields.len() == field_count, "Wrong number of fields in log entry.");
                Ok(fields)
            })
            .try_collect()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LogEntry {
    pub hash: String,
    pub refs: Vec<Ref>,
}


new_command_type!(Git, GitCommand);

impl GitCommand {
    pub fn clean(&mut self) -> &mut Self {
        self.arg(Command::Clean)
            .apply(&Clean::Ignored)
            .apply(&Clean::Force)
            .apply(&Clean::UntrackedDirectories)
    }
    pub fn nice_clean(&mut self) -> &mut Self {
        self.clean().apply(&Clean::Exclude(".idea".into()))
    }
}

/// A top-level command for git.
///
/// The full reference is available in the [official docs](https://git-scm.com/docs/git#_git_commands).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Command {
    /// Clone a repository into a new directory.
    Clone,
    /// Remove untracked files from the working tree.
    Clean,
    /// Show changes between commits, commit and working tree, etc.
    Diff,
    /// Create an empty Git repository or reinitialize an existing one.
    Init,
    /// Download objects and refs from another repository.
    Fetch,
    /// Show commit logs.
    Log,
    /// List references in a remote repository.
    LsRemote,
    /// Pick out and massage parameters.
    RevParse,
}

impl AsRef<OsStr> for Command {
    fn as_ref(&self) -> &OsStr {
        OsStr::new(match self {
            Command::Clone => "clone",
            Command::Clean => "clean",
            Command::Diff => "diff",
            Command::Init => "init",
            Command::Fetch => "fetch",
            Command::Log => "log",
            Command::LsRemote => "ls-remote",
            Command::RevParse => "rev-parse",
        })
    }
}

/// Create a new git wrapper for the repository owning the given path.
///
/// Path is not required to be a repository root, but it must be inside the repository.
///
/// # Examples
/// ```
/// use enso_build_base::prelude::*;
/// use ide_ci::programs::git;
/// use ide_ci::programs::Git;
/// #[tokio::main]
/// async fn main() -> Result {
///     let temp = tempfile::tempdir()?;
///     // There is no repository yet, cannot create a context.
///     assert!(git::new(temp.path()).await.is_err());
///
///     // Initialize a new repository in the temp path.
///     Git.init(temp.path()).await?;
///     let git = git::new(temp.path()).await?;
///     Ok(())
/// }
/// ```
pub async fn new(path: impl Into<PathBuf>) -> Result<Context> {
    Context::new(path).await
}

/// Reference available in a remote repository along with the associated commit ID.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RemoteLsEntry {
    /// Commit ID (hash).
    pub hash:  String,
    /// Reference name.
    pub r#ref: Ref,
}

/// Construct from a line of output of `git ls-remote`.
impl std::str::FromStr for RemoteLsEntry {
    type Err = anyhow::Error;

    #[context("Failed to parse remote ls entry from string: {}", line)]
    fn from_str(line: &str) -> std::result::Result<Self, Self::Err> {
        let mut parts = line.split_whitespace();
        let hash = parts.next().context("Missing hash")?.to_string();
        let r#ref = parts.next().context("Missing reference")?.parse2()?;
        ensure!(parts.next().is_none(), "Unexpected trailing extra parts.");
        Ok(Self { hash, r#ref })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn github_wrapping_non_directory() -> Result {
        let temp = tempfile::tempdir()?;
        let git = new(temp.path()).await;
        assert!(git.is_err());
        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn list_remote_tags_test() -> Result {
        let git = Context::new_current().await?;
        let tags = git.list_remote_tags().await?;
        let map: multimap::MultiMap<_, _> =
            tags.into_iter().map(|tag| (tag.hash, tag.r#ref)).collect();
        dbg!(&map);
        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn log_test() -> Result {
        let git = Context::new_current().await?;
        dbg!(git.log().await)?;
        Ok(())
    }


    #[tokio::test]
    #[ignore]
    async fn repo_root() -> Result {
        let git = Context::new(".").await?;
        let diff = git.repository_root().await?;
        println!("{:?}", diff);
        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn call_diff() -> Result {
        let git = Context::new(".").await?;
        let diff = git.diff_against("origin/develop").await?;
        println!("{:?}", diff);
        Ok(())
    }
}
