use crate::prelude::*;

use crate::new_command_type;


// ==============
// === Export ===
// ==============

pub mod clean;

pub use clean::Clean;



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
    pub fn init(&self, path: impl AsRef<Path>) -> Result<GitCommand> {
        let mut cmd = self.cmd()?;
        cmd.arg(Command::Init);
        cmd.current_dir(path);
        Ok(cmd)
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
    working_dir:     PathBuf,
}

impl Context {
    /// Initialize a new command invoking git.
    pub fn cmd(&self) -> Result<GitCommand> {
        Ok(Git.cmd()?.with_current_dir(&self.working_dir))
    }

    /// Create a wrapper with explicitly set repository root and working directory.
    ///
    /// The caller is responsible for ensuring that the `working_dir` is a subdirectory of the
    /// `repository_root`.
    pub async fn new_unchecked(
        repository_root: impl AsRef<Path>,
        working_dir: impl AsRef<Path>,
    ) -> Self {
        Self {
            repository_root: repository_root.as_ref().to_path_buf(),
            working_dir:     working_dir.as_ref().to_path_buf(),
        }
    }

    /// Create a `Git` invocation context within a given directory.
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

    pub async fn new_current() -> Result<Self> {
        Context::new(crate::env::current_dir()?).await
    }

    pub async fn head_hash(&self) -> Result<String> {
        self.cmd()?.args(["rev-parse", "--verify", "HEAD"]).output_ok().await?.single_line_stdout()
    }

    /// Fetch a branch from a remote repository.
    #[context("Failed to fetch branch {} from remote {}.", branch, remote)]
    pub async fn fetch_branch(&self, remote: &str, branch: &str) -> Result {
        self.cmd()?.args(["fetch", remote, branch]).run_ok().await
    }

    /// List of files that are different than the compared commit.
    #[context("Failed to list files that are different than {}.", compare_against.as_ref())]
    pub async fn diff_against(&self, compare_against: impl AsRef<str>) -> Result<Vec<PathBuf>> {
        let root = self.repository_root.as_path();
        Ok(self
            .cmd()?
            .args(["diff", "--name-only", compare_against.as_ref()])
            .output_ok()
            .await?
            .into_stdout_string()?
            .lines()
            .map(|line| root.join(line.trim()).normalize())
            .collect_vec())
    }

    pub async fn repository_root(&self) -> Result<PathBuf> {
        let output = self
            .cmd()?
            .args(["rev-parse", "--show-toplevel"])
            .output_ok()
            .await?
            .single_line_stdout()?;
        let path = PathBuf::from(output).normalize();
        Ok(path)
    }
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

#[derive(Clone, Copy, Debug)]
pub enum Command {
    /// Remove untracked files from the working tree.
    Clean,
    /// Show changes between commits, commit and working tree, etc.
    Diff,
    /// Create an empty Git repository or reinitialize an existing one.
    Init,
}

impl AsRef<OsStr> for Command {
    fn as_ref(&self) -> &OsStr {
        match self {
            Command::Clean => OsStr::new("clean"),
            Command::Diff => OsStr::new("diff"),
            Command::Init => OsStr::new("init"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
