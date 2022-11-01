use crate::prelude::*;

use crate::new_command_type;


// ==============
// === Export ===
// ==============

pub mod clean;

pub use clean::Clean;



#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Git {
    /// The path to the repository root above the `working_dir`.
    ///
    /// Many paths that git returns are relative to the repository root.
    repo_path:   PathBuf,
    /// Directory in which commands will be invoked.
    /// It might not be the repository root and it makes difference for many commands.
    working_dir: PathBuf,
}

impl Program for Git {
    type Command = GitCommand;
    fn executable_name(&self) -> &'static str {
        "git"
    }
    fn current_directory(&self) -> Option<PathBuf> {
        Some(self.working_dir.clone())
    }
}

impl Git {
    pub async fn new(repo_path: impl Into<PathBuf>) -> Result<Self> {
        let repo_path = repo_path.into();
        let temp_git = Git { working_dir: repo_path.clone(), repo_path };
        let repo_path = temp_git.repository_root().await?;
        Ok(Git { repo_path, working_dir: temp_git.working_dir })
    }

    pub async fn new_current() -> Result<Self> {
        Git::new(crate::env::current_dir()?).await
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
        let root = self.repo_path.as_path();
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
    Clean,
}

impl AsRef<OsStr> for Command {
    fn as_ref(&self) -> &OsStr {
        match self {
            Command::Clean => OsStr::new("clean"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore]
    async fn repo_root() -> Result {
        let git = Git::new(".").await?;
        let diff = git.repository_root().await?;
        println!("{:?}", diff);
        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn call_diff() -> Result {
        let git = Git::new(".").await?;
        let diff = git.diff_against("origin/develop").await?;
        println!("{:?}", diff);
        Ok(())
    }
}
