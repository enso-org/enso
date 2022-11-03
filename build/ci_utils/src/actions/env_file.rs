//! During the execution of a workflow, the runner generates temporary files that can be used to
//! perform certain actions. The path to these files are exposed via environment variables.
//!
//! See <https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#environment-files> for more information.

use crate::prelude::*;

use crate::actions::env;
use crate::env::new::PathBufVariable;



// ============================
// === GitHub-defined files ===
// ============================

/// Environment file that can be used to set environment variables for the subsequent steps of the
/// current job. See: <https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#setting-an-environment-variable>
pub static GITHUB_ENV: EnvironmentFile = EnvironmentFile::new(env::GITHUB_ENV);

/// Environment file used to set current step's output parameters. See: <https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#setting-an-output-parameter>
pub static GITHUB_OUTPUT: EnvironmentFile = EnvironmentFile::new(env::GITHUB_OUTPUT);

/// Environment file used to store job's summary. See: <https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#adding-a-job-summary>
pub static GITHUB_STEP_SUMMARY: EnvironmentFile = EnvironmentFile::new(env::GITHUB_STEP_SUMMARY);

/// File with environment variables that will be set for subsequent steps of the current job.
pub static GITHUB_PATH: EnvironmentFile = EnvironmentFile::new(env::GITHUB_PATH);


// =======================
// === EnvironmentFile ===
// =======================

/// Structure that handles access to the environment file.
///
/// Contains mutex for synchronization, so the different threads can access the file safely.
#[derive(Debug)]
pub struct EnvironmentFile {
    /// Environment variable that contains path to the file.
    pub env_var: PathBufVariable,
    /// Mutex for synchronization.
    mutex:       tokio::sync::Mutex<()>,
}

impl EnvironmentFile {
    /// Create a new environment file accessor.
    pub const fn new(env_var: PathBufVariable) -> Self {
        Self { env_var, mutex: tokio::sync::Mutex::const_new(()) }
    }

    /// Read the file contents.
    pub async fn read(&self) -> Result<String> {
        let _guard = self.mutex.lock().await;
        let path = self.env_var.get()?;
        crate::fs::tokio::read_to_string(path).await
    }

    /// Appends line to the file.
    pub async fn append_line(&self, line: impl AsRef<str>) -> Result {
        let _guard = self.mutex.lock().await;
        let path = self.env_var.get()?;
        let mut line = line.as_ref().to_string();
        if !line.ends_with('\n') {
            line.push('\n');
        };
        crate::fs::tokio::append(path, line).await
    }

    /// Append key-value pair to the file.
    ///
    /// Automatically generates a unique delimiter, so the value is allowed to contain `=` or
    /// newline characters.
    pub async fn append_key_value(&self, key: impl AsRef<str>, value: impl AsRef<str>) -> Result {
        let key = key.as_ref();
        let value = value.as_ref();
        let delimiter = format!("ghadelimiter_{}", Uuid::new_v4());
        ensure!(!key.contains(&delimiter), "Key cannot contain delimiter {}.", delimiter);
        ensure!(!value.contains(&delimiter), "Value cannot contain delimiter {}.", delimiter);
        let line = format!("{key}<<{delimiter}\n{value}\n{delimiter}");
        self.append_line(line).await
    }
}
