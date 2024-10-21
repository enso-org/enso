use crate::prelude::*;

use unicase::UniCase;



/// A trait for a shell.
///
/// Examples are `cmd.exe` and `powershell.exe` on windows or `bash` elsewhere.
pub trait Shell: Program {
    /// Prepare a command that will run a script passed as further arguments.
    fn run_command(&self) -> Result<Command>;

    /// Get a command that will run the given script file.
    fn run_script(&self, script_path: impl AsRef<Path>) -> Result<Command>;

    /// Get a command that spawns the shell.
    fn run_shell(&self) -> Result<Command>;

    /// Pretty-print shell command that applies given change to environment variables.
    fn modify_env(&self, change: &crate::env::Modification) -> Result<String>;

    /// A piece of command that will be interpreted by shell to get the value of the environment
    /// variable.
    fn access_environment_variable(&self, name: &str) -> String;

    /// Pretty-print shell command that prepends given paths to the environment variable.
    ///
    /// # Warning
    /// The platform-specific separator for PATH-like variables is used. Cross-platform script
    /// generation is thus not possible at this point. If such a need arises, we should roll out
    /// our own equivalent of [`std::env::join_paths`].
    fn set_prepended_paths(
        &self,
        variable_name: impl AsRef<str>,
        paths: impl IntoIterator<Item: AsRef<OsStr>>,
    ) -> Result<String> {
        let variable_name = UniCase::new(variable_name.as_ref().to_string());
        let current_paths = OsString::from(self.access_environment_variable(&variable_name));
        let mut all_paths =
            paths.into_iter().map(|path| path.as_ref().to_os_string()).collect_vec();
        all_paths.push(current_paths);
        let new_expression = std::env::join_paths(all_paths)?;
        self.modify_env(&crate::env::Modification {
            variable_name,
            action: crate::env::Action::Set(new_expression.as_str().to_string()),
        })
    }
}

/// Execute a given shell script.
///
/// The kind of shell to use shall be determined by the file extension of the script.
/// Will fail if the file extension is not recognized or if the appropriate shell is not available.
pub fn run_script(script_path: impl AsRef<Path>) -> Result<Command> {
    let shell_kind = match script_path.as_ref().extension() {
        Some(extension) => Recognized::from_extension(extension),
        None => bail!(
            "Cannot deduce shell for script {}. Missing file extension.",
            script_path.as_ref().display()
        ),
    }?;
    shell_kind.run_script(script_path)
}

/// Predefined set of recognized shells.
#[derive(Copy, Clone, Debug)]
pub enum Recognized {
    Command,
    PowerShell,
    Bash,
}

impl Recognized {
    /// Determine the shell kind from the given file extension.
    pub fn from_extension(extension: impl AsRef<Path>) -> Result<Recognized> {
        Ok(match extension.as_ref().to_string_lossy().as_ref() {
            "cmd" | "bat" => Self::Command,
            "ps1" | "pwsh" => Self::PowerShell,
            "sh" => Self::Bash,
            extension => bail!("Unrecognized shell script extension: {}.", extension),
        })
    }

    /// Execute a script file using this shell.
    pub fn run_script(self, script_path: impl AsRef<Path>) -> Result<Command> {
        match self {
            Recognized::Command => crate::programs::Cmd.run_script(script_path),
            Recognized::PowerShell => crate::programs::PwSh.run_script(script_path),
            Recognized::Bash => crate::programs::Bash.run_script(script_path),
        }
    }
}
