use crate::prelude::*;



pub trait Shell: Program {
    fn run_command(&self) -> Result<Command>;
    fn run_script(&self, script_path: impl AsRef<Path>) -> Result<Command>;
    fn run_shell(&self) -> Result<Command>;
}

// Deduces shell from file extension.
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


#[derive(Copy, Clone, Debug)]
pub enum Recognized {
    Command,
    PowerShell,
    Bash,
}

impl Recognized {
    pub fn from_extension(extension: impl AsRef<Path>) -> Result<Recognized> {
        Ok(match extension.as_ref().to_string_lossy().as_ref() {
            "cmd" | "bat" => Self::Command,
            "ps1" | "pwsh" => Self::PowerShell,
            "sh" => Self::Bash,
            extension => bail!("Unrecognized shell script extension: {}.", extension),
        })
    }

    pub fn run_script(self, script_path: impl AsRef<Path>) -> Result<Command> {
        match self {
            Recognized::Command => crate::programs::Cmd.run_script(script_path),
            Recognized::PowerShell => crate::programs::PwSh.run_script(script_path),
            Recognized::Bash => crate::programs::Bash.run_script(script_path),
        }
    }
}
