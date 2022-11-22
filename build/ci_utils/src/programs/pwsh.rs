use crate::prelude::*;



#[derive(Clone, Copy, Debug)]
pub struct PwSh;

pub mod arg {
    pub const RUN_COMMAND: &str = "-Command";
    pub const RUN_FILE: &str = "-File";
}

impl Program for PwSh {
    fn executable_name(&self) -> &'static str {
        "pwsh"
    }
    fn executable_name_fallback() -> Vec<&'static str> {
        vec!["powershell"]
    }
}

impl Shell for PwSh {
    fn run_command(&self) -> Result<Command> {
        let mut command = self.cmd()?;
        command.arg(arg::RUN_COMMAND);
        Ok(command)
    }

    fn run_script(&self, script_path: impl AsRef<Path>) -> Result<Command> {
        let mut command = self.run_command()?;
        command.arg(arg::RUN_FILE);
        command.arg(script_path.as_ref());
        Ok(command)
    }

    fn run_shell(&self) -> Result<Command> {
        self.cmd()
    }
}
