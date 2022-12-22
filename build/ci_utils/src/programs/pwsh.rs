/// Power Shell handling. See [`PwSh`] for details.
use crate::prelude::*;

use crate::env::Action;
use crate::env::Modification;



/// [PowerShell](https://github.com/PowerShell/PowerShell) program.
///
/// If Power Shell Core is installed, it will be used. Otherwise, Windows PowerShell will be used
/// as a fallback.
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

    fn modify_env(&self, change: &Modification) -> Result<String> {
        let name = &change.variable_name;
        Ok(match &change.action {
            Action::Remove => format!(r"Remove-Item Env:\{}", name),
            Action::Set(value) => {
                format!(r#"$env:{} = "{}""#, name, value)
            }
            Action::PrependPaths(paths) => self.set_prepended_paths(name, paths)?,
        })
    }

    fn access_environment_variable(&self, name: &str) -> String {
        format!(r"$env:{}", name)
    }
}


#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[cfg(target_os = "windows")]
    #[test]
    fn modify_env_commands() {
        let set_foobar = Modification {
            variable_name: "FOOBAR".into(),
            action:        Action::Set("foobar_value".into()),
        };
        let unset_foobar =
            Modification { variable_name: "FOOBAR".into(), action: Action::Remove };
        let prepend_path = Modification {
            variable_name: "PATH".into(),
            action:        Action::PrependPaths(vec!["C:\\foo".into(), "C:\\bar".into()]),
        };
        assert_eq!(PwSh.modify_env(&set_foobar).unwrap(), r#"$env:FOOBAR = "foobar_value""#);
        assert_eq!(PwSh.modify_env(&unset_foobar).unwrap(), r"Remove-Item Env:\FOOBAR");
        assert_eq!(
            PwSh.modify_env(&prepend_path).unwrap(),
            r#"$env:PATH = "C:\foo;C:\bar;$env:PATH""#
        );
    }
}
