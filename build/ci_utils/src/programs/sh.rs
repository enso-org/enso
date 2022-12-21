use crate::prelude::*;

use crate::env::Action;
use crate::env::Modification;



#[derive(Clone, Copy, Debug)]
pub struct Sh;

impl Program for Sh {
    fn executable_name(&self) -> &'static str {
        "sh"
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Bash;

impl Program for Bash {
    fn executable_name(&self) -> &'static str {
        "bash"
    }
}

impl Shell for Bash {
    fn run_command(&self) -> Result<Command> {
        let mut cmd = Bash.cmd()?;
        cmd.arg("-c");
        Ok(cmd)
    }

    fn run_script(&self, script_path: impl AsRef<Path>) -> Result<Command> {
        let mut cmd = Bash.cmd()?;
        cmd.arg(script_path.as_ref());
        Ok(cmd)
    }

    fn run_shell(&self) -> Result<Command> {
        self.cmd()
    }

    fn modify_env(&self, change: &Modification) -> Result<String> {
        let name = &change.variable_name;
        Ok(match &change.action {
            Action::Remove => format!("unset {}", name),
            Action::Set(value) => {
                format!("export {}={}", name, value)
            }
            Action::PrependPaths(paths) => self.set_prepended_paths(name, paths)?,
        })
    }

    fn access_environment_variable(&self, name: &str) -> String {
        format!("${}", name)
    }
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;


    #[cfg(not(target_os = "windows"))]
    #[test]
    fn test_modify_env() {
        let set_foobar = Modification {
            variable_name: "FOOBAR".into(),
            action:        Action::Set("foobar_value".into()),
        };
        let unset_foobar =
            Modification { variable_name: "FOOBAR".into(), action: Action::Remove };
        let prepend_path = Modification {
            variable_name: "PATH".into(),
            action:        Action::PrependPaths(vec!["/foo".into(), "/bar".into()]),
        };
        assert_eq!(Bash.modify_env(&set_foobar).unwrap(), "export FOOBAR=foobar_value");
        assert_eq!(Bash.modify_env(&unset_foobar).unwrap(), "unset FOOBAR");
        assert_eq!(Bash.modify_env(&prepend_path).unwrap(), "export PATH=/foo:/bar:$PATH");
    }
}
