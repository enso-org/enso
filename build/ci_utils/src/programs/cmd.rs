use crate::prelude::*;

use crate::env;
use crate::env::Modification;
use crate::programs::cmd::args::RUN_COMMAND;

use std::process::Stdio;
use unicase::UniCase;



#[derive(Clone, Copy, Debug)]
pub struct Cmd;

lazy_static::lazy_static! {
    static ref COMMAND_GLUE: [OsString; 3] = ["&&", "cls", "&&"].map(<_>::into);
}

pub mod args {
    /// Turns echo off.
    pub const ECHO_OFF: &str = "/Q";

    /// Carries out the command specified by string and then terminates.
    pub const RUN_COMMAND: &str = "/C";
}


impl Program for Cmd {
    fn executable_name(&self) -> &'static str {
        "cmd"
    }
}

impl Shell for Cmd {
    fn run_command(&self) -> Result<Command> {
        let mut cmd = self.cmd()?;
        cmd.arg(RUN_COMMAND);
        Ok(cmd)
    }

    fn run_script(&self, script_path: impl AsRef<Path>) -> Result<Command> {
        let mut command = self.run_command()?;
        command.arg(script_path.as_ref());
        Ok(command)
    }

    fn run_shell(&self) -> Result<Command> {
        self.cmd()
    }
}

pub fn run_commands<Cmds, Arg>(commands: Cmds) -> anyhow::Result<Command>
where
    Cmds: IntoIterator<Item: IntoIterator<Item = Arg>>,
    Arg: AsRef<OsStr>, {
    let mut ret = Cmd.run_command()?;
    ret.stdin(Stdio::null()).stdout(Stdio::piped());

    let mut command_itr = commands.into_iter();
    if let Some(first) = command_itr.next() {
        ret.args(first);
    }
    for following_command in command_itr {
        add_next_command(&mut ret, following_command);
    }
    Ok(ret)
}

pub fn add_next_command(
    cmd: &mut Command,
    command: impl IntoIterator<Item: AsRef<OsStr>>,
) -> &mut Command {
    cmd.args(COMMAND_GLUE.iter()).args(command)
}

pub fn split_command_outputs(output: &[u8]) -> impl Iterator<Item = &[u8]> {
    const ASCII_FORM_FEED: u8 = 0xC;
    output.split(|byte| *byte == ASCII_FORM_FEED)
}

pub async fn compare_env(
    f: impl FnOnce(&mut Command) -> &mut Command,
) -> Result<Vec<Modification>> {
    let mut cmd = run_commands([["set"]])?;
    cmd.args(COMMAND_GLUE.iter());
    f(&mut cmd);
    add_next_command(&mut cmd, ["set"]);
    let output = cmd.output_ok().await?;
    let outputs =
        split_command_outputs(&output.stdout).map(std::str::from_utf8).try_collect_vec()?;

    ensure!(outputs.len() == 3, "Expected outputs from all 3 commands!");

    let mut environment_before = parse_dumped_env(
        outputs.first().ok_or_else(|| anyhow!("Missing initial environment dump!"))?,
    )?;
    let environment_after = parse_dumped_env(
        outputs.last().ok_or_else(|| anyhow!("Missing final environment dump!"))?,
    )?;

    // dbg!(&environment_after);
    let mut changes = environment_after
        .into_iter()
        .filter_map(|(variable_name, new_value)| {
            let path_like = is_path_like(&variable_name);
            let action = match environment_before.remove(&variable_name) {
                Some(old_value) =>
                    if new_value != old_value {
                        if path_like {
                            // Check which elements are new and whether they are prepended.
                            // todo!();
                            env::Action::PrependPaths(std::env::split_paths(&new_value).collect())
                        } else {
                            env::Action::Set(new_value)
                        }
                    } else {
                        return None;
                    },
                None if path_like =>
                    env::Action::PrependPaths(std::env::split_paths(&new_value).collect()),
                None => env::Action::Set(new_value),
            };
            Some(Modification { variable_name, action })
        })
        .collect_vec();

    changes.extend(
        environment_before
            .into_keys()
            .map(|variable_name| Modification { variable_name, action: env::Action::Remove }),
    );

    Ok(changes)
}

const PATH_LIKE: [&str; 4] = ["INCLUDE", "LIB", "LIBPATH", "PATH"];

pub fn is_path_like(variable_name: impl AsRef<str>) -> bool {
    let variable_name = UniCase::<&str>::from(variable_name.as_ref());
    PATH_LIKE.iter().any(|pathlike| UniCase::<&str>::from(*pathlike) == variable_name)
}

pub fn parse_dumped_env(output: &str) -> Result<BTreeMap<UniCase<String>, String>> {
    // debug!("Got env:\n{}\n\n\n", output);
    let non_empty_lines = output.lines().map(|line| line.trim()).filter(|line| !line.is_empty());
    non_empty_lines
        .map(|line| match line.split_once('=') {
            Some((name, value)) => Ok((name.into(), value.into())),
            _ => Err(anyhow!("Cannot parse line {}!", line)),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_like() {
        assert!(is_path_like("Path"));
        assert!(is_path_like("PATH"));
        assert!(is_path_like("PaTh"));
        assert!(!is_path_like("PETh"));
        assert!(!is_path_like("foo"));
    }
}
