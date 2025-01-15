use crate::prelude::*;

use crate::define_env_var;
use crate::program::command::Manipulator;



define_env_var! {
    /// Force the SBT server to start, avoiding `ServerAlreadyBootingException`.
    /// See: https://github.com/sbt/sbt/issues/6777#issuecomment-1613316167
    SBT_SERVER_FORCESTART, bool;
}

#[derive(Clone, Copy, Debug)]
pub struct ServerAutostart(pub bool);
impl Manipulator for ServerAutostart {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        let arg = "sbt.server.autostart";
        let arg = format!("-D{arg}={}", self.0);
        command.arg(arg);
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Sbt;

impl Program for Sbt {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &'static str {
        "sbt"
    }
}

impl Sbt {
    /// Format a string with a command that will execute all the given tasks concurrently.
    pub fn concurrent_tasks(tasks: impl IntoIterator<Item: AsRef<str>>) -> String {
        let mut ret = String::from("all");
        for task in tasks {
            ret.push(' ');
            ret.push_str(task.as_ref())
        }
        ret
    }

    /// Format a string with a command that will execute all the given tasks sequentially.
    pub fn sequential_tasks<'a>(tasks: impl IntoIterator<Item = &'a str>) -> String {
        tasks.into_iter().collect::<Vec<_>>().join("; ")
    }
}

#[derive(Clone, Debug)]
pub struct SystemProperty {
    pub name:  String,
    pub value: String,
}

impl SystemProperty {
    pub fn new(name: impl Into<String>, value: impl Into<String>) -> Self {
        Self { name: name.into(), value: value.into() }
    }
}

impl<'a> IntoIterator for &'a SystemProperty {
    type Item = String;
    type IntoIter = std::iter::Once<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        once(format!("-D{}={}", self.name, self.value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_concurrent_tasks() {
        let tasks = ["test", "syntaxJS/fullOptJS"];
        assert_eq!(Sbt::concurrent_tasks(tasks), "all test syntaxJS/fullOptJS");
    }
}
