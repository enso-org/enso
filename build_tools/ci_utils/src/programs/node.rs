use crate::prelude::*;

use crate::new_command_type;

#[derive(Clone, Copy, Debug, Default)]
pub struct Node;

impl Program for Node {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &'static str {
        "node"
    }
}

new_command_type! {Pnpm, PnpmCommand}

impl PnpmCommand {
    pub fn install(&mut self) -> &mut Self {
        self.arg("pnpm").arg("i");
        self
    }

    pub fn run(&mut self, script_name: impl AsRef<OsStr>) -> &mut Self {
        self.arg("pnpm").arg("run").arg(script_name);
        self
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Pnpm;

impl Program for Pnpm {
    type Command = PnpmCommand;
    type Version = Version;

    fn executable_name(&self) -> &'static str {
        "corepack"
    }

    fn pretty_name() -> Option<&'static str> {
        Some("pnpm")
    }
}
