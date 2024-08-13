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

new_command_type! {Npm, NpmCommand}

impl NpmCommand {
    pub fn install(&mut self) -> &mut Self {
        // // We must strip any UNC prefix, because CMD does not support having it as a current
        // // directory, and npm is effectively a CMD script wrapping the actual program. See:
        // // https://github.com/npm/cli/issues/3349
        // //
        // // If this becomes an issue, consider toggling `DisableUNCCheck` on win runner machines
        // and // revert this workaround. See also:
        // // https://www.ibm.com/support/pages/disableunccheck-registry-key-created-during-rational-synergy-installation
        // let path = dbg!(path.as_ref().strip_prefix(r"\\?\")).unwrap_or(path.as_ref());
        self.arg("install");
        self
    }
    pub fn workspace(&mut self, workspace: impl AsRef<OsStr>) -> &mut Self {
        self.arg("--workspace").arg(workspace)
    }
    pub fn run(&mut self, script_name: impl AsRef<OsStr>) -> &mut Self {
        self.arg("run").arg(script_name)
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Npm;

impl Program for Npm {
    type Command = NpmCommand;
    type Version = Version;

    fn executable_name(&self) -> &'static str {
        "npm"
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
