use crate::prelude::*;

use crate::env::accessor::Separated;
use crate::program::command::Manipulator;


// ==============
// === Export ===
// ==============

pub mod build_env;
pub mod clippy;
pub mod fmt;



/// Extra flags that Cargo invokes rustc with.
///
/// See: <https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-reads>
pub const CARGO_ENCODED_RUSTFLAGS: Separated =
    Separated { separator: "\x1F", name: "CARGO_ENCODED_RUSTFLAGS" };

pub const RUSTFLAGS: Separated = Separated { separator: " ", name: "RUSTFLAGS" };

#[derive(Clone, Copy, Debug, Default)]
pub struct Cargo;

impl Program for Cargo {
    fn init_command<'a>(&self, cmd: &'a mut Self::Command) -> &'a mut Self::Command {
        Color::Always.apply(cmd);
        cmd
    }
    fn executable_name(&self) -> &'static str {
        "cargo"
    }
}

/// Control when colored output is used.
#[derive(Clone, Copy, PartialEq, Eq, Debug, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum Command {
    /// Run the benchmarks
    Bench,
    /// Compile the current package
    Build,
    /// Analyze the current package and report errors, but don't build object files
    Check,
    /// Remove the target directory
    Clean,
    /// Build this package's and its dependencies' documentation
    Doc,
    /// Create a new cargo package
    New,
    /// Create a new cargo package in an existing directory
    Init,
    /// Run a binary or example of the local package
    Run,
    /// Run the tests
    Test,
    /// Update dependencies listed in Cargo.lock
    Update,
    /// Search registry for crates
    Search,
    /// Package and upload this package to the registry
    Publish,
    /// Install a Rust binary. Default location is $HOME/.cargo/bin
    Install,
    /// Uninstall a Rust binary
    Uninstall,
    /// Print a JSON representation of a Cargo.toml file's location
    LocateProject,
}

impl Manipulator for Command {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.arg(self.as_ref());
    }
}

/// Control when colored output is used.
#[derive(Clone, Copy, PartialEq, Eq, Debug, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum Color {
    /// Never display colors.
    None,
    /// Always display colors.
    Always,
    /// Automatically detect if color support is available on the terminal.
    Auto,
}

impl Manipulator for Color {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        command.args(["--color", self.as_ref()]);
    }
}

#[derive(Clone, PartialEq, Eq, Debug, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum Options {
    Workspace,
    Package(String),
    AllTargets,
}

impl Manipulator for Options {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        let base_arg = format!("--{}", self.as_ref());
        command.arg(base_arg);
        use Options::*;
        match self {
            Workspace | AllTargets => {}
            Package(package_name) => {
                command.arg(package_name.as_str());
            }
        }
    }
}

/// Options for the `cargo run` command.
#[derive(Clone, PartialEq, Eq, Debug, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum RunOption {
    /// Name of the bin target to run.
    Bin(String),
}

impl Manipulator for RunOption {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        let base_arg = format!("--{}", self.as_ref());
        command.arg(base_arg);
        use RunOption::*;
        match self {
            Bin(binary_name) => {
                command.arg(binary_name.as_str());
            }
        }
    }
}

/// The representation in which to print the project location.
#[derive(Clone, Copy, PartialEq, Eq, Debug, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum MessageFormat {
    /// JSON object with the path under the key "root".
    Json,
    /// Just the path.
    Plain,
}

/// Options for the `cargo locate-project` command.
#[derive(Clone, Copy, PartialEq, Eq, Debug, strum::AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum LocateProjectOption {
    /// Locate the Cargo.toml at the root of the workspace, as opposed to the current workspace
    /// member.
    Workspace,
    /// The representation in which to print the project location.
    MessageFormat(MessageFormat),
}

impl Manipulator for LocateProjectOption {
    fn apply<C: IsCommandWrapper + ?Sized>(&self, command: &mut C) {
        let base_arg = format!("--{}", self.as_ref());
        command.arg(base_arg);
        use LocateProjectOption::*;
        match self {
            Workspace => {}
            MessageFormat(format) => {
                command.arg(format.as_ref());
            }
        }
    }
}
