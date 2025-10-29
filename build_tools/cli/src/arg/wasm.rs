use clap::Args;
use clap::Subcommand;

#[derive(Subcommand, Clone, Debug, PartialEq)]
pub enum Command {
    /// Run the unit tests.
    Test,
    Lint,
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    /// Command for WASM part of GUI (aka the Rust part).
    #[clap(subcommand, name = "command")]
    pub command: Command,
}
