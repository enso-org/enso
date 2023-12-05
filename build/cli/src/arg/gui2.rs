use enso_build::prelude::*;

use crate::arg::BuildJob;
use crate::arg::Source;
use crate::source_args_hlp;

use clap::clap_derive::ArgEnum;
use clap::Args;
use clap::Subcommand;
use enso_build::project::gui2::Gui2;



source_args_hlp!(Gui2, "gui2", BuildInput);

#[derive(Args, Clone, Copy, Debug, PartialEq)]
pub struct BuildInput {}

#[derive(ArgEnum, Clone, Copy, Debug, PartialEq)]
pub enum TestType {
    /// Run unit tests in watch mode
    Unit,
    /// Run E2E tests
    E2E,
    /// Run all tests without watching nor HTML reports.
    CI,
}

#[derive(Subcommand, Clone, Debug, PartialEq)]
pub enum Command {
    /// Builds the GUI from the local sources.
    Build(BuildJob<Gui2>),
    /// Gets the GUI, either by compiling it from scratch or downloading from an external source.
    Get(Source<Gui2>),
    /// Runs the GUI's unit tests.
    Test {
        /// What kind of test should be run?
        #[clap(long, arg_enum, default_value_t = TestType::Unit)]
        r#type: TestType,
    },
    /// Run linter on the GUI's sources.
    Lint,
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    /// Command for GUI package.
    #[clap(subcommand)]
    pub command: Command,
}
