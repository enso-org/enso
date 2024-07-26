use enso_build::prelude::*;

use crate::arg::ArgExt;

use clap::Args;
use clap::Subcommand;
use enso_build::project::wasm::test::Browser;



#[derive(Subcommand, Clone, Debug, PartialEq)]
pub enum Command {
    /// Run the unit tests.
    Test {
        /// Skip the native (non-WASM) Rust tests.
        #[clap(long)]
        no_native: bool,
        /// Skip the WASM Rust tests.
        #[clap(long)]
        no_wasm:   bool,
        /// Which browsers should be used to run WASM tests.
        ///
        /// More than one browser can be specified.
        #[clap(long, enso_env(), value_enum, default_values_t = [Browser::Firefox])]
        browser:   Vec<Browser>,
    },
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    /// Command for WASM part of GUI (aka the Rust part).
    #[clap(subcommand, name = "command")]
    pub command: Command,
}
