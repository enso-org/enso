use crate::prelude::*;

use crate::arg::OutputPath;
use crate::arg::Source;
use crate::arg::WatchJob;
use crate::source_args_hlp;

use clap::Args;
use clap::Subcommand;
use enso_build::project::backend::Backend;
use enso_build::project::gui::Gui;
use enso_build::project::wasm::DEFAULT_INTEGRATION_TESTS_WASM_TIMEOUT;
use octocrab::models::ReleaseId;



source_args_hlp!(Target, "ide", BuildInput);

#[derive(Args, Clone, Debug, PartialEq)]
pub struct BuildInput {
    #[clap(flatten)]
    pub gui:             Source<Gui>,
    #[clap(flatten)]
    pub project_manager: Source<Backend>,
    #[clap(flatten)]
    pub output_path:     OutputPath<Target>,
    /// Override the default target for electron-builder. E.g. pass `dir` for unpacked directory
    /// (fastest). See <https://www.electron.build> for all supported targets.
    #[clap(long, enso_env())]
    pub electron_target: Option<String>,
}

#[derive(Subcommand, Clone, Debug)]
pub enum Command {
    /// Builds both Project Manager and GUI, puts them together into a single, client Electron
    /// application.
    Build {
        #[clap(flatten)]
        params: BuildInput,
    },
    Upload {
        #[clap(flatten)]
        params:     BuildInput,
        #[clap(long, env = *enso_build::env::ENSO_RELEASE_ID)]
        release_id: ReleaseId,
    },
    /// Like `Build` but automatically starts the IDE.
    Start {
        #[clap(flatten)]
        params:     BuildInput,
        /// Additional option to be passed to Enso IDE. Can be used multiple times to pass many
        /// arguments.
        #[clap(long, allow_hyphen_values = true, enso_env())]
        ide_option: Vec<String>,
    },
    Watch {
        #[clap(flatten)]
        gui:             WatchJob<Gui>,
        #[clap(flatten)]
        project_manager: Source<Backend>,
        #[clap(long, allow_hyphen_values = true, enso_env())]
        ide_option:      Vec<String>,
    },
    /// Runs integration tests. This involves building and spawning Project Manager, unless
    /// requested otherwise.
    IntegrationTest {
        /// If set, the project manager won't be spawned.
        #[clap(long)]
        external_backend:  bool,
        #[clap(flatten)]
        project_manager:   Source<Backend>,
        /// Run WASM tests in the headless mode
        #[clap(long, parse(try_from_str), default_value_t = true)]
        headless:          bool,
        /// Custom timeout for wasm-bindgen test runner. Supports formats like "300secs" or "5min".
        #[clap(long, default_value_t = DEFAULT_INTEGRATION_TESTS_WASM_TIMEOUT.into())]
        wasm_timeout:      humantime::Duration,
        /// Additional options to be appended to the wasm-pack invocation. Note that wasm-pack will
        /// further redirect any unrecognized option to the underlying cargo call.
        #[clap(last = true)]
        wasm_pack_options: Vec<String>,
    },
}

#[derive(Args, Clone, Debug)]
pub struct Target {
    #[clap(subcommand)]
    pub command: Command,
}
