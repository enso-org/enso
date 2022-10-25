use enso_build::prelude::*;

use crate::arg::ArgExt;
use crate::arg::Source;
use crate::source_args_hlp;

use clap::Args;
use clap::Subcommand;
use enso_build::project;
use enso_build::project::backend::Backend;



#[derive(Args, Clone, Debug, PartialEq)]
pub struct BuildInput {
    #[clap(flatten)]
    pub runtime: Source<project::Runtime>,
}

source_args_hlp!(Backend, "backend", BuildInput);

#[derive(Subcommand, Clone, Debug, PartialEq)]
pub enum Command {
    /// Build the backend from local sources.
    #[clap(alias = "get")]
    Build {
        #[clap(flatten)]
        source: Source<Backend>,
    },
    /// Build backend and upload it as a release asset. This command is intended to be run as part
    /// of the CI process.
    Upload {
        #[clap(flatten)]
        input: BuildInput,
    },
    /// Execute benchmarks.
    Benchmark {
        /// Execute benchmark code only once. This is not useful for benchmarking, but ensures that
        /// the benchmarks can execute without issues.
        #[clap(long, enso_env())]
        minimal_run: bool,
        #[clap(arg_enum)]
        which:       Vec<enso_build::engine::Benchmarks>,
    },
    /// Run the tests.
    Test {
        #[clap(arg_enum, required = true)]
        which: Vec<enso_build::engine::Tests>,
    },
    /// Run an SBT command.
    Sbt {
        #[clap(last = true)]
        command: Vec<String>,
    },
    /// Perform the CI check routine for the backend.
    CiCheck {},
}

#[derive(Args, Clone, Debug, PartialEq)]
pub struct Target {
    /// Command for backend package.
    #[clap(subcommand)]
    pub command: Command,
}
