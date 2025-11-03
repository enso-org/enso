use enso_build::prelude::*;

use crate::arg::ArgExt;
use crate::arg::Source;
use crate::source_args_hlp;

use clap::Args;
use clap::Subcommand;
use enso_build::engine::BenchmarkType;
use enso_build::project;
use enso_build::project::backend::Backend;

#[derive(Args, Clone, Debug, PartialEq)]
#[group(skip)]
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
        bench_type: BenchmarkType,
        #[clap(enso_env())]
        bench_name: Option<String>,
    },
    /// Run the tests.
    Test {
        #[clap(value_enum, required = true)]
        which: Vec<enso_build::engine::Tests>,
    },
    /// Run an SBT command.
    Sbt {
        #[clap(last = true)]
        args: Vec<String>,
    },
    /// Build Engine Distribution
    CiBuildEngineDistribution {},
    /// Perform the stdlib API checks
    StdlibApiCheck {},

    /// Generate Cloud credentials
    GenerateCloudCredentials {},
}

#[derive(Args, Clone, Debug, PartialEq)]
pub struct Target {
    /// Command for backend package.
    #[clap(subcommand)]
    pub command: Command,
}
