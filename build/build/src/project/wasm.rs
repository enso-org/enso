//! Wrappers over the Rust part of the IDE codebase.

use crate::prelude::*;

use ide_ci::programs::cargo;
use ide_ci::programs::Cargo;


// ==============
// === Export ===
// ==============

pub mod env;
pub mod test;



#[derive(
    clap::ValueEnum,
    Clone,
    Copy,
    Debug,
    Default,
    strum::Display,
    strum::EnumString,
    PartialEq,
    Eq
)]
#[strum(serialize_all = "kebab-case")]
pub enum ProfilingLevel {
    #[default]
    Objective,
    Task,
    Detail,
    Debug,
}

#[derive(
    clap::ValueEnum,
    Clone,
    Copy,
    Debug,
    Default,
    strum::Display,
    strum::EnumString,
    PartialEq,
    Eq
)]
#[strum(serialize_all = "kebab-case")]
pub enum LogLevel {
    Error,
    #[default]
    Warn,
    Info,
    Debug,
    Trace,
}


pub async fn test(repo_root: PathBuf, wasm: &[test::Browser], native: bool) -> Result {
    async fn maybe_run<Fut: Future<Output = Result>>(
        name: &str,
        enabled: bool,
        f: impl (FnOnce() -> Fut),
    ) -> Result {
        if enabled {
            info!("Will run {name} tests.");
            f().await.context(format!("Running {name} tests."))
        } else {
            info!("Skipping {name} tests.");
            Ok(())
        }
    }

    maybe_run("native", native, || async {
        Cargo
            .cmd()?
            .current_dir(repo_root.clone())
            .apply(&cargo::Command::Test)
            .apply(&cargo::Options::Workspace)
            // Color needs to be passed to tests themselves separately.
            // See: https://github.com/rust-lang/cargo/issues/1983
            .arg("--")
            .apply(&cargo::Color::Always)
            .run_ok()
            .await
    })
    .await?;

    maybe_run("wasm", !wasm.is_empty(), || test::test_all(repo_root.clone(), wasm)).await?;
    Ok(())
}
