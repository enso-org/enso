//! This script would run Tailwind CLI utility to generate a CSS stylesheet by scanning the
//! source code for class names and including needed CSS rules in the output file.
//!
//! See crate documentation to learn more.

use ide_ci::prelude::*;
use ide_ci::programs::Npm;

/// The path to the input file. One can define arbitrary CSS rules there and they will be copied
/// in the output file.
const CSS_INPUT_PATH: &str = "assets/input.css";

#[tokio::main]
async fn main() -> Result {
    // We should rerun the tailwind on changes in our sources.
    // Tailwind scans this directory to determine the used classes.
    println!("cargo:rerun-if-changed=src");
    // We should rerun the tailwind on changes in the input CSS file.
    // It may contain custom CSS rules.
    println!("cargo:rerun-if-changed={CSS_INPUT_PATH}");

    install_and_run_tailwind().await?;
    Ok(())
}

async fn install_and_run_tailwind() -> Result {
    let no_args: [&str; 0] = [];
    Npm.cmd()?.install().run_ok().await?;
    Npm.cmd()?.run("generate", no_args).run_ok().await?;
    Ok(())
}
