//! This script would run Tailwind CLI utility to generate a CSS stylesheet by scanning the
//! source code for class names and including needed CSS rules in the output file.
//!
//! See crate documentation to learn more.

use ide_ci::prelude::*;

use ide_ci::programs::Npm;



/// The path to the input file. One can define arbitrary CSS rules there and they will be copied
/// in the output file.
const CSS_INPUT_PATH: &str = "assets/input.css";
/// The filename of the resulting CSS stylesheet. It will be generated inside `OUT_DIR`.
const CSS_OUTPUT_FILENAME: &str = "stylesheet.css";

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
    Npm.cmd()?.install().run_ok().await?;
    let out_path = PathBuf::from(std::env::var("OUT_DIR").unwrap()).join(CSS_OUTPUT_FILENAME);
    let args: &[&str] = &["--", "-i", CSS_INPUT_PATH, "-o", out_path.as_str()];
    Npm.cmd()?.run("generate", args).run_ok().await?;
    Ok(())
}
