//! This script would run Tailwind CLI utility to generate a CSS stylesheet by scanning the
//! source code for class names and including needed CSS rules in the output file.
//!
//! This script would only run if the `RUN_TAILWIND` environment variable is defined.
//!
//! See crate documentation to learn more.

use std::process::Command;



/// The path to the input file. One can define arbitrary CSS rules there and they will be copied
/// in the output file.
const CSS_INPUT_PATH: &str = "assets/input.css";
/// The path to the output file. It is included in VCS, so it is not necessary to run this script on
/// every build.
const CSS_OUTPUT_PATH: &str = "assets/stylesheet.css";
/// The name of the Tailwind CLI utility. This name is provided to the `npx` utility.
const TAILWIND_BINARY_NAME: &str = "tailwindcss";

fn main() {
    println!("cargo:rerun-if-changed=src");

    if std::env::var("RUN_TAILWIND").is_ok() {
        let output = Command::new("npx")
            .args([TAILWIND_BINARY_NAME, "-i", CSS_INPUT_PATH, "-o", CSS_OUTPUT_PATH])
            .stderr(std::process::Stdio::inherit())
            .output()
            .expect("Failed to execute TailwindCSS");
        if !output.status.success() {
            panic!(
                "TailwindCSS failed with status code {:?}. Is it installed?",
                output.status.code()
            );
        }
    }
}
