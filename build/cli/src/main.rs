// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_build::prelude::*;

use enso_build::config;



fn main() -> Result {
    setup_logging()?;
    trace!("Starting CLI driver, cwd is {}", ide_ci::env::current_dir()?.display());
    let build_config = config::load()?;
    enso_build_cli::lib_main(Some(build_config))
}
