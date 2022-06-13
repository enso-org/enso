// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use enso_build::prelude::*;



fn main() -> Result {
    let build_config_yaml = include_str!("../../build-config.yaml");
    let config = enso_build::config::load_yaml(build_config_yaml)?;
    enso_build_cli::lib_main(config)
}
