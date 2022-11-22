// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_build::prelude::*;

use enso_build::config::Config;
use enso_build::config::ConfigRaw;



fn main() -> Result {
    setup_logging()?;
    trace!("Starting CLI driver, cwd is {}", ide_ci::env::current_dir()?.display());
    let build_config_yaml = include_str!("../../../build-config.yaml");
    let build_config_raw = serde_yaml::from_str::<ConfigRaw>(build_config_yaml)?;
    let build_config = Config::try_from(build_config_raw)?;
    enso_build_cli::lib_main(Some(build_config))
}
