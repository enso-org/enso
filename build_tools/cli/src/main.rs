use enso_build::prelude::*;

use enso_build::config;



fn main() -> Result {
    unsafe { backtrace_on_stack_overflow::enable() };
    setup_logging()?;
    trace!("Starting CLI driver, cwd is {}", ide_ci::env::current_dir()?.display());
    let build_config = config::load()?;
    enso_build_cli::lib_main(Some(build_config))
}
