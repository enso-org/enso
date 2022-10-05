use crate::prelude::*;



pub mod env {
    crate::define_env_var! {
        /// An environment variable set commonly by most of popular CI systems.
        CI, String;
    }
}

/// Check if the environment suggests that we are being run in a CI.
pub fn run_in_ci() -> bool {
    env::CI.is_set()
}
