//! Environment variables used by the engine's SBT-based build system.

//use crate::prelude::*;

use ide_ci::define_env_var;



define_env_var! {
    /// Factor applied to timeouts in tests. 1.0 means no change, 2.0 means double the timeout.
    CI_TEST_TIMEFACTOR, usize;

    /// Whether flaku tests should be run.
    CI_TEST_FLAKY_ENABLE, bool;
}
