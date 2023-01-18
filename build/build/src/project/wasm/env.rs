//! Environment variables used by the GUI's Rust part build.

use crate::project::wasm::LogLevel;
use crate::project::wasm::ProfilingLevel;



ide_ci::define_env_var! {
    /// Use the environment-variable API provided by the `enso_profiler_macros` library to
    /// implement the public interface to profiling-level configuration (see:
    /// https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md)
    ENSO_MAX_PROFILING_LEVEL, ProfilingLevel;

    /// Set the level of logging detail that will be enabled at compile-time.
    ENSO_MAX_LOG_LEVEL, LogLevel;
    /// Set the level of logging detail that will be displayed initially-open in hierarchical views,
    /// such as the Web Console.
    ENSO_MAX_UNCOLLAPSED_LOG_LEVEL, LogLevel;

    /// The timeout for `wasm-bindgen-test-runner` in seconds.
    WASM_BINDGEN_TEST_TIMEOUT, u64;
}
