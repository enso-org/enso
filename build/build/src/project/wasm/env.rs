//! Environment variables used by the GUI's Rust part build.

use crate::project::wasm::ProfilingLevel;



ide_ci::define_env_var! {
    /// Enable a Rust unstable feature that the `#[profile]` macro uses to obtain source-file
    /// and line number information to include in generated profile files.
    ///
    /// The IntelliJ Rust plugin does not support the `proc_macro_span` Rust feature; using it
    /// causes JetBrains IDEs to become entirely unaware of the items produced by `#[profile]`.
    /// (See: https://github.com/intellij-rust/intellij-rust/issues/8655)
    ///
    /// In order to have line number information in actual usage, but keep everything
    /// understandable by JetBrains IDEs, we need IntelliJ/CLion to build crates differently
    /// from how they are built for the application to be run. This is accomplished by gating
    /// the use of the unstable functionality by a `cfg` flag. A `cfg` flag is disabled by
    /// default, so when a Rust IDE builds crates internally in order to determine macro
    /// expansions, it will do so without line numbers. When this script is used to build the
    /// application, it is not for the purpose of IDE macro expansion, so we can safely enable
    /// line numbers.
    ///
    /// The reason we don't use a Cargo feature for this is because this script can build
    /// different crates, and we'd like to enable this feature when building any crate that
    /// depends on the `profiler` crates. We cannot do something like
    /// '--feature=enso_profiler/line-numbers' without causing build to fail when building a
    /// crate that doesn't have `enso_profiler` in its dependency tree.
    ENSO_ENABLE_PROC_MACRO_SPAN, bool;

    /// Use the environment-variable API provided by the `enso_profiler_macros` library to
    /// implement the public interface to profiling-level configuration (see:
    /// https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md)
    ENSO_MAX_PROFILING_LEVEL, ProfilingLevel;

    /// The timeout for `wasm-bindgen-test-runner` in seconds.
    WASM_BINDGEN_TEST_TIMEOUT, u64;
}
