//! Build script for [`enso_profiler_macros`]. This is needed because `profiler_macros` has a
//! profiling level controlled by the value of an environment variable at compile time, and cargo
//! needs to be made aware that changes to the env can invalidate the result of compiling this
//! crate and any dependents.

// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]



fn main() {
    println!("cargo:rerun-if-env-changed=ENSO_MAX_PROFILING_LEVEL");
    // This is a no-op assignment, except it makes cargo aware that the output depends on the env.
    let value = std::env::var("ENSO_MAX_PROFILING_LEVEL").unwrap_or_default();
    println!("cargo:rustc-env=ENSO_MAX_PROFILING_LEVEL={}", value);
}
