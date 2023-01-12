//! Utilities for tests related to the web-based executors.



/// Set up a global animation-frame-based executor.
/// Leaks it handle so it will run indefinitely.
/// To be used in asynchronous wasm tests.
pub fn setup_and_forget() {
    std::mem::forget(crate::setup_global_executor());
}
