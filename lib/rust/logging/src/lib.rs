//! High-performance logging library.

// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]



enso_logging_macros::define_log_levels![Error, Warn, Info, Debug, Trace];
