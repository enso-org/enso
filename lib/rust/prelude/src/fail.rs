//! General-purpose code related to error handling.

/// General-purpose `Result` supporting any `Error`-compatible failures.
pub type FallibleResult<T = ()> = Result<T, failure::Error>;
