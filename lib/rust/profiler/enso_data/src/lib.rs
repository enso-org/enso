//! Facilities for interpreting profiler data in ways that depend on Enso application datatypes.

// === Features ===
#![feature(test)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]

pub mod backend;
pub mod beanpole;



// ================
// === Metadata ===
// ================

/// Any type of Enso metadata.
#[derive(Clone, Debug, serde::Deserialize)]
pub enum Metadata {
    /// A message the received by the IDE from the Language Server.
    #[serde(rename = "RpcEvent")]
    RpcMessage(String),
    /// A message between the Language Server and the Engine.
    #[serde(rename = "BackendMessage")]
    BackendMessage(backend::Message),
}
