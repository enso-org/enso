//! Types for interpreting profiles containing Enso application data.

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

use serde::Serializer;
use std::fmt::Display;
use std::fmt::Formatter;



// ================
// === Metadata ===
// ================

/// Metadata that is logged within the Enso core libraries.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Metadata {
    /// A message received by the IDE from the Language Server.
    RpcEvent(String),
    /// A message between the Language Server and the Engine.
    BackendMessage(backend::Message),
    /// Performance stats gathered from the EnsoGL rendering engine.
    RenderStats(ensogl_core::debug::StatsData),
}

impl Display for Metadata {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Metadata::RpcEvent(name) => f.collect_str(name),
            Metadata::BackendMessage(backend::Message { endpoint, .. }) => f.collect_str(endpoint),
            Metadata::RenderStats(stats) => f.collect_str(&format!("{:#?}", stats)),
        }
    }
}
