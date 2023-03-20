//! Types for interpreting profiles containing Enso application data.

// === Features ===
#![feature(test)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]

use serde::Serializer;
use std::fmt::Display;
use std::fmt::Formatter;


// ==============
// === Export ===
// ==============

pub mod backend;
pub mod beanpole;



// ================
// === Metadata ===
// ================

/// Metadata that is logged within the Enso core libraries.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Metadata {
    /// A message received by the IDE from the Language Server.
    RpcEvent(String),
    /// A message sent from the IDE to the Language Server.
    RpcRequest(json_rpc::log::RpcRequest),
    /// A message between the Language Server and the Engine.
    BackendMessage(backend::Message),
    /// Performance stats gathered from the EnsoGL rendering engine.
    RenderStats(ensogl_core::debug::StatsData),
    /// Any other metadata type.
    ///
    /// The types defined above are handled specially by `enso-profiler-enso-data` tools: E.g. the
    /// RPC events and `RenderStats` are displayed in different ways by the `profiling_run_graph`
    /// entry point.
    ///
    /// Other types are logged purely so they they can be seen in the events logs, e.g. when
    /// inspecting a log with the `measurements` tool.
    #[serde(other)]
    Other,
}

impl Display for Metadata {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Metadata::RpcEvent(name) => f.collect_str(name),
            Metadata::RpcRequest(method) => f.collect_str(&method.to_string()),
            Metadata::BackendMessage(backend::Message { endpoint, .. }) => f.collect_str(endpoint),
            Metadata::RenderStats(stats) => f.collect_str(&format!("{stats:#?}")),
            Metadata::Other => f.collect_str("<value>"),
        }
    }
}
