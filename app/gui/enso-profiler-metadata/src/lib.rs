//! Enso specific metadata logging utilities for use with the profiling framework.
use serde::Serializer;
use std::fmt::Display;
use std::fmt::Formatter;



// ================
// === Metadata ===
// ================

/// Metadata that is logged within the Enso core libraries.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Metadata {
    /// An RPC event that was received from the backend.
    RpcEvent(String),
    /// Performance stats gathered from the EnsoGL rendering engine.
    RenderStats(ensogl_core::debug::StatsData),
}

impl Display for Metadata {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Metadata::RpcEvent(name) => f.collect_str(name),
            Metadata::RenderStats(stats) => f.collect_str(&format!("{:#?}", stats)),
        }
    }
}
