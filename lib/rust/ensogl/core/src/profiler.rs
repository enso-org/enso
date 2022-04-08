//! Enso specific metadata logging utilities for use with the profiling framework.
use crate::debug::StatsData;
use enso_profiler;
use serde;
use serde::Serializer;
use std::fmt::Display;
use std::fmt::Formatter;


/// Metadata that is logged within the Enso core libraries.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Metadata {
    /// An RPC event that was received from the backend.
    RpcEvent(String),
    /// Performance stats gathered from the EnsoGL rendering engine.
    RenderStats(StatsData),
}

impl Display for Metadata {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Metadata::RpcEvent(name) => f.collect_str(name),
            Metadata::RenderStats(stats) => f.collect_str(&format!("{:#?}", stats)),
        }
    }
}

/// Log an RPC Event to the profiling framework.
pub fn log_rpc_event(event_name: &'static str) {
    let event_logger = enso_profiler::MetadataLogger::new("RpcEvent");
    event_logger.log(event_name);
}

/// Log an RPC Event to the profiling framework.
pub fn log_stats_data(data: StatsData) {
    let event_logger = enso_profiler::MetadataLogger::new("RenderStats");
    event_logger.log(data);
}
