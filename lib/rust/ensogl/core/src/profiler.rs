//! Enso specific metadata logging utilities for use with the profiling framework.
use crate::prelude::*;

use enso_profiler;
use serde;
use serde::Serializer;
use std::fmt::Display;
use std::fmt::Formatter;


/// Metadata that is logged within the Enso core libraries.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Eq, PartialEq)]
pub enum Metadata {
    /// An RPC event that was received from the backend.
    RpcEvent(String),
}

impl Display for Metadata {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Metadata::RpcEvent(name) => f.collect_str(name),
        }
    }
}

thread_local! {
    /// A common preamble used to start every shader program.
    static RPC_EVENT_LOGGER: enso_profiler::MetadataLogger<& 'static str> = enso_profiler::MetadataLogger::new("RpcEvent");
}


/// Log an RPC Event to the profiling framework.
pub fn log_rpc_event(event_name: &'static str) {
    RPC_EVENT_LOGGER.with(|logger| logger.log(event_name));
}
