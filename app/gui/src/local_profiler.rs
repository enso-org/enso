//! GUI specific metadata logging utilities for use with the profiling framework.

thread_local! {
    /// A common preamble used to start every shader program.
    static RPC_EVENT_LOGGER: enso_profiler::MetadataLogger<& 'static str> = enso_profiler::MetadataLogger::new("RpcEvent");
}

/// Log an RPC Event to the profiling framework.
pub fn log_rpc_event(event_name: &'static str) {
    RPC_EVENT_LOGGER.with(|logger| logger.log(event_name));
}
