//! Interface for logging RPC Requests as [`enso_profiler`] metadata, and interpreting the resultant
//! log events.



// ===========================
// === Rpc Request Logging ===
// ===========================

/// Log an RPC Request to the profiling framework.
pub fn rpc_request(method: &'static str) {
    thread_local! {
        static RPC_REQUEST_LOGGER: enso_profiler::MetadataLogger<& 'static str> =
            enso_profiler::MetadataLogger::new("RpcRequest");
    }
    RPC_REQUEST_LOGGER.with(|logger| logger.log(method));
}



// ==================
// === RpcRequest ===
// ==================

/// Message sent from the IDE to the Language Server.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct RpcRequest(std::borrow::Cow<'static, str>);

impl std::fmt::Display for RpcRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}
