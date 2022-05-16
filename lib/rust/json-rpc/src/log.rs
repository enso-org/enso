//! Interface for logging RPC Requests as [`enso_profiler`] metadata, and interpreting the resultant
//! log events.



// ===========================
// === Rpc Request Logging ===
// ===========================

enso_profiler::metadata_logger!("RpcRequest", rpc_request(&'static str));



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
