//! Interface for logging RPC Requests as [`enso_profiler`] metadata, and interpreting the resultant
//! log events.

use crate::prelude::*;



// ===========================
// === Rpc Request Logging ===
// ===========================

profiler::metadata_logger!("RpcRequest", rpc_request(&'static str));



// ==================
// === RpcRequest ===
// ==================

/// Message sent from the IDE to the Language Server.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct RpcRequest(Cow<'static, str>);

impl Display for RpcRequest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.0)
    }
}
