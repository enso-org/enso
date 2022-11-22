//! Constants defined for the Language Server JSON-RPC API.

use crate::prelude::*;



/// Recognized error codes used by the Language Server messages.
///
/// They follow `org.enso.jsonrpc.Error` object defined in the `enso` repository.
#[derive(Clone, Copy, Debug)]
pub enum ErrorCodes {
    /// Server failed to parse JSON message.
    ParseError          = -32700,
    /// JSON message sent was not a valid Request object.
    InvalidRequest      = -32600,
    /// Requested method does not exist or is unavailable.
    MethodNotFound      = -32601,
    /// Invalid method parameters.
    InvalidParams       = -32602,
    /// Service error.
    ServiceError        = 1,
    /// The requested method is not implemented.
    NotImplementedError = 10,
    /// Request timeout.
    /// Note that timeout can also happen on the client side, as part of the Handler's logic.
    Timeout             = 11,
}
