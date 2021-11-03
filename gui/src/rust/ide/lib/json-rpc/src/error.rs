//! Error types used by this crate. `RpcError` is used in remote call results,
//! while `HandlingError` can be raised by `Handler`.

use crate::prelude::*;

use crate::messages::Error;
use crate::messages::Response;

use futures::channel::oneshot::Canceled;



// ================
// === RpcError ===
// ================

/// Errors that can cause a remote call to fail.
#[derive(Debug, Fail)]
pub enum RpcError<Payload: Debug + Send + Sync + 'static = serde_json::Value> {
    /// Error returned by the remote server.
    #[fail(display = "Peer has replied with an error: {:?}.", _0)]
    RemoteError(Error<Payload>),

    /// Lost connection while waiting for response.
    #[fail(display = "Lost connection before receiving a reply.")]
    LostConnection,

    /// Failed to deserialize message from server.
    #[fail(display = "Failed to deserialize a message: {}.", _0)]
    DeserializationFailed(serde_json::Error),

    /// Response was deserialized but its type was wrong.
    #[fail(display = "Received a reply of a wrong type.")]
    MismatchedResponseType,

    /// Response timeout.
    ///
    /// Note that this represents the client-side timeout. Server-side timeout will be treated as
    /// an [`RemoteError`].
    #[allow(missing_docs)]
    #[fail(display = "Response timed out after {} ms.", millis)]
    TimeoutError { millis: u128 },
}

impl RpcError {
    /// Wraps provided by the remote peer code and message into a `RpcError`.
    pub fn new_remote_error(code: i64, message: impl Str) -> RpcError {
        RpcError::RemoteError(Error { code, message: message.into(), data: None })
    }
}

impl From<Canceled> for RpcError {
    fn from(_: Canceled) -> Self {
        RpcError::LostConnection
    }
}

impl From<serde_json::Error> for RpcError {
    fn from(e: serde_json::Error) -> Self {
        RpcError::DeserializationFailed(e)
    }
}



// =====================
// === HandlingError ===
// =====================

/// Errors specific to the Handler itself, not any specific request.
///
/// Caused either internal errors in the handler or bugs in the server.
#[derive(Debug, Fail)]
pub enum HandlingError {
    /// When incoming text message can't be decoded.
    #[fail(display = "Failed to decode incoming text message: {}.", _0)]
    InvalidMessage(#[cause] serde_json::Error),

    /// Server responded to an identifier that does not match to any known
    /// ongoing request.
    #[fail(display = "Server generated a response with no matching request: id={:?}.", _0)]
    UnexpectedResponse(Response<serde_json::Value>),

    /// JSON-RPC client does not expect any binary messages, yet it received one.
    #[fail(display = "Server sent unexpected binary message: {:?}.", _0)]
    UnexpectedBinaryMessage(Vec<u8>),

    /// Server send a message that is notification but client wasn't able to
    /// decode it.
    #[fail(display = "Failed to decode a notification: {}.", _0)]
    InvalidNotification(#[cause] serde_json::Error),
}
