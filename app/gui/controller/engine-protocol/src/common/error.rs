//! A set of various error types used by the Enso Protocol RPC handler.

use crate::prelude::*;

/// When trying to parse a line, not a single line was produced.
#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "No active request by id {}", _0)]
pub struct NoSuchRequest<Id: Sync + Send + Debug + Display + 'static>(pub Id);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(display = "Failed to deserialize the received message. {}", _0)]
pub struct DeserializationError(pub String);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "Received a message that is neither a response nor a notification")]
pub struct UnexpectedMessage;

/// The error codes defined in Enso Protocol (see
/// https://enso.org/docs/developer/enso/language-server/protocol-language-server.html#error)
pub mod code {
    /// Informs that the requested content root cannot be found.
    pub const CONTENT_ROOT_NOT_FOUND: i64 = 1001;

    /// Signals that requested file doesn’t exist.
    pub const FILE_NOT_FOUND: i64 = 1003;
}
