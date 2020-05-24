//! A set of various error types used by the Enso Protocol RPC handler.

use crate::prelude::*;

/// When trying to parse a line, not a single line was produced.
#[derive(Debug,Fail,Clone,Copy)]
#[fail(display = "No active request by id {}", _0)]
pub struct NoSuchRequest<Id:Sync + Send + Debug + Display + 'static>(pub Id);

#[allow(missing_docs)]
#[derive(Debug,Fail,Clone)]
#[fail(display = "Failed to deserialize the received message. {}", _0)]
pub struct DeserializationError(pub String);

#[allow(missing_docs)]
#[derive(Debug,Fail,Clone,Copy)]
#[fail(display = "Received a message that is neither a response nor a notification")]
pub struct UnexpectedMessage;
