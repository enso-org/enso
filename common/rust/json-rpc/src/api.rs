//! Module contains entities used by client implementor to describe remote API.

use prelude::*;

use crate::error::RpcError;
use crate::messages::Id;
use crate::messages::Message;
use crate::messages::RequestMessage;

use serde::Serialize;
use serde::de::DeserializeOwned;



// ==============
// === Result ===
// ==============

/// A result of an RPC-call.
pub type Result<T> = std::result::Result<T, RpcError>;



// ========================
// === RemoteMethodCall ===
// ========================

/// Structure describing a call to a remote method.
///
/// A serialized value of this trait represents the method's input arguments.
pub trait RemoteMethodCall : Serialize + Debug {
    /// Name of the remote method.
    const NAME:&'static str;

    /// A type of value returned from successful remote call.
    type Returned:DeserializeOwned;
}

/// Make a request message from given RemoteMethodInput value.
pub fn into_request_message<In:RemoteMethodCall>
(input:In, id:Id) -> RequestMessage<In> {
    Message::new_request(id,In::NAME,input)
}
