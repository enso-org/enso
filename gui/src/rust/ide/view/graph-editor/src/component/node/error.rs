//! Contains a struct definition for error information on nodes.
use crate::prelude::*;

use crate::component::node::visualization;



// =============
// === Error ===
// =============

/// Error information to be displayed on a node.
///
/// Note:
/// Tis is a dummy implementation that can and should be extended once we have the proper
/// error information from the language server.
/// See [issue #1026](https://github.com/enso-org/ide/issues/1026) for more information.
#[derive(Clone,Debug)]
pub struct Error {
    /// The error message to show on the node.
    pub message:String,
}

impl From<Error> for visualization::Data {
    fn from(error:Error) -> visualization::Data {
        let content = serde_json::Value::String(error.message).into();
        visualization::Data::Json {content}
    }
}

impl From<String> for Error {
    fn from(message:String) -> Error {
        Self{message}
    }
}
