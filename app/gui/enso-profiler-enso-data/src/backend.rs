//! Defines a metadata type for representing backend messages.



// ===============
// === Message ===
// ===============

/// Metadata type for messages between the Language Server and the Compiler.
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub struct Message {
    /// Identifies whether the logging process is the sender or receiver of the message.
    pub direction:  Direction,
    /// Used to associate requests and responses.
    pub request_id: Option<String>,
    /// Identifies an RPC method.
    pub endpoint:   String,
}


// === Direction ===

/// Identifies which process is the sender of a message.
#[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
pub enum Direction {
    /// From logging process to other process.
    Request,
    /// From other process to logging process.
    Response,
}
