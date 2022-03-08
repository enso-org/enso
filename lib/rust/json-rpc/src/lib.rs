//! This is a library aimed to facilitate implementing JSON-RPC protocol
//! clients. The main type is `Handler` that a client should build upon.

// === Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



//
// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-standard linter configuration ===



// === Standard linter configuration ===
#![warn(missing_copy_implementations)]#![warn(missing_debug_implementations)]#![warn(missing_docs)]#![warn(trivial_casts)]#![warn(trivial_numeric_casts)]#![warn(unsafe_code)]#![warn(unused_import_braces)]#![warn(unused_qualifications)]
// === Non-standard linter configuration ===

// === Features ===
#![feature(trait_alias)]


// ==============
// === Export ===
// ==============

pub mod api;
pub mod error;
pub mod handler;
pub mod macros;
pub mod messages;
pub mod test_util;
pub mod transport;

pub use api::RemoteMethodCall;
pub use api::Result;
pub use enso_prelude as prelude;
pub use enso_web as ensogl;
pub use error::RpcError;
pub use handler::Event;
pub use handler::Handler;
pub use transport::Transport;
pub use transport::TransportEvent;



#[allow(missing_docs)]
pub mod constants {
    use std::time::Duration;

    /// The default timeout for all responses.
    pub const TIMEOUT: Duration = Duration::from_secs(10);
}
