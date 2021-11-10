//! This is a library aimed to facilitate implementing JSON-RPC protocol
//! clients. The main type is `Handler` that a client should build upon.

#![feature(trait_alias)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]


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
