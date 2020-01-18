#![feature(trait_alias)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

//! This is a library aimed to facilitate implementing JSON-RPC protocol
//! clients. The main type is `Handler` that a client should build upon.

pub mod api;
pub mod error;
pub mod handler;
pub mod messages;
pub mod transport;

pub use transport::Transport;
pub use transport::TransportEvent;
pub use handler::Handler;
