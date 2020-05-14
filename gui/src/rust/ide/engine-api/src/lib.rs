//! Crate containing the Engine Services binary protocol interface.

#[allow(dead_code, unused_imports)]
use flatbuffers;

pub mod generated;

pub use generated::binary_protocol_generated as binary_protocol;
