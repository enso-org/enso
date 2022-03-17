//! Root module for system specific implementations. Please note that system is a broad term here,
//! including the native system, JS world, GPU runtime, etc.


// ==============
// === Export ===
// ==============

pub mod context;
pub mod gpu;
pub mod js;
pub mod web;

pub use context::Context;
pub use context::ContextLostHandler;
