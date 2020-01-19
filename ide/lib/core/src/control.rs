//! Root module for all control abstractions, like event loops or event systems.

pub mod callback;
pub mod io;
mod event_loop;

pub use event_loop::*;
