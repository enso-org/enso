//! Root module for symbols, visible objects on the scene. There are two main symbol kinds,
//! the gpu symbols and the dom symbols. You should always prefer to use the former because
//! they provide much greater visual experience and much better performance. However, if you
//! need to use an existing HTML element, use the dom symbol to manage it on the stage.


// ==============
// === Export ===
// ==============

pub mod dom;
pub mod gpu;

pub use dom::*;
pub use gpu::*;
