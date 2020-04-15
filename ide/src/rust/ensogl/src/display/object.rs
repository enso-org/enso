//! Display object abstraction and related utilities.

pub mod class;
pub mod transform;
pub mod weak;

pub use class::*;
pub use weak::*;



// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    // Read the Rust Style Guide to learn more about the used naming.
    pub use super::Object        as TRAIT_Object;
    pub use super::ObjectOps     as TRAIT_ObjectOps;
    pub use super::WeakObjectOps as TRAIT_WeakObjectOps;
}
