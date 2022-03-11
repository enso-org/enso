//! Root module for primitive geometry definitions. A primitive geometry could not be constructed
//! from other types.


// ==============
// === Export ===
// ==============

pub mod mesh;



// ===============
// === Exports ===
// ===============

/// Common types.
pub mod types {
    use super::*;
    pub use mesh::types::*;
}
pub use types::*;
