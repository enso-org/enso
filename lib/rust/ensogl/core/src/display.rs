//! Root module for all display-related abstractions, including display objects, shapes, geometries,
//! rendering utilities, etc.


// ==============
// === Export ===
// ==============

pub mod camera;
pub mod garbage;
pub mod layout;
pub mod navigation;
pub mod object;
pub mod render;
pub mod scene;
pub mod shape;
pub mod style;
pub mod symbol;
pub mod world;



// ===============
// === Exports ===
// ===============

/// Common traits.
pub mod traits {
    use super::*;
    pub use object::traits::*;
}

/// Common types.
pub mod types {
    pub use super::symbol::*;
    pub use super::traits::*;
    use super::*;
    pub use object::Object;
    pub use scene::dom::DomScene;
    pub use scene::Scene;
}
pub use types::*;
