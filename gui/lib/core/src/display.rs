//! Root module for all display-related abstractions, including display objects, shapes, geometries,
//! rendering utilities, etc.

pub mod camera;
pub mod layout;
pub mod object;
pub mod navigation;
pub mod render;
pub mod scene;
pub mod shape;
pub mod symbol;
pub mod world;



/// Commonly used types.
pub mod types {
    use super::*;
    pub use scene::Scene;
}
pub use types::*;
