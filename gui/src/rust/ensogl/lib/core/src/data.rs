//! Root module for generic data containers and modifiers.

pub mod color;
pub mod container;
pub mod dirty;
pub mod function;
pub mod seq;
pub mod theme;

pub use data::hash_map_tree;

pub use data::hash_map_tree::HashMapTree;
pub use data::index::Index;
pub use data::opt_vec::OptVec;

/// A type alias for color representation used in the GUI. It is implemented as the perceptual
/// uniform LCH color space because it provides the best visual properties of all available color
/// spaces implemented in this library (there are color spaces which have even better properties
/// but they cannot be implemented using only 3 components, which makes them almost useless when
/// drawing with GPU support).
pub type Color = color::Lch;
