//! Root module for generic data containers and modifiers.

pub mod color;
pub mod container;
pub mod dirty;
pub mod function;
pub mod mix;
pub mod seq;

pub use enso_data::dependency_graph;
pub use enso_data::hash_map_tree;
pub use enso_data::hash_map_tree::HashMapTree;
pub use enso_data::index::Index;
pub use enso_data::opt_vec::OptVec;
