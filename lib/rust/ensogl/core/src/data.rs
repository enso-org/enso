//! Root module for generic data containers and modifiers.

pub mod color;
pub mod container;
pub mod dirty;
pub mod function;
pub mod mix;
pub mod seq;

pub use enso_data_structures::dependency_graph;
pub use enso_data_structures::hash_map_tree;
pub use enso_data_structures::hash_map_tree::HashMapTree;
pub use enso_data_structures::index::Index;
pub use enso_data_structures::opt_vec::OptVec;
