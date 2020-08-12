//! Root module for graph component definitions.

pub mod type_coloring;
pub mod edge;
pub mod node;
pub mod visualization;
pub mod project_name;

pub use edge::Edge;
pub use node::Node;
pub use project_name::ProjectName;
