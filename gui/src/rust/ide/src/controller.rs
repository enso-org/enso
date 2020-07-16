//! This module contains all the controllers. They cover everything that is
//! between clients of remote services (like language server and file manager)
//! and views.
//!
//! The controllers create a tree-like structure, with project controller being
//! a root, then module controllers below, then graph/text controller and so on.
//!
//! As a general rule, while the "upper" (i.e. closer to root) nodes may keep
//! handles to the "lower" nodes (e.g. to allow their reuse), they should never
//! manage their lifetime.
//!
//! Primarily views are considered owners of their respective controllers.
//! Additionally, controllers are allowed to keep strong handle "upwards".
//!
//! Controllers store their handles using `utils::cell` handle types to ensure
//! that mutable state is safely accessed.

pub mod graph;
pub mod module;
pub mod text;
pub mod visualization;
pub mod searcher;

pub use graph::Handle           as Graph;
pub use graph::executed::Handle as ExecutedGraph;
pub use module::Handle          as Module;
pub use text::Handle            as Text;
pub use visualization::Handle   as Visualization;
pub use searcher::Searcher;



// ============
// === Path ===
// ============

/// Path to a file on disc, used across all controllers
pub type FilePath = enso_protocol::language_server::Path;
