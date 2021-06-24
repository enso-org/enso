//! This module contains all the controllers. They cover everything that is
//! between clients of remote services (like language server and file manager)
//! and views.
//!
//! The API of each controller is "view-facing", in contrast to the models in [`crate::model`] which
//! are focusing on reflecting the Engine entities (thus can be called "Engine-facing").

pub mod graph;
pub mod ide;
pub mod module;
pub mod text;
pub mod project;
pub mod visualization;
pub mod searcher;
pub mod upload;

pub use graph::Handle           as Graph;
pub use graph::executed::Handle as ExecutedGraph;
pub use self::ide::Ide;
pub use module::Handle          as Module;
pub use project::Project;
pub use text::Handle            as Text;
pub use visualization::Handle   as Visualization;
pub use searcher::Searcher;



// ============
// === Path ===
// ============

/// Path to a file on disc, used across all controllers
pub type FilePath = enso_protocol::language_server::Path;
