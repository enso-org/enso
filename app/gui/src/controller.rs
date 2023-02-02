//! This module contains all the controllers. They cover everything that is between clients of
//! remote services (like language server and file manager) and views.
//!
//! The API of each controller is "view-facing", in contrast to the models in [`crate::model`] which
//! are focusing on reflecting the Engine entities (thus can be called "Engine-facing").


// ==============
// === Export ===
// ==============

pub mod graph;
pub mod ide;
pub mod module;
pub mod project;
pub mod searcher;
pub mod text;
pub mod upload;
pub mod visualization;

pub use self::ide::Ide;
pub use graph::executed::Handle as ExecutedGraph;
pub use graph::widget::Controller as Widget;
pub use graph::Handle as Graph;
pub use module::Handle as Module;
pub use project::Project;
pub use searcher::Searcher;
pub use text::Handle as Text;
pub use visualization::Handle as Visualization;



// ============
// === Path ===
// ============

/// Path to a file on disc, used across all controllers
pub type FilePath = engine_protocol::language_server::Path;
