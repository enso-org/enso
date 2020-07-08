//! The Models.
//!
//! Models in this crate are structure which represents the logical "objects" being a part of the
//! project, like project itself, module, execution context etc. They usually reflects the entites
//! managed by LanguageServer.
//!
//! Some models are additionally wrapped in "Synchronized" structures, which adds an automatic
//! synchronization between states of the model and the analogous entity in Language Server: for
//! example `synchronized::Module` sends notification about each module change to LS.
//!
//! The models should implement internal mutability pattern, because they should be easily shared
//! between controllers.

pub mod execution_context;
pub mod module;
pub mod registry;
pub mod suggestion_database;
pub mod synchronized;
pub mod project;

pub use execution_context::ExecutionContext;
pub use module::Module;
pub use project::Project;
pub use suggestion_database::SuggestionDatabase;
