//! The Models.
//!
//! Models in this crate are structure which represents the logical "objects" being a part of the
//! project, like project itself, module, execution context etc. They usually reflects the entities
//! managed by LanguageServer.
//!
//! For Each model there is defined:
//! * An API trait
//! * A Synchronized Model implementing the API, which try to notify Language Server about all
//!   changes in the modeled entity.
//! * Sometimes a Plain Model, which stores data but does not synchronize anything.
//! * Sometimes a Mock generated with `mock-all` crate.
//! * A Shared Handle, which hides some of above API implementors under `Rc<dyn API>`.
//!
//! Plain Model and Mock should be used in tests only, which simplifies mocking dependencies. In
//! the actual code the Synchronized Model should be used. Each struct using model as a dependency
//! should use the Shared Handle.
//!
//! As the models are usually shared between many controllers, they should implement "internal
//! mutability" pattern.


// ==============
// === Export ===
// ==============

pub mod execution_context;
pub mod module;
pub mod project;
pub mod registry;
pub mod undo_redo;

pub use enso_suggestion_database as suggestion_database;
pub use execution_context::ExecutionContext;
pub use module::Module;
pub use project::Project;
pub use suggestion_database::SuggestionDatabase;



/// A module with commonly used traits to mass import.
pub mod traits {
    use super::*;

    pub use execution_context::API as TRAIT_ExecutionContextApi;
    pub use module::APIExt as TRAIT_ModuleApiExt;
    pub use module::API as TRAIT_ModuleApi;
    pub use project::API as TRAIT_ProjectApi;
    pub use undo_redo::Aware as TRAIT_UndoRedoAware;
}
