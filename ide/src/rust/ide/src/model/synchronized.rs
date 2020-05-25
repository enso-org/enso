//! This module contains synchronising wrappers for models whose state are a reflection of
//! Language Server state, e.g. modules, execution contexts etc. These wrappers synchronize both
//! states by notifying Language Server of every change and listening on LanguageServer.
pub mod execution_context;
pub mod module;

pub use execution_context::ExecutionContext;
pub use module::Module;
