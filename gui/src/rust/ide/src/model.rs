//! The module with structures describing models shared between different controllers.

pub mod execution_context;
pub mod module;
pub mod registry;
pub mod synchronized;

pub use execution_context::ExecutionContext;
pub use module::Module;
