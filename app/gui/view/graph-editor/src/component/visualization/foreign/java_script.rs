//! Root module for JavaScript API bindings.


// ==============
// === Export ===
// ==============

pub mod binding;
pub mod definition;
pub mod instance;
pub mod source;

pub use definition::field;
pub use definition::method;
pub use definition::Definition;
pub use definition::FallibleDefinition;
pub use instance::Instance;
pub use instance::PreprocessorCallback;
pub use source::Sources;
