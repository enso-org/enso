//! Operator related functionalities.



mod annotations;
mod application;
mod apply;
mod arity;
mod group;
mod named_app;
mod precedence_resolver;
mod reducer;
mod section;
mod types;
// ===============
// === Exports ===
// ===============

pub use precedence_resolver::Precedence;
pub use types::SectionTermination;
pub use types::Warnings;
