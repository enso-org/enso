//! Operator related functionalities.



mod application;
mod apply;
mod arity;
mod operand;
mod precedence_resolver;
mod reducer;
mod types;


// ===============
// === Exports ===
// ===============

pub use precedence_resolver::Precedence;
pub use types::SectionTermination;
pub use types::Warnings;
