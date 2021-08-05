//! A module with all functions used to synchronize different representations of our language
//! module.

pub mod alias_analysis;
pub mod connection;
pub mod definition;
pub mod graph;
pub mod identifier;
pub mod module;
pub mod node;
pub mod project;
pub mod refactorings;
pub mod text;
pub mod tp;

#[cfg(test)]
pub mod test_utils;



// ==============
// === Consts ===
// ==============

/// Indentation value from language specification:
///
/// Indentation: Indentation is four spaces, and all tabs are converted to 4 spaces. This is not
/// configurable on purpose.
///
/// Link: https://github.com/enso-org/enso/blob/main/doc/syntax/encoding.md
pub const INDENT : usize = 4;
