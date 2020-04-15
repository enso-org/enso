//! A module with all functions used to synchronize different representations of our language
//! module.

pub mod alias_analysis;
pub mod definition;
pub mod graph;
pub mod node;
pub mod text;



// ==============
// === Consts ===
// ==============

/// Indentation value from language specification:
///
/// Indentation: Indentation is four spaces, and all tabs are converted to 4 spaces. This is not
/// configurable on purpose.
///
/// Link: https://github.com/luna/enso/blob/master/doc/design/syntax/syntax.md#encoding
pub const INDENT : usize = 4;
