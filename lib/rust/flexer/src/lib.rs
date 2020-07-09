#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports the API for defining a simple lexer based on a deterministic finite state
//! automaton.
//!
//! These lexers are capable of lexing any regular grammar, with some extensions to allow working
//! with context sensitive (e.g. indentation-aware) syntax.

pub mod automata;
pub mod group;
pub mod data;

pub use enso_prelude as prelude;
