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

pub use enso_prelude as prelude;



pub mod automata;
pub mod group;
pub mod data;



// ===============
// === Flexer ====
// ===============

/// The flexer is an engine for generating lexers.
///
/// Akin to flex and other lexer generators, it is given a definition as a series of rules from
/// which it then generates code for a highly optimised lexer implemented on top of a
/// [DFA](https://en.wikipedia.org/wiki/Deterministic_finite_automaton).
pub trait Flexer {
    /// Creates a new lexer.
    fn new() -> Self;

    /// Returns a code for a highly-optimised lexer implemented on top of a finite-state-automaton.
    fn generate_specialized_code(&mut self) -> String {
        String::from("#[derive(Debug)]\npub struct Lexer {}")
    }
}
