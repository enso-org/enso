#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports simple parser based on Deterministic Finite State Automata for regular
//! grammars (anything parsable with regex patterns).

pub mod automata;
pub mod group;
pub mod parser;
pub mod data;
