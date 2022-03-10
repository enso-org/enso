//! Fuzzly Search Utilities.
//!
//! This crate is designed to be used in various search engines; when you get the list of names
//! matching the given pattern, the next step is to order the items, so the best matches
//! are listed first. In such case the `find_best_subsequence` function may be used to score (order
//! priority) for each element.
//!
//! The metrics used for scoring may be adjusted by implementing `Metric` trait, or by customizing
//! parameters of metrics defined in `metric` module.

// === Features ===
#![feature(option_result_contains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]


// ==============
// === Export ===
// ==============

pub mod metric;
pub mod score;
pub mod subsequence_graph;

pub use enso_prelude as prelude;
pub use metric::Metric;
pub use score::find_best_subsequence;
pub use score::matches;
pub use score::Subsequence;
pub use subsequence_graph::Graph as SubsequenceGraph;
