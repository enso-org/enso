//! Generic data types and utilities.



// ==============
// === Export ===
// ==============

mod non_empty_vec;
pub mod semigroup;
pub mod vec_indexed_by;

pub use non_empty_vec::NonEmptyVec;
pub use semigroup::*;
pub use vec_indexed_by::VecIndexedBy;
