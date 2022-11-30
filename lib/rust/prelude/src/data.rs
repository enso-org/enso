//! Generic data types and utilities.


// ==============
// === Export ===
// ==============

pub mod at_least_one_of_two;
pub mod lazy_invariant_vec;
pub mod monoid;
pub mod non_empty_vec;
pub mod semigroup;
pub mod vec_indexed_by;

pub use at_least_one_of_two::*;
pub use lazy_invariant_vec::LazyInvariantVec;
pub use monoid::*;
pub use non_empty_vec::NonEmptyVec;
pub use semigroup::*;
pub use vec_indexed_by::VecIndexedBy;
