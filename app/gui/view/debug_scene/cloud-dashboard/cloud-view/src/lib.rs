//! Crate containing types defining the API used by the Cloud dashboard.
//!
//! The types defined in this crate are slimmed down versions of the types defined in the private
//! `enso_cloud_lambdas` crate. The reason this crate exists, rather than both the Cloud frontend
//! and Cloud backend using those types is because they contain implementation details that should
//! not be user-visible for security reasons, or aren't necessary for the Cloud frontend to operate.
//! Thus, this crate contains representations of the same types that exist in `enso_cloud_lambdas`,
//! but with only the minimum information necessary. The full types in `enso_cloud_lambdas` are then
//! built on top of these definitions.
//!
//! The types in this crate are used by the:
//! - **Cloud frontend** when *requests* are send to the Cloud backend,
//! - **Cloud backend** when *requests* are deserialized prior to being handled,
//! - **Cloud backend** when *responses* are serialized prior to being returned to the Cloud
//! frontend,
//! - **Cloud frontend** when *responses* are deserialized.
//!
//! Defining the types in this shared crate keeps our API *definition* and *usage* consistent in
//! both the frontend and the backend.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(keyword_idents)]
#![deny(macro_use_extern_crate)]
#![deny(missing_abi)]
#![deny(pointer_structural_match)]
#![deny(unsafe_op_in_unsafe_fn)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(absolute_paths_not_starting_with_crate)]
#![warn(elided_lifetimes_in_paths)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(single_use_lifetimes)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_lifetimes)]
#![warn(unused_qualifications)]
#![warn(variant_size_differences)]
#![warn(unreachable_pub)]

use std::error;


// ==============
// === Export ===
// ==============

pub mod id;
pub mod project;



// ===============
// === Prelude ===
// ===============

/// A module to let consumers of this crate import common Cloud-related types.
///
/// For example, the [`Error`] type is used in almost all Cloud-related crates so it is desirable
/// to always have it in scope. Glob-importing this module makes this easy to do.
pub mod prelude {
    pub use crate::Error;
}



// =============
// === Error ===
// =============

/// Type alias for a thread-safe boxed [`Error`].
///
/// Convert your error to this type when your error can not be recovered from, or no further context
/// can be added to it.
///
/// [`Error`]: ::std::error::Error
pub type Error = Box<dyn error::Error + Send + Sync + 'static>;



// ==============================================
// === `separate_pascal_case_str_with_spaces` ===
// ==============================================

/// Takes a [`&str`] containing a Pascal-case identifier (e.g. `"UsagePlan"`) and
/// returns a [`String`] with the individual words of the identifier separated by a
/// single whitespace (e.g., `"Usage Plan"`).
pub fn separate_pascal_case_str_with_spaces(s: &str) -> String {
    let mut new = String::with_capacity(s.len());
    let mut first_char = true;
    for c in s.chars() {
        if c.is_uppercase() && !first_char {
            new.push(' ');
        }
        new.push(c);
        first_char = false;
    }
    new
}
