//! Rust Generics implementation.
//!
//! Datatype-generic programming, also frequently just called generic programming or generics, is a
//! form of abstraction that allows defining functions that can operate on a large class of
//! data types. For a more in-depth introduction to generic programming in general, have a look at
//! [Datatype-Generic Programming](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/dgp.pdf), or
//! the [Libraries for Generic Programming](http://dreixel.net/research/pdf/lgph.pdf) paper.

#![deny(unconditional_recursion)]

#![allow(incomplete_features)] // To be removed, see: https://github.com/enso-org/ide/issues/1559
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

#![feature(const_fn_trait_bound)]
#![feature(specialization)]
#![feature(trait_alias)]

pub mod generic;
pub mod hlist;
pub mod tuple;

pub use generic::*;
pub use hlist::*;
pub use tuple::*;
