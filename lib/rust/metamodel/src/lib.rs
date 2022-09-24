//! A *metamodel* is a data model for data models in a particular typesystem. This crate defines a
//! few metamodels, some operations for inspecting and transforming data models within a metamodel,
//! and operations for translating a data model from one metamodel to another.
//!
//! # Modules
//!
//! The core modules define the metamodels, and operations on them:
//! - [`rust`]: A metamodel representing data models in the Rust typesystem.
//! - [`java`]: A metamodel representing data models in the Java typesystem.
//! - [`meta`]: An abstract metamodel, used to perform language-independent analysis of data models,
//!   and as an intermediate when translating data models between language-specific metamodels.
//!
//! Some accessory modules:
//! - [`graphviz`]: Support for rendering graphical representations of data models with GraphViz.
//!   This serves primarily to support developing and understanding transformations on and between
//!   metamodels.
//!
//! # Use cases
//!
//! The primary use case currently supported by this crate is Rust-to-Java datatype transpilation:
//! - Accept a Rust data model as an input (which may be obtained by the [`enso_reflect`] crate).
//! - Translate the data model to a Java data model (via the [`meta`] intermediate representation),
//!   using [`rust::to_meta`] and [`java::from_meta`].
//! - Derive deserialization for the Java data model, using [`java::bincode`].
//! - Generate Java code implementing the data model, using [`java::to_syntax`].
//!
//! Other use cases supported include:
//! - Analyze a data model's serialization to generate exhaustive test cases, using
//!   [`meta::serialization::testcases`].
//! - Produce graphs of type relationships, using [graphviz::Graph]`
//!
//! # Design
//!
//! A major design principle of this crate is: Operate on the most abstracted representation
//! possible. Primarily, this means we don't try to analyze or reason about *syntax* any more than
//! necessary. The [`rust`] data produced by [`enso_reflect`] is much higher-level than the [`syn`]
//! trees it is created from; it is easier to reason about a graph of datatypes than the tree of
//! tokens that implements it. The [`meta`] intermediate representation is even more abstract, and
//! simpler to operate on than Rust or Java. When we manipulate the data in Java terms (i.e. using
//! [`java::transform::optional_to_null`] to rewrite `Optional` types to nullable types), we do so
//! on the [`java`] graph of types. It is not until we are done with analysis and transformation
//! that we generate a [`java::syntax`] tree from the [`java`] types. [`java::syntax`] is treated as
//! write-only; we never try to inspect it, but just use its [`Display`] implementation to produce
//! Java code after all computation is completed.

// === Features ===
#![feature(map_first_last)]
#![feature(option_get_or_insert_default)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]



mod data_structures;
#[cfg(feature = "graphviz")]
pub mod graphviz;
#[cfg(feature = "java")]
pub mod java;
pub mod meta;
#[cfg(feature = "rust")]
pub mod rust;
