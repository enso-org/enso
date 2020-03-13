//! This module contains all the controllers. They cover everything that is
//! between clients of remote services (like language server and file manager)
//! and views.
//!
//! The controllers create a tree-like structure, with project controller being
//! a root, then module controllers below, then graph/text controller and so on.
//!
//! As a general rule, while the "upper" (i.e. closer to root) nodes may keep
//! handles to the "lower" nodes (e.g. to allow their reuse), they should never
//! manage their lifetime.
//!
//! Primarily views are considered owners of their respective controllers.
//! Additionally, controllers are allowed to keep strong handle "upwards".
//!
//! Controllers store their handles using `utils::cell` handle types to ensure
//! that mutable state is safely accessed.

pub mod text;
pub mod module;
pub mod notification;
pub mod project;

/// General-purpose `Result` supporting any `Error`-compatible failures.
pub type FallibleResult<T> = Result<T,failure::Error>;
