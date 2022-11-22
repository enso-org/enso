//! This module defines the visualization widgets and related functionality.
//!
//! The overall architecture of visualizations consists of four parts:
//!
//! 1. The `DataRenderer` trait provides the functionality to render the actual visualization view
//!    that implements `display::Object`. It is provided with data and itself provides frp streams
//!    of its output if there is some, for example, if it acts as a widget.
//!
//! 2. The `Visualization` is a struct that wraps the `DataRenderer` and implements the generic
//!    tasks that are the same for all visualizations. That is, interfacing with the other UI
//!    elements, the visualization registry, as well as propagating frp messages.
//!
//! 3. The `Class` is a struct that can instantiate multiple `Visualization` structs and provides
//!    data about the visualization even before they are instantiated like input datatype and name.
//!
//! 4. The `Container` wraps the `Visualization` and provides the UI elements that facilitate
//!    user interactions. For example, selecting a visualization or connecting it to nodes in the
//!    graph editor scene.
//!
//! In addition this module also contains a `Data` struct that provides a dynamically typed way to
//! handle data for visualizations. This allows the `Visualization` struct to be without type
//! parameters and simplifies the FRP communication and complexity of the node system.

// FIXME: please update the above docs.


// ==============
// === Export ===
// ==============

pub mod container;
pub mod data;
pub mod definition;
pub mod foreign;
pub mod instance;
pub mod layer;
pub mod metadata;
pub mod path;
pub mod registry;

pub use container::*;
pub use data::*;
pub use definition::*;
pub use foreign::*;
pub use instance::Instance;
pub use layer::*;
pub use metadata::*;
pub use path::*;
pub use registry::*;
