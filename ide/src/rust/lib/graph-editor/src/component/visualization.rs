//! This module defines the visualization widgets and related functionality.
//!
//! The overall architecture of visualizations consists of three parts:
//!
//! 1. The `DataRenderer` trait provides the functionality to render the actual visualization view
//!    that implements `display::Object`. It is provided with data and itself provides frp streams
//!    of its output if there is some, for example, if it acts as a widget.
//!
//! 2. The `Visualization` is a struct that wraps the `DataRenderer` and implements the generic
//!    tasks that are the same for all visualisations. That is, interfacing with the other UI
//!    elements, the visualization registry, as well as propagating frp messages.
//!
//! 3. The `Container` wraps the `Visualisation` and provides the UI elements that facilitate
//!    user interactions. For example, selecting a visualisation or connecting it to nodes in the
//!    graph editor scene.
//!
//! In addition this module also contains a `Data` struct that provides a dynamically typed way to
//! handle data for visualisations. This allows the `Visualisation` struct to be without type
//! parameters and simplifies the FRP communication and complexity of the node system.

pub mod class;
pub mod container;
pub mod renderer;
pub mod data;

pub use class::*;
pub use data::*;
pub use container::*;
pub use renderer::*;
