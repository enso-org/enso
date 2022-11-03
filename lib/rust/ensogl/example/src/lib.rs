//! The Visual Part of IDE.
//!
//! This crate has the all code for displaying GUI of Enso IDE application. The views provides
//! the FRP endpoints to communicate with controllers. It also have a mocked debug scenes of IDE.

#![recursion_limit = "1024"]
// === Features ===
#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


// ==============
// === Export ===
// ==============

pub use ensogl_example_animation as animation;
pub use ensogl_example_complex_shape_system as complex_shape_system;
pub use ensogl_example_dom_symbols as dom_symbols;
pub use ensogl_example_drop_manager as drop_manager;
pub use ensogl_example_easing_animator as easing_animator;
pub use ensogl_example_grid_view as grid_view;
pub use ensogl_example_list_view as list_view;
pub use ensogl_example_mouse_events as mouse_events;
pub use ensogl_example_profiling_run_graph as profiling_run_graph;
pub use ensogl_example_render_profile_flamegraph as render_profile_flamegraph;
pub use ensogl_example_scroll_area as scroll_area;
pub use ensogl_example_slider as slider;
pub use ensogl_example_sprite_system as sprite_system;
pub use ensogl_example_sprite_system_benchmark as sprite_system_benchmark;
pub use ensogl_example_text_area as text_area;
