//! The Visual Part of IDE.
//!
//! This crate has the all code for displaying GUI of Enso IDE application. The views provides
//! the FRP endpoints to communicate with controllers. It also have a mocked debug scenes of IDE.

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "1024"]

pub use ensogl_example_animation as animation;
pub use ensogl_example_complex_shape_system as complex_shape_system;
pub use ensogl_example_dom_symbols as dom_symbols;
pub use ensogl_example_drop_manager as drop_manager;
pub use ensogl_example_easing_animator as easing_animator;
pub use ensogl_example_glyph_system as glyph_system;
pub use ensogl_example_list_view as list_view;
pub use ensogl_example_mouse_events as mouse_events;
pub use ensogl_example_scroll_area as scroll_area;
pub use ensogl_example_shape_system as shape_system;
pub use ensogl_example_slider as slider;
pub use ensogl_example_sprite_system as sprite_system;
pub use ensogl_example_sprite_system_benchmark as sprite_system_benchmark;
pub use ensogl_example_text_area as text_area;
