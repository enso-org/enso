//! This module defines a macro that makes it easy to access multiple values from a theme at the
//! same time. It creates a FRP endpoint that contains all the desired values, and will be updated
//! if any of the values changes.
//!
//! The derive creates a method `from_theme` that can be used to receive updates to any of the
//! values. A base path for the values can be set via the `base_path` attribute. The field name is
//! appended to `base_path` to get the full path to the value in the theme. It can be overridden
//! using the `theme_path` attribute on the field.
//!
//! A custom accessor function can be provided for each field using the `accessor` attribute. This
//! function should have a following signature:
//!
//! ```ignore
//! fn accessor<P: Into<ensogl_core::display::style::Path>>(
//!    network: &frp::Network,
//!    style: &StyleWatchFrp,
//!    path: P,
//! ) -> frp::Sampler<T>
//! ```
//! where `T` is the type of the field. This accessor will be used to retrieve the value from the
//! stylesheet. If no accessor is provided, a standard getters of the [`StyleWatchFrp`] are used
//! instead.
//!
//! Example usage
//!```no_compile
//! use ensogl_core::data::color;
//! use ensogl_derive_theme::FromTheme;
//!
//! #[derive(FromTheme)]
//! #[base_path = "ensogl_hardcoded_theme"]
//! struct Style {
//!     some_number: f32,
//!     some_color:  color::Rgba,
//!     #[theme_path = "ensogl_hardcoded_theme::some_path::label"]
//!     some_label:  String,
//!     #[accessor = "my_custom_accessor"]
//!     some_custom_value: CustomType,
//! }
//! ```

#![recursion_limit = "512"]
// === Features ===
#![allow(incomplete_features)]
#![feature(associated_type_defaults)]
#![feature(cell_update)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(marker_trait_attr)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(trace_macros)]
#![feature(const_trait_impl)]
#![feature(slice_as_chunks)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
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



extern crate proc_macro;

use proc_macro::TokenStream;
use syn::parse_macro_input;
use syn::DeriveInput;

mod from_theme;


/// Implements the `FromTheme` derive macro. See thr crate docs for more information.
#[proc_macro_derive(FromTheme, attributes(base_path, theme_path, accessor))]
pub fn derive_from_thee(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    from_theme::expand(input).into()
}
