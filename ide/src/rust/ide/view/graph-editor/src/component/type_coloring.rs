//! This module contains functionality that allows ports and edges to be colored according
//! to their type information.

use crate::prelude::*;

use crate::Type;

use ensogl::data::color;
use ensogl::display::shape::StyleWatch;
use ensogl::display::style::data::DataMatch;
use ensogl_theme as theme;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;



// ================================
// === Type to Color Conversion ===
// ================================

/// Return the color that corresponds to the given type. Can be used to color edges and ports.
///
/// ## Overview
/// Currently, this is a very simple mechanism, which infers color based on the input type name.
/// There are many ways we can improve it in the future, but it will also require Engine to develop
/// better type inference mechanisms. For example, currently we do not get information about
/// parametric types, so we don't know whether something is a `Vector Number`, `Vector String`, or
/// something else. It is just `Vector` and it is equivalent to `Vector Any`.
///
/// ## Theme Defined Colors
/// Please note that this function queries the currently selected theme for special colors. Themes
/// can define color overrides, like `code::types::String::hue = 0.3` to override the selected
/// color hue.
///
/// ## Future Development
/// There are few important ideas regarding future development of this mechanism. First of all,
/// user defining a new type should be able to mark it as a "container" type and point which of
/// parameters should be used to obtain the color. So for example, user defining the type
/// `Vector a` should be able to describe it as a "container", and point `a` as the type of its
/// items. This way, `Vector Number` should be colored in similar way to `Number` (maybe with some
/// additional "container edge style", like dashed lines). If user do not provide such
/// parametrization, other mechanisms should be used. For example, `Point Float` and `Point Number`
/// should have similar colors, completely distinct from their parameter types.
pub fn compute(tp:&Type, styles:&StyleWatch) -> color::Lcha {
    let hue_types_path = theme::code::types::overriden::HERE.path();
    let hue_style_path = hue_types_path.into_sub(tp.as_str()).into_sub("hue");
    let hue            = styles.get(hue_style_path).number_or_else(||auto_hue(tp));
    let luminance      = styles.get_number_or(theme::code::types::luminance,0.85);
    let chroma         = styles.get_number_or(theme::code::types::chroma,0.6);
    color::Lch::new(luminance,chroma,hue).into()
}

/// Computes LCH hue value based on incoming type information.
fn auto_hue(tp:&Type) -> f32 {
    // Defines how many different hue values we can have based on our incoming type name.
    let hue_step  = 512;
    let hue_shift = 0.0;
    (hash(&tp) % hue_step) as f32 / hue_step as f32 + hue_shift
}

/// Compute the hash of the type for use in the `compute` function.
fn hash(s:&str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}
