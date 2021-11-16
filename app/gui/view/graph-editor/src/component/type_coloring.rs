//! This module contains functionality that allows ports and edges to be colored according
//! to their type information.

use crate::prelude::*;

use crate::Type;

use ensogl::data::color;
use ensogl::display::shape::StyleWatch;
use ensogl::display::style::data::DataMatch;
use ensogl_hardcoded_theme as theme;
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
pub fn compute(tp: &Type, styles: &StyleWatch) -> color::Lcha {
    let types_path = theme::code::types::overriden::HERE.path();
    let type_path = types_path.into_subs(tp.as_str().split('.'));
    let hue = styles.get(type_path.sub("hue")).number_or_else(|| auto_hue(tp, styles));
    let lightness = styles
        .get(type_path.sub("lightness"))
        .number_or_else(|| styles.get_number_or(theme::code::types::lightness, 0.85));
    let chroma = styles
        .get(type_path.sub("chroma"))
        .number_or_else(|| styles.get_number_or(theme::code::types::chroma, 0.6));
    color::Lch::new(lightness, chroma, hue).into()
}

/// Get the code color for the provided type or default code color in case the type is None.
pub fn compute_for_code(tp: Option<&Type>, styles: &StyleWatch) -> color::Lcha {
    let opt_color = tp.as_ref().map(|tp| compute(tp, styles));
    opt_color.unwrap_or_else(|| styles.get_color(theme::graph_editor::node::text).into())
}

/// Get the code color for the provided type or default code color in case the type is None.
pub fn compute_for_selection(tp: Option<&Type>, styles: &StyleWatch) -> color::Lcha {
    let opt_color = tp.as_ref().map(|tp| compute(tp, styles));
    opt_color.unwrap_or_else(|| styles.get_color(theme::code::types::any::selection).into())
}

/// Computes LCH hue value based on incoming type information.
fn auto_hue(tp: &Type, styles: &StyleWatch) -> f32 {
    // Defines how many hue values we can have based on our incoming type name.
    let hue_steps = styles.get_number_or(theme::code::types::hue_steps, 512.0);
    let hue_shift = styles.get_number_or(theme::code::types::hue_shift, 0.0);
    (hash(tp) % (hue_steps as u64)) as f32 / hue_steps + hue_shift
}

/// Compute the hash of the type for use in the `compute` function.
fn hash(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}
