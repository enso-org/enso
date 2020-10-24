//! This module contains functionality that allows ports and edges to be colored according
//! to their type information.

use crate::prelude::*;

use crate::Type;

use ensogl::data::color;
use ensogl::display::shape::StyleWatch;
use ensogl_theme;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;



// ================================
// === Type to Color Conversion ===
// ================================

/// Return the color that corresponds to the given type. Can be used to color edges and ports.
///
/// Currently, this is a very simple mechanism, which infers color based on the input type name.
/// There are many ways we can improve it in the future, but it will also require Engine to develop
/// better type inference mechanisms. For example, currently we do not get information about
/// parametric types, so we don't know whether something is a `Vector Number`, `Vector String`, or
/// something else. It is just `Vector` and it is equivalent to `Vector Any`.
///
/// There are few important ideas regarding future development of this mechanism. First of all,
/// user defining a new type should be able to mark it as a "container" type and point which of
/// parameters should be used to obtain the color. So for example, user defining the type
/// `Vector a` should be able to describe it as a "container", and point `a` as the type of its
/// items. This way, `Vector Number` should be colored in similar way to `Number` (maybe with some
/// additional "container edge style", like dashed lines). If user do not provide such
/// parametrization, other mechanisms should be used. For example, `Point Float` and `Point Number`
/// should have similar colors, completely distinct from their parameter types.
pub fn color_for_type(type_information:Type, styles:&StyleWatch) -> color::Lcha {
    // 360 is a arbitrary intermediate value to normalise the hash from 0..u64::MAX to 0..1
    // Assuming the hash is random the number does not matter, otherwise 360 might better preserve
    // the relationship between hash values and make the value 360 and 361 look more similar.
    let angle           = 360;
    let hue             = (str_to_hash(&type_information) % angle) as f32 / angle as f32;
    let luminance_path  = ensogl_theme::vars::graph_editor::edge::_type::color::luminance;
    let chroma_path     = ensogl_theme::vars::graph_editor::edge::_type::color::chroma;
    let color_luminance = styles.get_number_or(luminance_path,0.7);
    let color_chroma    = styles.get_number_or(chroma_path,0.6);
    color::Lch::new(color_luminance,color_chroma,hue).into()
}

/// Compute the hash of the type for use in the `color_for_type` function.
fn str_to_hash(s:&str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}



// // ======================
// // === Type Color Map ===
// // ======================
//
// /// `Allows to keep track of the type and color of a `ast::Id`. It allows to store the
// /// `ast::Id` -> `Type` mapping and infer the colour for the given `ast::Id` from that through the
// /// `type_color` method.
// #[derive(Clone,CloneRef,Debug,Default)]
// pub struct TypeColorMap {
//     data: SharedHashMap<ast::Id,Type>,
// }
//
// impl TypeColorMap {
//     /// Return the colour for the `ast_id`. If no type information is available, returns `None`.
//     pub fn type_color(&self, ast_id:ast::Id, style:&StyleWatch) -> Option<color::Lcha> {
//         self.data.get_cloned(&ast_id).map(|type_information| {
//             color_for_type(type_information,style)
//         })
//     }
//
//     /// Set the type for the given `ast::Id`. Discards the type, if `None` is given as value.
//     pub fn update_entry(&self, key:ast::Id, value:Option<Type>) {
//         match value {
//             Some(value) => self.data.insert(key,value),
//             None        => self.data.remove(&key),
//         };
//     }
// }
