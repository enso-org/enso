//! This module contains functionality that allows ports and edges to be colored according
//! to their type information.

use crate::prelude::*;

use crate::SharedHashMap;
use crate::Type;

use ensogl::data::color;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;



// =================
// === Constants ===
// =================

/// Color that should be used if no type information is available.
pub const MISSING_TYPE_COLOR: color::Lcha = color::Lcha::new(0.5, 0.0, 0.0, 1.0);

const TYPE_COLOR_LUMINANCE : f32 = 0.5;
const TYPE_COLOR_CHROMA    : f32 = 0.8;



// ================================
// === Type to Color Conversion ===
// ================================

/// Return the color that corresponds to the given type. Can be used to color edges and ports.
pub fn color_for_type(type_information:Type) -> color::Lch {
    // 360 is a arbitrary intermediate value to normalise the hash from 0..u64::MAX to 0..1
    // Assuming the hash is random the number does not matter, otherwise 360 might better preserve
    // the relationship between hash values and make the value 360 and 361 look more similar.
    let hue = (type_to_hash(type_information) % 360) as f32 / 360.0;
    color::Lch::new(TYPE_COLOR_LUMINANCE,TYPE_COLOR_CHROMA,hue)
}

/// Compute the hash of the type for use in the `color_for_type` function.
fn type_to_hash(type_information:Type) -> u64 {
    let mut hasher = DefaultHasher::new();
    type_information.hash(&mut hasher);
    hasher.finish()
}



// ======================
// === Type Color Map ===
// ======================

/// `Allows to keep track of the type and color of a `ast::Id`. It allows to store the
/// `ast::Id` -> `Type` mapping and infer the colour for the given `ast::Id` from that through the
/// `type_color` method.
#[derive(Clone,CloneRef,Debug,Default)]
pub struct TypeColorMap {
    data: SharedHashMap<ast::Id,Type>,
}

impl TypeColorMap {
    /// Return the colour for the `ast_id`. If no type information is available, returns `None`.
    pub fn type_color(&self, ast_id:ast::Id) -> Option<color::Lcha> {
        self.data.get_cloned(&ast_id).map(|type_information| {
            color_for_type(type_information).into()
        })
    }

    /// Set the type for the given `ast::Id`. Discards the type, if `None` is given as value.
    pub fn update_entry(&self, key:ast::Id, value:Option<Type>) {
        match value {
            Some(value) => self.data.insert(key,value),
            None        => self.data.remove(&key),
        };
    }
}
