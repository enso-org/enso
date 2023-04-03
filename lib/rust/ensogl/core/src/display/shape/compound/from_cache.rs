//! A common shape systems displaying cached shapes.
//!
//! The [`basic one`](View) just displays its [`AnyCachedShape`] parameter.

use crate::prelude::*;

use crate::display::shape::AnyCachedShape;



// ===============================
// === Basic from_cache System ===
// ===============================

crate::shape! {
    pointer_events = false;
    alignment = center;
    (style: Style, icon: AnyCachedShape) {
        icon.into()
    }
}



// ===============================
// === from_cache_recolorized ===
// ===============================

/// A Shape parameterized by [`AnyCachedShape`] which display recolorized version of the cached
/// shape.
pub mod recolorized {
    crate::shape! {
        pointer_events = false;
        alignment = center;
        (style: Style, icon: AnyCachedShape, r_component: Vector4, g_component: Vector4, b_component: Vector4) {
            let r: Var<color::Rgba> = r_component.into();
            let g: Var<color::Rgba> = g_component.into();
            let b: Var<color::Rgba> = b_component.into();
            icon.recolorize(r, g, b).into()
        }
    }
}
