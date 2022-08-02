//! A module containing the highlight shape.
//!
//! The Highlight shape handles both highlights at once (selection and hover), so we are sure
//! the selection highlight will always be displayed over hover highlight without making unnecessary
//! shape systems.
//!
//! The shape is clipped to the viewport "manually", because it is used as a mask for _Masked Layer_
//! (see [`crate::selectable::highlight::layer::Handler`], and masks in EnsoGL cannot be further
//! masked.
//!
//! # Setting Parameters
//!
//! The positioning, size, and other highlight parameters are not easily mapped to shape parameter
//! due to above reasons, and the fact that the number of parameters of one shape is constrained.
//! Use the helper functions defined in this module instead:
//! * Keep up-to-date the info about the grid view's viewport using [`set_viewport`].
//! * Set parameters of the specific highlight using one of [`AttrSetter`] instances:
//!   [`HoverAttrSetter`] or [`SelectionAttrSetter`]

use crate::prelude::*;

use ensogl_core::data::color;
use ensogl_scroll_area::Viewport;



// ========================
// === Shape Definition ===
// ========================

ensogl_core::define_shape_system! {
    pointer_events = false;
    (
        style: Style,
        // Corners radii of viewport (x), hover highlight (y) and selection highlight (z).
        corners_radii: Vector3,
        // Positions of highlights: hover (xy) and selection (zw).
        highlights_pos: Vector4,
        // Sizes of highlights: hover (xy) and selection (zw).
        highlights_sizes: Vector4,
        hover_color: Vector4,
        selection_color: Vector4,
    ) {
        let viewport_width = Var::<Pixels>::from("input_size.x");
        let viewport_height = Var::<Pixels>::from("input_size.y");
        let viewport = Rect((viewport_width, viewport_height));
        let viewport = viewport.corners_radius(corners_radii.x().px());
        let hover = Rect(highlights_sizes.xy().px()).corners_radius(corners_radii.y().px());
        let hover = hover.translate(highlights_pos.xy().px());
        let hover = (&hover * &viewport).fill(hover_color);
        let selection = Rect(highlights_sizes.zw().px()).corners_radius(corners_radii.z().px());
        let selection = selection.translate(highlights_pos.zw().px());
        let selection = (&selection * &viewport).fill(selection_color);
        let highlights = &hover + &selection;
        highlights.into()
    }
}



// ===========================
// === Parameters' Setters ===
// ===========================

// === set_viewport ===

/// Updates the shape's viewport. The position and size of the sprite will be updated. See
/// [module's docs](mod@self) for more info.
pub fn set_viewport(shape: &View, viewport: Viewport) {
    shape.size.set(viewport.size());
    shape.set_position_xy(viewport.center_point());
}


// === AttrSetter ===

/// The trait with setters for all attributes of a single highlight.
#[allow(missing_docs)]
pub trait AttrSetter {
    fn set_position(shape: &View, position: Vector2, viewport: Viewport);
    fn set_size(shape: &View, size: Vector2);
    fn set_corners_radius(shape: &View, radius: f32);
    fn set_color(shape: &View, color: color::Rgba);
}


// === HoverAttrSetter ===

/// Struct with setters for all attributes for hover highlight.
#[derive(Copy, Clone, Debug)]
pub struct HoverAttrSetter;

impl AttrSetter for HoverAttrSetter {
    fn set_position(shape: &View, position: Vector2, viewport: Viewport) {
        let viewport_position = viewport.center_point();
        let relative_pos = position - viewport_position;
        let mut attr = shape.highlights_pos.get();
        attr.x = relative_pos.x;
        attr.y = relative_pos.y;
        shape.highlights_pos.set(attr);
    }

    fn set_size(shape: &View, size: Vector2) {
        let mut attr = shape.highlights_sizes.get();
        attr.x = size.x;
        attr.y = size.y;
        shape.highlights_sizes.set(attr)
    }

    fn set_corners_radius(shape: &View, radius: f32) {
        let mut old_radii = shape.corners_radii.get();
        old_radii.y = radius;
        shape.corners_radii.set(old_radii);
    }

    fn set_color(shape: &View, color: color::Rgba) {
        shape.hover_color.set(color.into())
    }
}


// === SelectionAttrSetter ===

/// Struct with setters for all attributes for selection highlight.
#[derive(Copy, Clone, Debug)]
pub struct SelectionAttrSetter;

impl AttrSetter for SelectionAttrSetter {
    fn set_position(shape: &View, position: Vector2, viewport: Viewport) {
        let viewport_position = viewport.center_point();
        let relative_pos = position - viewport_position;
        let mut attr = shape.highlights_pos.get();
        attr.z = relative_pos.x;
        attr.w = relative_pos.y;
        shape.highlights_pos.set(attr);
    }

    fn set_size(shape: &View, size: Vector2) {
        let mut attr = shape.highlights_sizes.get();
        attr.z = size.x;
        attr.w = size.y;
        shape.highlights_sizes.set(attr)
    }

    fn set_corners_radius(shape: &View, radius: f32) {
        let mut old_radii = shape.corners_radii.get();
        old_radii.z = radius;
        shape.corners_radii.set(old_radii);
    }

    fn set_color(shape: &View, color: color::Rgba) {
        shape.selection_color.set(color.into())
    }
}
