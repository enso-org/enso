use crate::prelude::*;

use crate::entry::Contour;

use ensogl_core::data::color;
use ensogl_scroll_area::Viewport;

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
        let hover_trans = highlights_pos.xy().px();
        let hover = hover.translate(&hover_trans);
        let hover = (&hover * &viewport).fill(hover_color);
        let selection = Rect(highlights_sizes.zw().px()).corners_radius(corners_radii.z().px());
        let selection = hover.translate(highlights_pos.zw().px() - hover_trans);
        let selection = (&selection * &viewport).fill(selection_color);
        let highlights = &hover + &selection;
        highlights.into()
    }
}

pub fn set_viewport(shape: &View, viewport: Viewport) {
    shape.size.set(viewport.size());
    shape.set_position_xy(viewport.center_point());
}

pub trait AttrSetter {
    fn set_position(shape: &View, position: Vector2, viewport: Viewport);
    fn set_size(shape: &View, size: Vector2);
    fn set_corners_radius(shape: &View, radius: f32);
    fn set_color(shape: &View, color: color::Rgba);
}

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
