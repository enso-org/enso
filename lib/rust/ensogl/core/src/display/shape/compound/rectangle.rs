//! A rectangle shape with numerous parameters allowing drawing diverse range of shapes,
//! such as circles, rings, or ring segments. The advantage of having a singular shape for these
//! cases is that a single draw call can be used to render multiple GUI elements, which ultimately
//! enhances performance.

use crate::prelude::*;

use crate::data::color;
use crate::display;



// =============
// === Shape ===
// =============

mod shape {
    use super::*;
    crate::shape! {
        (
            style: Style,
            color: Vector4,
            corner_radius: f32,
            inset: f32,
            border: f32,
            border_color: Vector4,
            clip: Vector2,
        ) {
            // === Canvas ===
            let canvas_width = Var::<Pixels>::from("input_size.x");
            let canvas_height = Var::<Pixels>::from("input_size.y");

            // === Clip ===
            // Clipping scales the shape in such a way, that the visible part will occupy whole
            // canvas area. Thus, we need to recompute the new canvas size for the scaled shape.
            let canvas_clip_height_diff = &canvas_height * (clip.y() * 2.0);
            let canvas_clip_width_diff = &canvas_width * (clip.x() * 2.0);
            let canvas_height = canvas_height + &canvas_clip_height_diff.abs();
            let canvas_width = canvas_width + &canvas_clip_width_diff.abs();

            // === Body ===
            let inset2 = (&inset * 2.0).px();
            let width = &canvas_width - &inset2;
            let height = &canvas_height - &inset2;
            let color = Var::<color::Rgba>::from(color);
            let body = Rect((&width, &height)).corners_radius(corner_radius.px());
            let body = body.fill(color);

            // === Border ===
            let border = body.grow(border.px());
            let border_color = Var::<color::Rgba>::from(border_color);
            let border = border.fill(border_color);

            // === Shape ===
            let shape = border.union_exclusive(&body);

            // === Clip Adjustment ===
            let shape = shape.translate((-canvas_clip_width_diff/2.0, -canvas_clip_height_diff/2.0));
            shape.into()
        }
    }
}



// =================
// === Rectangle ===
// =================

/// A rectangle shape with the following configurable properties:
/// - The body color of the shape.
/// - The corner radius of the shape.
/// - The inset, padding between edge of the frame and shape itself.
/// - The border width and color.
/// - The clipping of the shape (e.g. clipping bottom half of the shape).
///
/// # Performance
/// This shape has been specifically designed to be utilized across various sections of the GUI. Its
/// numerous parameters enable a highly adaptable approach to drawing a diverse range of shapes,
/// such as circles, rings, or ring segments. The advantage of having a singular shape for these
/// cases is that a single draw call can be used to render multiple GUI elements, which ultimately
/// enhances performance.
#[derive(Clone, CloneRef, Debug, Deref, Default)]
#[allow(missing_docs)]
pub struct Rectangle {
    pub view: shape::View,
}

impl Rectangle {
    fn modify_view(&self, f: impl FnOnce(&shape::View)) -> &Self {
        f(&self.view);
        self
    }

    /// Constructor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Builder-style modifier, allowing setting shape properties without creating a temporary
    /// variable after its construction.
    pub fn build(self, f: impl FnOnce(&Self)) -> Self {
        f(&self);
        self
    }

    /// Set the color of the body of the shape.
    pub fn set_color(&self, color: color::Rgba) -> &Self {
        self.modify_view(|view| view.color.set(color.into()))
    }

    /// Set the corner radius. If the corner radius will be larger than possible (e.g. larger than
    /// the shape dimension), it will be clamped to the highest possible value.
    pub fn set_corner_radius(&self, radius: f32) -> &Self {
        self.modify_view(|view| view.corner_radius.set(radius))
    }

    /// Set the corner radius to maximum. If the width and height of the shape are equal, it will
    /// result in a circle.
    pub fn set_corner_radius_max(&self) -> &Self {
        // We are using here a value bigger than anything we will ever need. We are not using
        // biggest possible GLSL float value in order not to get rendering artifacts.
        let max_radius = 1000000.0;
        self.set_corner_radius(max_radius)
    }

    /// Set the padding between edge of the frame and shape itself. If you want to use border, you
    /// should always set the inset at least of the size of the border. If you do not want the
    /// border to be animated, you can use [`Self::set_inset_border`] instead.
    pub fn set_inset(&self, inset: f32) -> &Self {
        self.modify_view(|view| view.inset.set(inset))
    }

    /// Set the border size of the shape. If you want to use border, you should always set the inset
    /// at least of the size of the border. If you do not want the border to be animated, you can
    /// use [`Self::set_inset_border`] instead.
    pub fn set_border(&self, border: f32) -> &Self {
        self.modify_view(|view| view.border.set(border))
    }

    /// Set both the inset and border at once. See documentation of [`Self::set_border`] and
    /// [`Self::set_inset`] to learn more.
    pub fn set_inset_border(&self, border: f32) -> &Self {
        self.set_inset(border).set_border(border)
    }

    /// Set the border color.
    pub fn set_border_color(&self, color: color::Rgba) -> &Self {
        self.modify_view(|view| view.border_color.set(color.into()))
    }

    /// Set clipping of the shape. The clipping is normalized, which means, that the value of 0.5
    /// means that we are clipping 50% of the shape. For positive clip values, the clipping is
    /// performed always on the left and on the bottom of the shape. For negative clip values, the
    /// clipping is performed on the right and on the top of the shape.
    /// [`Self::keep_bottom_half`].
    pub fn set_clip(&self, clip: Vector2) -> &Self {
        self.modify_view(|view| view.clip.set(clip))
    }

    /// Keep only the top half of the shape.
    pub fn keep_top_half(&self) -> &Self {
        self.set_clip(Vector2(0.0, 0.5))
    }

    /// Keep only the bottom half of the shape.
    pub fn keep_bottom_half(&self) -> &Self {
        self.set_clip(Vector2(0.0, -0.5))
    }

    /// Keep only the right half of the shape.
    pub fn keep_right_half(&self) -> &Self {
        self.set_clip(Vector2(0.5, 0.0))
    }

    /// Keep only the left half of the shape.
    pub fn keep_left_half(&self) -> &Self {
        self.set_clip(Vector2(-0.5, 0.0))
    }

    /// Keep only the top right quarter of the shape.
    pub fn keep_top_right_quarter(&self) -> &Self {
        self.set_clip(Vector2(0.5, 0.5))
    }

    /// Keep only the bottom right quarter of the shape.
    pub fn keep_bottom_right_quarter(&self) -> &Self {
        self.set_clip(Vector2(0.5, -0.5))
    }

    /// Keep only the bottom left quarter of the shape.
    pub fn keep_bottom_left_quarter(&self) -> &Self {
        self.set_clip(Vector2(-0.5, -0.5))
    }

    /// Keep only the top left quarter of the shape.
    pub fn keep_top_left_quarter(&self) -> &Self {
        self.set_clip(Vector2(-0.5, 0.5))
    }
}

impl display::Object for Rectangle {
    fn display_object(&self) -> &display::object::Instance {
        self.view.display_object()
    }
}

/// Rectangle constructor.
#[allow(non_snake_case)]
pub fn Rectangle() -> Rectangle {
    Rectangle::default()
}

/// Rounded rectangle constructor. It is a wrapper around [`Rectangle`] with a corner radius set.
#[allow(non_snake_case)]
pub fn RoundedRectangle(radius: f32) -> Rectangle {
    let shape = Rectangle();
    shape.set_corner_radius(radius);
    shape
}

/// Circle constructor. It is a wrapper around [`Rectangle`] with a corner radius set to maximum.
#[allow(non_snake_case)]
pub fn Circle() -> Rectangle {
    let shape = Rectangle();
    shape.set_corner_radius_max();
    shape
}
