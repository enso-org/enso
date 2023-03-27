//! Example scene showing the usage of built-in high-level shapes.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;



// ==============
// === Shapes ===
// ==============

mod rectangle {
    use super::*;
    ensogl_core::shape! {(
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
            let canvas_height = canvas_height + &canvas_clip_height_diff;
            let canvas_width = canvas_width + &canvas_clip_width_diff;

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
    pub view: rectangle::View,
}

impl Rectangle {
    fn modify_view(&self, f: impl FnOnce(&rectangle::View)) -> &Self {
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
    /// means that we are clipping 50% of the shape. The clipping is performed always on the left
    /// and on the bottom of the shape. If you want to clip other sides of the shape, you can rotate
    /// it after clipping or use one of the predefined helper functions, such as
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
        self.keep_top_half().flip()
    }

    /// Keep only the right half of the shape.
    pub fn keep_right_half(&self) -> &Self {
        self.set_clip(Vector2(0.5, 0.0))
    }

    /// Keep only the left half of the shape.
    pub fn keep_left_half(&self) -> &Self {
        self.keep_right_half().flip()
    }

    /// Keep only the top right quarter of the shape.
    pub fn keep_top_right_quarter(&self) -> &Self {
        self.set_clip(Vector2(0.5, 0.5))
    }

    /// Keep only the bottom right quarter of the shape.
    pub fn keep_bottom_right_quarter(&self) -> &Self {
        self.keep_top_right_quarter().rotate_90()
    }

    /// Keep only the bottom left quarter of the shape.
    pub fn keep_bottom_left_quarter(&self) -> &Self {
        self.keep_bottom_right_quarter().rotate_180()
    }

    /// Keep only the top left quarter of the shape.
    pub fn keep_top_left_quarter(&self) -> &Self {
        self.keep_bottom_left_quarter().rotate_270()
    }

    /// Flip the shape via its center. This is equivalent to rotating the shape by 180 degrees.
    pub fn flip(&self) -> &Self {
        self.rotate_180()
    }

    /// Rotate the shape by 90 degrees.
    pub fn rotate_90(&self) -> &Self {
        self.modify_view(|view| view.set_rotation_z(-std::f32::consts::PI / 2.0))
    }

    /// Counter rotate the shape by 90 degrees.
    pub fn counter_rotate_90(&self) -> &Self {
        self.modify_view(|view| view.set_rotation_z(std::f32::consts::PI / 2.0))
    }

    /// Rotate the shape by 180 degrees.
    pub fn rotate_180(&self) -> &Self {
        self.modify_view(|view| view.set_rotation_z(-std::f32::consts::PI))
    }

    /// Counter rotate the shape by 180 degrees.
    pub fn rotate_270(&self) -> &Self {
        self.modify_view(|view| view.set_rotation_z(-3.0 / 2.0 * std::f32::consts::PI))
    }

    /// Counter rotate the shape by 270 degrees.
    pub fn counter_rotate_270(&self) -> &Self {
        self.modify_view(|view| view.set_rotation_z(3.0 / 2.0 * std::f32::consts::PI))
    }
}

impl display::Object for Rectangle {
    fn display_object(&self) -> &display::object::Instance {
        self.view.display_object()
    }
}

/// Rectangle constructor.
#[allow(non_snake_case)]
fn Rectangle() -> Rectangle {
    Rectangle::default()
}

/// Rounded rectangle constructor. It is a wrapper around [`Rectangle`] with a corner radius set.
#[allow(non_snake_case)]
fn RoundedRectangle(radius: f32) -> Rectangle {
    let shape = Rectangle();
    shape.set_corner_radius(radius);
    shape
}

/// Circle constructor. It is a wrapper around [`Rectangle`] with a corner radius set to maximum.
#[allow(non_snake_case)]
fn Circle() -> Rectangle {
    let shape = Rectangle();
    shape.set_corner_radius_max();
    shape
}


// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let shapes = [
        Circle().build(|t| {
            t.set_size(Vector2::new(100.0, 100.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_inset_border(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_bottom_left_quarter();
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(100.0, 100.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_inset_border(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0));
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(100.0, 50.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_inset_border(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_top_half();
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(100.0, 50.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_inset_border(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_bottom_half();
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(50.0, 100.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_inset_border(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_right_half();
        }),
        RoundedRectangle(10.0).build(|t| {
            t.set_size(Vector2::new(50.0, 100.0))
                .set_color(color::Rgba::new(0.5, 0.0, 0.0, 0.3))
                .set_inset_border(5.0)
                .set_border_color(color::Rgba::new(0.0, 0.0, 1.0, 1.0))
                .keep_left_half();
        }),
    ];

    let root = display::object::Instance::new();
    root.set_size(Vector2::new(300.0, 100.0));
    root.use_auto_layout().set_column_count(3).set_gap((10.0, 10.0));
    for shape in &shapes {
        root.add_child(shape);
    }
    world.add_child(&root);

    world.keep_alive_forever();
    mem::forget(navigator);
    mem::forget(root);
    mem::forget(shapes);
}
