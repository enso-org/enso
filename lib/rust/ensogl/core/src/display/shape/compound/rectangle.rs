//! A rectangle shape with numerous parameters allowing drawing diverse range of shapes,
//! such as circles, rings, or ring segments. The advantage of having a singular shape for these
//! cases is that a single draw call can be used to render multiple GUI elements, which ultimately
//! enhances performance.

use crate::prelude::*;

use crate::data::color;
use crate::display;
use crate::display::shape::StyleWatchFrp;
use crate::display::style::data::DataMatch;
use crate::display::style::Path;


// ==============
// === Export ===
// ==============

pub use shape::Shape;



// =================
// === Constants ===
// =================

/// The threshold of minimum border width, below which the border is considered to be not visible.
/// This is necessary to be able to avoid rendering zero-width border completely, removing any
/// artifacts caused by anti-aliasing near the zero value.
const MINIMUM_BORDER_WIDTH: f32 = 0.1;


// =============
// === Shape ===
// =============

/// Shape definition.
pub mod shape {
    use super::*;
    crate::shape! {
        pointer_events_instanced = true;
        (
            style: Style,
            color: Vector4,
            border_color: Vector4,
            clip: Vector2,
            corner_radius: f32,
            inset: f32,
            border: f32,
            rotate: f32,
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
            let inset2 = (Max::max(inset.clone(), Var::from(0.0)) * 2.0).px();
            let width = &canvas_width - &inset2;
            let height = &canvas_height - &inset2;
            let body = Rect((&width, &height)).corners_radius(corner_radius.px());

            // === Border ===
            let border_center = &inset * border.negative() + &border * 0.5;
            let abs_border = border.abs();
            // when border width is close enough to zero, offset it thickness into far negatives to
            // avoid rendering it completely. Necessary due to anti-aliasing.
            let border_below_threshold = (&abs_border - Var::from(MINIMUM_BORDER_WIDTH)).negative();
            let border_thickness = abs_border - border_below_threshold * 1000.0;
            let border_body = body.grow(border_center.px()).stroke(border_thickness.px());

            // When the border is touching the edge of the body, extend the body by up to a pixel.
            // That way there is no visual gap between the shapes caused by anti-aliasing. In those
            // scenarios, the extended body will be occluded by the border, therefore it will not
            // have any visible effect, other than removing the unwanted artifact.
            let fwidth = Var::<f32>::from("fwidth(position.x)");
            let touch_offset = border.clamp(0.0.into(), fwidth);
            let body = body.grow(touch_offset);

            // === Shape ===
            let color = Var::<color::Rgba>::from(color);
            let border_color = Var::<color::Rgba>::from(border_color);
            let colored_body = body.fill(color);
            let colored_border = border_body.fill(border_color);
            let shape = colored_body.union_exclusive(&colored_border);

            // === Rotation ===
            // Rotate about one corner.
            let shape = shape
                .translate((&canvas_width/2.0, &canvas_height/2.0))
                .rotate(rotate)
                .translate((-&canvas_width/2.0, -&canvas_height/2.0));

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
#[derive(Clone, CloneRef, Deref, display::Object)]
#[allow(missing_docs)]
pub struct Rectangle {
    pub view: shape::View,
}

impl Default for Rectangle {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Rectangle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rectangle").finish()
    }
}

impl Rectangle {
    fn modify_view(&self, f: impl FnOnce(&shape::View)) -> &Self {
        f(&self.view);
        self
    }

    /// Constructor.
    pub fn new() -> Self {
        Self { view: default() }.build(|r| {
            r.set_border_color(display::shape::INVISIBLE_HOVER_COLOR);
        })
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

    /// Set the corner radius of the body.
    ///
    /// Note that the corner radius of the border will be different, depending on the chosen border
    /// settings. The corner radius of border shape will always naturally flow around the rectangle
    /// body, following its curvature and keeping corners concentric.
    ///
    /// For [outer border](Self::set_border), the outer border's radius will be equal to the body's
    /// corner radius plus the border width. For [inner border](Self::set_border_inner), the inner
    /// border's radius will be smaller, depending on used distance.
    ///
    /// If the corner radius will be larger than the body size, it will be clamped to at most half
    /// of the smaller body dimension.
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

    /// Set the padding between edge of the frame and main shape.
    ///
    /// This value should not be less than the width of the border. To set it to the same width as
    /// the border, you can use [`Self::set_border_and_inset`].
    ///
    /// If the inset value is greater than the absolute value of border width, the shape will have
    /// additional transparent padding. If the inset value is positive, the extra padding will
    /// be between the body and the outside frame (size) of the rectangle. When it is negative,
    /// it can be used to inset the border into the body. To avoid confusion, it is recommended
    /// to use [`Self::set_inner_border`] instead.
    pub fn set_inset(&self, inset: f32) -> &Self {
        self.modify_view(|view| view.inset.set(inset))
    }

    /// Set the outer border size of the shape. The border will grow outwards from the rectangle
    /// body, towards the frame. In order to accommodate its width, use [`Self::set_inset`] to set
    /// the inset value to be greater or equal to the maximum planned border's width.
    ///
    /// If you don't plan to animate the border width, you can use [`Self::set_border_and_inset`] to
    /// set both settings to the same value.
    pub fn set_border(&self, border_width: f32) -> &Self {
        self.modify_view(|view| view.border.set(border_width.max(0.0)))
    }

    /// Set the inner border size of the shape, positioned within the rectangle body. The inner
    /// border can be offset away from the body's edge by using a non-zero `distance` value.
    ///
    /// When the `distance` is set to 0, the effect is similar to an outer border set using
    /// [`Self::set_border_and_inset`], but the interpretation of corner radius is different. In
    /// this case, the effective corner radius of the border's outside edge will be equal to the
    /// body's, corner radius, while the inside edge corners will be appropriately sharper.
    ///
    /// NOTE: This setting will override the inset value set with [`Self::set_inset`], setting it to
    /// a negative value. Therefore mixing it with other border settings is not recommended.
    pub fn set_inner_border(&self, border_width: f32, distance: f32) -> &Self {
        self.modify_view(|view| {
            let clamped_width = border_width.max(0.0);
            view.border.set(-clamped_width);
            view.inset.set(-distance.max(0.0));
        })
    }

    /// Set the frame border size of the shape. The border will grow inwards from the frame's
    /// outside edge, towards the rectangle body. In order to create space between the body and
    /// frame's border, use [`Self::set_inset`] to set the inset value to be greater or equal to the
    /// maximum planned border's width.
    ///
    /// If you don't plan to animate the border width, you can use [`Self::set_border_and_inset`] to
    /// set both settings to the same value.
    pub fn set_frame_border(&self, border_width: f32) -> &Self {
        self.modify_view(|view| view.border.set(-border_width.max(0.0)))
    }

    /// Set both the inset and border at once. The effect is visually similar to an inner border set
    /// using [`Self::set_inner_border`] with distance set to 0, but the interpretation of corner
    /// radius is different. In this case, the effective corner radius of the border's outside edge
    /// will be equal to the body's corner radius plus the border width.
    ///
    /// See documentation of [`Self::set_border`] and [`Self::set_inset`] to learn more. To make the
    /// border visible, you also need to set the border color using [`Self::set_border_color`].
    pub fn set_border_and_inset(&self, border: f32) -> &Self {
        self.set_inset(border).set_border(border)
    }

    /// Set the border color.
    pub fn set_border_color(&self, color: color::Rgba) -> &Self {
        self.modify_view(|view| view.border_color.set(color.into()))
    }

    /// Set whether the shape interacts with the mouse.
    pub fn set_pointer_events(&self, enabled: bool) -> &Self {
        let disabled = match enabled {
            true => 0.0,
            false => 1.0,
        };
        self.modify_view(|view| view.disable_pointer_events.set(disabled))
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

    /// Set the style properties from the given [`StyleWatchFrp`].
    pub fn set_style(&self, path: impl Into<Path>, style: &StyleWatchFrp) {
        let path = path.into();
        macro_rules! set_property {
            ($name:ident: $ty:ident) => {{
                let value = style.get(path.sub(stringify!($name))).value();
                let value = value.and_then(|value| value.$ty());
                if let Some(value) = value {
                    self.view.$name.set(value.into());
                }
            }};
        }
        set_property!(corner_radius: number);
        set_property!(color: color);
        set_property!(border_color: color);
        set_property!(inset: number);
        set_property!(border: number);
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

/// Right triangle constructor. It is a wrapper around [`Rectangle`] that constructs a right
/// triangle, pointing up. Its size can be determined by `set_size_xy`; the `x` and `y` factors
/// should be equal. The triangle drawn will extend from the bottom of the bounding box to the point
/// in the center.
#[allow(non_snake_case)]
pub fn RightTriangle() -> Rectangle {
    let shape = Rectangle();
    shape.view.rotate.set(std::f32::consts::FRAC_PI_4);
    // We will produce a right triangle by rotating a square 45°, and then rectilinearly clipping.
    //
    // The part of the shape we keep is based on the point where the edge of the rotated square
    // intersects with the edges of the unrotated square.
    //     +------+
    //     |      |
    //     |      |
    //     |   /\ |
    //     |  /  \|
    //     | /    x <--- here
    //     |/     |\
    //     +------+ \
    //      \       /
    //       \     /
    //        \   /
    //         \ /
    //
    // When we have that value, we can rectilinearly clip the rotated square to produce a right
    // triangle:
    //     . -----+
    //     | |    |
    //     | |    |
    //     | | /\ | <-- keep this part
    //     | |/  \|
    //     | +----+
    //     |
    //     +------.  <---- bounds of the unclipped unrotated shape (rotated square not shown)
    //
    // The critical intersection point is at (√2-1, √2-1) [this is found by intersecting the rotated
    // top line, `y = √2 - x`, and the original right line, `x = 1`].
    // Let's call this point `(x1, x1)`.
    let x1 = std::f32::consts::SQRT_2 - 1.0;
    // We will provide a clip value that keeps the part beyond this point. First we'll find the
    // formula for `clip`:
    // `unclipped_height = (1 + 2 * clip) * canvas_height` [From shape definition.]
    // `clip = ((unclipped_height / canvas_height) - 1) / 2` [Solve for clip.]
    //
    // Now, let's express x1 as a relationship between the size of shape to draw, and the size of
    // the bounding box to keep:
    // `canvas_height / unclipped_height = (1 - x1)`.
    //
    // Substituting into the `clip` formula:
    // `clip = ((1 / (1 - x1)) - 1) / 2`
    let clip = (((1.0 - x1).recip()) - 1.0) / 2.0;
    shape.view.clip.set(Vector2(clip, clip));
    shape
}



// ========================
// === Simple triangles ===
// ========================

/// An isosceles triangle of any apex angle, drawn using a single `Rectangle`. Note that borders are
/// not not supported by this triangle implementation, and if rounding is used it will be
/// elliptical. This is a result of this implementation being based on scaling a right triangle.
///
/// (There is an alternative approach based on keeping the opposite corner of the shape when
/// clipping to produce of wedge with a border on one side, and drawing two wedges together to
/// achieve any angle with correct borders and rounding. However, this approach requires two
/// `Rectangle`s to draw each triangle, and as of this writing we have no need for triangles with
/// borders.)
#[derive(Debug, Clone, display::Object)]
pub struct SimpleTriangle {
    shape: Rectangle,
}

impl SimpleTriangle {
    /// Return an upward-pointing isosceles rectangle with the given base width and altitude.
    pub fn from_base_and_altitude(base: f32, altitude: f32) -> Self {
        let shape = RightTriangle();
        shape.set_size(Vector2(base, base));
        shape.set_scale_y(altitude / (base / 2.0));
        shape.set_border_color(color::Rgba::transparent());
        Self { shape }
    }

    /// Return an upward-pointing isosceles rectangle sized to fit a bounding box of the given size.
    pub fn from_size(size: Vector2) -> Self {
        let (base, altitude) = (size.x(), size.y());
        Self::from_base_and_altitude(base, altitude)
    }

    /// Set whether the shape receives pointer events.
    pub fn set_pointer_events(&self, value: bool) {
        self.shape.set_pointer_events(value);
    }
}

impl From<SimpleTriangle> for Rectangle {
    fn from(value: SimpleTriangle) -> Self {
        value.shape
    }
}
