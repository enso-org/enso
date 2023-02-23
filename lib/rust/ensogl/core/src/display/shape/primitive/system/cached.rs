//! Shape Systems with instance cached on global texture
//!
//! These systems are a valid [shape systems](super) which cannot be parameterized, but have
//! a single instance cached on a special texture (available in GLSL as `pass_cached_shapes`). The
//! texture keeps the color (except alpha) and the signed distance. The cached instance may be later
//! used inside other shape systems by using [`AnyCachedShape`].
//!
//! # Limitations
//!
//! * The cached shapes do not support alpha channels fully: it will be taken into consideration
//!   when adding shapes in their definition, but the alpha value will be dropped during rendering
//!   to the texture. See _Cached Shapes Texture_ section for details.
//! * The signed distance kept in the texture is capped at [`CACHED_TEXTURE_MAX_DISTANCE`]. This may
//!   give aliasing effects when zoomed out more than this distance.
//! * The shapes rendered from the texture may not always give the best quality results, see
//!   _Limitations_ section of [`AnyCachedShape`] docs.
//!
//! # Usages
//!
//! The main idea behind cached shapes is speeding up rendering applications with many shape
//! systems. Because each shape system is drawn with a different draw call, having e.g. a grid of
//! icons can increase the number of those, greatly worsening the application performance. But
//! instead all the icons can be rendered to texture, and the icon grid can use only one shape
//! system parameterized by the exact icon ID.
//!
//! # Cached Shapes Texture
//!
//! As a part of the [`World`](crate::display::world::World) initialization, the exact size of the
//! texture and shape positions is computed. The packing algorithm is defined in
//! [`arrange_on_texture`] module. Then, the shapes are rendered to texture in
//! [`CacheShapesPass`](crate::display::render::passes::CacheShapesPass) as soon as they are ready
//! (their shader is compiled).
//!
//! The texture is rendered in
//! [`CachdShapesTexture` display
//! mode](crate::display::shape::primitive::glsl::codes::DisplayModes), where the RGB channels keeps
//! the color of a given pixel (as one would expect), and the alpha channel contains the information
//! about signed distance from the shape boundary, linearly transformed so:
//! * 0.5 value means distance 0.0 distance,
//! * 0.0 is [`CACHED_TEXTURE_MAX_DISTANCE`],
//! * 1.0 is `-CACHED_TEXTURE_MAX_DISTANCE`.
//! As the alpha channel is capped at `0.0..=1.0` range, this will limit the stored signed distance
//! to `-CACHED_TEXTURE_MAX_DISTANCE..=CACHED_TEXTURE_MAX_DISTANCE`, which is one of the limitations
//! of cached shapes quality (see _Limitations_ section).
//!
//! # Using Cached Shapes
//!
//! The shapes may be read from the texture by creating [`AnyCachedShape`]. This structure can also
//! be a parameter for shape systems defined with [`shape!`] macro. See [the docs](AnyCachedShape)
//! for details.
//!
//! # Example
//!
//! ```rust
//! use ensogl_core::display::shape::*;
//!
//!
//! // === Defining Cached Shapes ===
//!
//! mod cached {
//!     use super::*;
//!     ensogl_core::cached_shape! { 32 x 32;
//!        (_style: Style) { Circle(16.px()).into() }
//!     }
//! }
//!
//! mod another_cached {
//!     use super::*;
//!     ensogl_core::cached_shape! { 32 x 32;
//!        (_style: Style) { Circle(16.px()).into() }
//!     }
//! }
//!
//!
//! // === Using Cached Shapes ===
//!
//! mod shape {
//!     use super::*;
//!
//!     ensogl_core::shape! {
//!         (_style: Style) {
//!             let bg = Rect((100.px(), 100.px())).fill(color::Rgba::white());
//!             // Our shape may be very complex, lets read it from the texture.
//!             let with_bg = &bg + &AnyCachedShape::<crate::cached::Shape>();
//!             with_bg.into()
//!         }
//!     }
//! }
//!
//! /// A parametrized shape, allowing us to draw many cached shapes in a single draw call.
//! mod parameterized_shape {
//!     use super::*;
//!     ensogl_core::shape! {
//!         (_style: Style, icon: AnyCachedShape) {
//!             let bg = Rect((100.px(), 100.px())).fill(color::Rgba::white());
//!             let with_bg = &bg + &icon;
//!             with_bg.into()
//!         }
//!     }
//! }
//!
//! # fn main() {
//! #   let _world = ensogl_core::display::world::World::new();
//! let shape = parameterized_shape::View::new();
//! shape.icon.set(cached::Shape::any_cached_shape_parameter());
//! // We can change the icon if we want:
//! shape.icon.set(another_cached::Shape::any_cached_shape_parameter());
//! # }
//! ```

use crate::prelude::*;

use crate::data::bounding_box::BoundingBox;
use crate::display::shape;
use crate::display::shape::canvas;
use crate::display::shape::canvas::Canvas;
use crate::display::shape::class::ShapeRef;
use crate::display::shape::system::cached::arrange_on_texture::arrange_shapes_on_texture;
use crate::display::shape::system::cached::arrange_on_texture::ShapeWithPosition;
use crate::display::shape::system::cached::arrange_on_texture::ShapeWithSize;
use crate::display::shape::AnyShape;
use crate::display::shape::Parameter;
use crate::display::shape::Var;
use crate::display::world::CACHED_SHAPES_DEFINITIONS;
use crate::display::IntoGlsl;
use crate::gui::component::ShapeView;


// ==============
// === Export ===
// ==============

pub mod arrange_on_texture;



// =================
// === Constants ===
// =================

/// The maximum absolute value of signed distance stored in the Texture. See
/// [full cached shapes documentation](self) for information why we need it.
pub const CACHED_TEXTURE_MAX_DISTANCE: f32 = 8.0;

/// A parameter of [`arrange_shapes_on_texture`] algorithm: the initial assumed size for the texture
/// with cached shapes. If it turn out to be too small, we extend its size and try again.
const INITIAL_TEXTURE_SIZE: i32 = 512;



// ======================
// === Texture Layout ===
// ======================

// === Texture Size ===

thread_local! {
    static TEXTURE_SIZE: Cell<Vector2<i32>> = default();
}

/// Read the texture size.
///
/// The texture size is initialized as a part of the [`World`](crate::display::world::World)
/// initialization. See [`arrange_shapes_on_texture`] for texture initialization details.
pub fn texture_size() -> Vector2<i32> {
    TEXTURE_SIZE.get()
}


// === initialize_cached_shape_positions_in_texture ===

/// Initialize texture size and cached shape positions.
///
/// Is done as part of the [`World`](crate::display::world::World) initialization. As a result,
/// every shape's [`get_position_in_texture`](CachedShape::get_position_in_texture) and
/// [`location_in_texture`](CachedShape::location_in_texture), as well as [`texture_size`] will
/// return a proper result.
///
/// See [`arrange_shapes_on_texture`] for packing algorithm details.
pub fn initialize_cached_shape_positions_in_texture() {
    CACHED_SHAPES_DEFINITIONS.with_borrow(|shape_defs| {
        let with_sizes = shape_defs.iter().map(|shape| {
            let size = shape.size;
            ShapeWithSize { shape, size }
        });
        let arranged = arrange_shapes_on_texture(with_sizes, INITIAL_TEXTURE_SIZE);
        TEXTURE_SIZE.set(arranged.texture_size);
        for ShapeWithPosition { shape, position } in arranged.shape_positions {
            (shape.position_on_texture_setter)(position)
        }
    })
}



// ===================
// === CachedShape ===
// ===================

/// A cached shape system definition. You do not need to implement it manually, use the
/// [`cached_shape!`] macro instead.
pub trait CachedShape: shape::system::Shape {
    /// The declared width of the cached shape.
    const WIDTH: f32;
    /// The height of the cached shape in texture.
    const HEIGHT: f32;
    /// The width of the space taken by the cached shape on the texture.
    const TEX_WIDTH: f32 = Self::WIDTH + CACHED_TEXTURE_MAX_DISTANCE * 2.0;
    /// The height of the space taken by the cached shape on the texture.
    const TEX_HEIGHT: f32 = Self::HEIGHT + CACHED_TEXTURE_MAX_DISTANCE * 2.0;

    /// Set the position of the shape in the texture.
    ///
    /// Setting the proper position is done as a part of
    /// [texture initialization](initialize_cached_shape_positions_in_texture).
    fn set_position_in_texture(position: Vector2);

    /// Read the position of the shape in the texture.
    fn get_position_in_texture() -> Vector2;

    /// Create view which will have position and all other properties set, so it will be ready to
    /// being render to cached shapes texture.
    fn create_view_for_texture() -> ShapeView<Self>
    where Self::ShapeData: Default {
        let shape = ShapeView::<Self>::new();
        shape.set_size(Vector2(Self::TEX_WIDTH, Self::TEX_HEIGHT));
        shape.set_xy(Self::get_position_in_texture());
        shape
    }

    /// Return the bounding box of the shape in the texture.
    fn location_in_texture() -> BoundingBox {
        let position = Self::get_position_in_texture();
        let left = position.x - Self::TEX_WIDTH / 2.0;
        let right = position.x + Self::TEX_WIDTH / 2.0;
        let bottom = position.y - Self::TEX_HEIGHT / 2.0;
        let top = position.y + Self::TEX_HEIGHT / 2.0;
        BoundingBox::from_corners(Vector2(left, bottom), Vector2(right, top))
    }

    /// Return the parameter for [`AnyCachedShape`] pointing to this specific shape.
    fn any_cached_shape_parameter() -> Vector4 {
        let bbox = Self::location_in_texture();
        Vector4(bbox.left(), bbox.bottom(), bbox.right(), bbox.top())
    }
}



// ======================
// === AnyCachedShape ===
// ======================

/// The mutable part of [`AnyCachedShape`].
pub mod mutable {
    use super::*;

    /// The [`AnyCachedShape`] definition.
    #[derive(Debug)]
    pub struct AnyCachedShape {
        /// A bounding box locating the cached shape on the texture. `xy` is a left-bottom corner,
        /// and `zw` is a top-right corner.
        pub tex_location: Var<Vector4>,
    }
}

/// One of the cached shapes represented as SDF shape.
///
/// This structure has the same API as other SDF shapes like [`Rect`](shape::primitive::def::Rect)
/// or [`Circle`](shape::primitive::def::Circle). Internally it reads from `pass_cached_shapes`
/// texture the SDF and proper color.
///
/// # Limitations
///
/// As the SDF and colors are aliased in the texture, the displayed AnyShape may be not as good
/// as rendering the shape directly. To have the ideal results, the shape cannot be scaled, zoomed
/// in or out, and it should be aligned with pixels. Otherwise the edges of the shape and different
/// color joints will be blurred.
///
/// # As Shape Parameter
///
/// [`AnyCachedShape`] may be a valid parameter for a different shape defined with [`shape!`] macro.
/// You can then set the concrete shape using [`CachedShape::any_cached_shape_parameter`] as in the
/// following example:
///
///
/// The parameter is passed to glsl as a single `vec4` describing the location of the cached shape
/// in the texture.
pub type AnyCachedShape = ShapeRef<mutable::AnyCachedShape>;

/// Create [`AnyCachedShape`] instance displaying given shape
#[allow(non_snake_case)]
pub fn AnyCachedShape<T: CachedShape>() -> AnyCachedShape {
    ShapeRef::new(mutable::AnyCachedShape { tex_location: T::any_cached_shape_parameter().into() })
}

impl AsOwned for AnyCachedShape {
    type Owned = AnyCachedShape;
}

impl canvas::Draw for AnyCachedShape {
    fn draw(&self, canvas: &mut Canvas) -> canvas::Shape {
        canvas.if_not_defined(self.id(), |canvas| {
            canvas.new_shape_from_expr(&format!(
                "return cached_shape(Id({}), position, {});",
                self.id(),
                self.tex_location.glsl()
            ))
        })
    }
}

impl From<AnyCachedShape> for AnyShape {
    fn from(value: AnyCachedShape) -> Self {
        Self::new(value)
    }
}

impl From<&AnyCachedShape> for AnyShape {
    fn from(value: &AnyCachedShape) -> Self {
        Self::new(value.clone())
    }
}

impl Parameter for AnyCachedShape {
    type GpuType = Vector4;
    type Variable = AnyCachedShape;

    fn create_var(name: &str) -> Self::Variable {
        ShapeRef::new(mutable::AnyCachedShape { tex_location: name.into() })
    }
}



// =============================
// === `cached_shape!` Macro ===
// =============================


/// Defines a new cached shape system.
///
/// The outcome is the same as for [`shape!`] macro, but quickly after application start the shape
/// will be rendered to the special "cached shapes" texture. The cached shapes may be then used
/// efficiently with [`AnyCachedShape`] structure. See [the module documentation](self) for the
/// usage overview and examples.
///
/// Because shape, once cached, is not redrawn, we don't allow for any parameterization except
/// styles.
#[macro_export]
macro_rules! cached_shape {
    (
        $width:literal x $height:literal;
        $(type SystemData = $system_data:ident;)?
        $(type ShapeData = $shape_data:ident;)?
        $(flavor = $flavor:path;)?
        $(above = [$($always_above_1:tt $(::$always_above_2:tt)*),*];)?
        $(below = [$($always_below_1:tt $(::$always_below_2:tt)*),*];)?
        $(pointer_events = $pointer_events:tt;)?
        ($style:ident : Style) {$($body:tt)*}
    ) => {
        $crate::_shape! {
            $(SystemData($system_data))?
            $(ShapeData($shape_data))?
            $(flavor = [$flavor];)?
            $(above = [$($always_above_1 $(::$always_above_2)*),*];)?
            $(below = [$($always_below_1 $(::$always_below_2)*),*];)?
            $(pointer_events = $pointer_events;)?
            [$style] (){$($body)*}
        }

        pub mod cached_shape_system_definition {
            use $crate::prelude::*;
            use super::shape_system_definition::Shape;
            use $crate::display::shape::primitive::system::cached::CachedShape;

            thread_local! {
                static POSITION_IN_TEXTURE: Cell<Vector2> = default();
            }

            impl CachedShape for Shape {
                const WIDTH: f32 = $width as f32;
                const HEIGHT: f32 = $height as f32;

                fn set_position_in_texture(position: Vector2) {
                    POSITION_IN_TEXTURE.with(|pos| pos.set(position))
                }
                fn get_position_in_texture() -> Vector2 {
                    POSITION_IN_TEXTURE.with(|pos| pos.get())
                }
            }

            #[before_main]
            pub fn register_cached_shape_entry_point() {
                register_cached_shape()
            }

            pub fn register_cached_shape() {
                $crate::display::world::CACHED_SHAPES_DEFINITIONS.with(|shapes| {
                    let mut shapes = shapes.borrow_mut();
                    shapes.push($crate::display::world::CachedShapeDefinition {
                        for_texture_constructor: Box::new(|| Box::new(Shape::create_view_for_texture())),
                        position_on_texture_setter: Box::new(Shape::set_position_in_texture),
                        size: Vector2(Shape::TEX_WIDTH, Shape::TEX_HEIGHT),
                    });
                });
            }
        }
    };
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display::object::layout;
    use crate::display::world::World;

    mod shape1 {
        use super::*;
        cached_shape! { 240 x 240;
            (_style: Style) {
                Plane().into()
            }
        }
    }

    mod shape2 {
        use super::*;
        cached_shape! { 240 x 112;
            (_style: Style) {
                Plane().into()
            }
        }
    }

    mod shape3 {
        use super::*;
        cached_shape! { 112 x 114;
            (_style: Style) {
                Plane().into()
            }
        }
    }

    #[test]
    fn cached_shapes_initialization_and_texture_location() {
        shape1::cached_shape_system_definition::register_cached_shape();
        shape2::cached_shape_system_definition::register_cached_shape();
        shape3::cached_shape_system_definition::register_cached_shape();
        let _world = World::new();

        // Those sizes includes margins for sdf. The margins are of [`CACHED_TEXTURE_MAX_DISTANCE`]
        // size.
        let tex_width = 512.0;
        let tex_height = 258.0;
        assert_eq!(texture_size(), Vector2(tex_width as i32, tex_height as i32));

        assert_eq!(shape1::Shape::get_position_in_texture(), Vector2(-128.0, -1.0));
        assert_eq!(shape2::Shape::get_position_in_texture(), Vector2(128.0, -65.0));
        assert_eq!(shape3::Shape::get_position_in_texture(), Vector2(64.0, 64.0));

        assert_eq!(shape1::Shape::location_in_texture(), ((-256.0, -129.0), (0.0, 127.0)).into());
        assert_eq!(shape2::Shape::location_in_texture(), ((0.0, -129.0), (256.0, -1.0)).into());
        assert_eq!(shape3::Shape::location_in_texture(), ((0.0, -1.0), (128.0, 129.0)).into());

        let view = shape1::Shape::create_view_for_texture();
        assert_eq!(view.xy(), Vector2(-128.0, -1.0));
        assert_eq!(
            view.size(),
            Vector2(
                layout::Size::Fixed(layout::Unit::Pixels(256.0)),
                layout::Size::Fixed(layout::Unit::Pixels(256.0))
            )
        );
    }
}
