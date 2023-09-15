//! This module defines the class of all shapes.

use super::modifier::*;
use super::unit::*;
use crate::prelude::*;

use crate::data::color;
use crate::display::shape::primitive::def::var::Var;
use crate::display::shape::primitive::shader::canvas;
use crate::display::shape::primitive::shader::canvas::Canvas;



// =============
// === Shape ===
// =============

/// Type of any shape which we can display on the canvas.
pub trait Shape = 'static + canvas::Draw;

/// Generic 2d shape representation. You can convert any specific shape type to this type and use it
/// as a generic shape type.
#[derive(Debug, Clone, CloneRef)]
pub struct AnyShape {
    rc: Rc<dyn canvas::Draw>,
}

impl AsOwned for AnyShape {
    type Owned = AnyShape;
}

impl AnyShape {
    /// Constructor.
    pub fn new<T: Shape>(t: T) -> Self {
        Self { rc: Rc::new(t) }
    }
}

impl canvas::Draw for AnyShape {
    fn draw(&self, canvas: &mut Canvas) -> canvas::Shape {
        self.rc.draw(canvas)
    }
}



// ================
// === ShapeRef ===
// ================

/// Immutable reference to a shape. It is also used to get unique id for each shape.
#[derive(Debug, Derivative, Deref)]
#[derivative(Clone(bound = ""))]
pub struct ShapeRef<T> {
    rc: Rc<T>,
}

impl<T> From<&ShapeRef<T>> for ShapeRef<T> {
    fn from(t: &ShapeRef<T>) -> Self {
        t.clone()
    }
}

impl<T> ShapeRef<T> {
    /// Constructor.
    pub fn new(t: T) -> Self {
        Self { rc: Rc::new(t) }
    }

    /// Unwraps the shape and provides the raw reference to its content.
    pub fn unwrap(&self) -> &T {
        self.deref()
    }
}

impl<T> ShapeRef<T> {
    /// Each shape definition has to be assigned with an unique id in order for the painter to
    /// implement results cache. For example, we can create three shapes `s1`, `s2`, and `s3`. We
    /// want to define `s4 = s1 - s2`, `s5 = s1 - s3`, and `s6 = s4 + s5`. We need to discover that
    /// we use `s1` twice under the hood in order to optimize the GLSL.
    pub fn id(&self) -> usize {
        Rc::downgrade(&self.rc).as_ptr() as *const () as usize
    }
}



// ================
// === ShapeOps ===
// ================

impl<T> ShapeOps for ShapeRef<T> {}
impl ShapeOps for AnyShape {}

/// Methods implemented by every shape.
pub trait ShapeOps: Sized
where for<'t> &'t Self: IntoOwned<Owned = Self> {
    /// Translate the shape by a given offset.
    fn translate<V: Into<Var<Vector2<Pixels>>>>(&self, v: V) -> Translate<Self> {
        Translate(self, v)
    }

    /// Translate the shape along X-axis by a given offset.
    fn translate_x<X>(&self, x: X) -> Translate<Self>
    where (X, Var<Pixels>): Into<Var<Vector2<Pixels>>> {
        self.translate((x, 0.px()))
    }

    /// Translate the shape along Y-axis by a given offset.
    fn translate_y<Y>(&self, y: Y) -> Translate<Self>
    where (Var<Pixels>, Y): Into<Var<Vector2<Pixels>>> {
        self.translate((0.px(), y))
    }

    /// Rotate the shape by a given angle.
    fn rotate<A: Into<Var<Radians>>>(&self, angle: A) -> Rotation<Self> {
        Rotation(self, angle)
    }

    /// Flip the shape upside-down, mirroring it over the X axis.
    fn flip_y(&self) -> FlipY<Self> {
        FlipY(self)
    }

    /// Scales the shape by a given value.
    fn scale<S: Into<Var<f32>>>(&self, value: S) -> Scale<Self> {
        Scale(self, value)
    }

    /// Unify the shape with another one.
    ///
    /// Warning: This operator is an approximation - given correct exact input signed distance, it
    /// will return exact distance outside the shape, but underestimate the distance inside the
    /// shape. For more details, see https://iquilezles.org/articles/interiordistance/
    fn union<S: IntoOwned>(&self, that: S) -> Union<Self, Owned<S>> {
        Union(self, that)
    }

    /// Unify two shapes, blending their colors based on the foreground shape's SDF value. This
    /// means that even if these shapes overlap and the foreground is semi-transparent, it will
    /// blend with the background only in the anti-aliased areas.
    ///
    /// Warning: This operator is an approximation - given correct exact input signed distance, it
    /// will return exact distance outside the shape, but underestimate the distance inside the
    /// shape. For more details, see https://iquilezles.org/articles/interiordistance/
    fn union_exclusive<S: IntoOwned>(&self, that: S) -> UnionExclusive<Self, Owned<S>> {
        UnionExclusive(self, that)
    }

    /// Subtracts the argument from this shape.
    ///
    /// Warning: This operator is an approximation - given correct exact input signed distance, it
    /// will overestimate the distance to the resulting shape. For more details, see
    /// https://iquilezles.org/articles/interiordistance/
    fn difference<S: IntoOwned>(&self, that: S) -> Difference<Self, Owned<S>> {
        Difference(self, that)
    }

    /// Computes the intersection of the shapes.
    ///    
    /// Warning: This operator is an approximation - given correct exact input signed distance, it
    /// will underestimate the distance to the resulting shape. For more details, see
    /// https://iquilezles.org/articles/interiordistance/
    fn intersection<S: IntoOwned>(&self, that: S) -> Intersection<Self, Owned<S>> {
        Intersection(self, that)
    }

    /// Fill the shape with the provided color.
    #[must_use]
    fn fill<Color: Into<Var<color::Rgba>>>(&self, color: Color) -> Fill<Self> {
        Fill(self, color)
    }

    /// Change the shape color depending on RGB components.
    ///
    /// ### How The New Color Is Defined
    ///
    /// Assuming `s.color` is a previous shape premultiplied color (i.e. the alpha component is
    /// applied to each channel), a new color is defined as:
    /// `r * s.color.r + b * s.color.b + g * s.color.g`.
    ///
    /// ### Usage
    ///
    /// The main case for this function is coloring a complex shape serving as a
    /// template - the best example are [cached
    /// shapes](crate::display::shape::primitive::system::cached), which cannot be
    /// parameterized.
    ///
    /// When the only colors in that template are [full red](color::Rgba::red),
    /// [full green](color::Rgba::green), or [full blue](color::Rgba::blue), this method will
    /// replace the template colors with the specialized ones.
    ///
    /// A real-world example is an icon (cached in the texture) which should change color on mouse
    /// hover or click.
    fn recolorize<RColor, GColor, BColor>(
        &self,
        r: RColor,
        g: GColor,
        b: BColor,
    ) -> Recolorize<Self>
    where
        RColor: Into<Var<color::Rgba>>,
        GColor: Into<Var<color::Rgba>>,
        BColor: Into<Var<color::Rgba>>,
    {
        Recolorize(self, r, g, b)
    }

    /// Makes the borders of the shape crisp. Please note that it removes any form of anti-aliasing
    /// and can cause distortions especially with round surfaces.
    fn pixel_snap(&self) -> PixelSnap<Self> {
        PixelSnap(self)
    }

    /// Grows the shape by the given amount.
    fn grow<T: Into<Var<Pixels>>>(&self, value: T) -> Grow<Self> {
        Grow(self, value.into())
    }

    /// Shrinks the shape by the given amount.
    fn shrink<T: Into<Var<Pixels>>>(&self, value: T) -> Shrink<Self> {
        Shrink(self, value.into())
    }

    /// Create a stroke of given thickness around shape's boundary. The stroke is centered around
    /// the shape's 0 distance isoline. If you want to offset it, use `grow` or `shrink` operators
    /// before applying `stroke`.
    ///
    /// Also known as "annulus" or "onion" operator. See "Making shapes annular" section in
    /// https://iquilezles.org/articles/distfunctions2d/ for more details.
    ///
    /// Note: This operator is exact - given correct exact input signed distance, it will produce an
    /// exact signed distance field for the resulting shape. But it is particularly sensitive to
    /// non-exactness of the input signed distance field, both outside and inside the shape. Be
    /// careful when using it after applying `union`, `difference` or `intersection` operators. See
    /// following shadertoy for demonstration of potential artifacts:
    /// https://www.shadertoy.com/view/dslfzH
    fn stroke<T: Into<Var<Pixels>>>(&self, thickness: T) -> Stroke<Self> {
        Stroke(self, thickness.into())
    }

    /// Repeats the shape with the given tile size.
    fn repeat<T: Into<Var<Vector2<Pixels>>>>(&self, value: T) -> Repeat<Self> {
        Repeat(self, value)
    }
}

macro_rules! define_shape_operator {
    ($($op_trait:ident :: $op:ident => $shape_trait:ident :: $shape:ident)*) => {$(
        impl<T,S:IntoOwned> $op_trait<S> for &ShapeRef<T> {
            type Output = $shape_trait<ShapeRef<T>,Owned<S>>;
            fn $op(self, that:S) -> Self::Output {
                self.$shape(that)
            }
        }

        impl<T,S:IntoOwned> $op_trait<S> for ShapeRef<T> {
            type Output = $shape_trait<ShapeRef<T>,Owned<S>>;
            fn $op(self, that:S) -> Self::Output {
                self.$shape(that)
            }
        }

        impl<S:IntoOwned> $op_trait<S> for &AnyShape {
            type Output = $shape_trait<AnyShape,Owned<S>>;
            fn $op(self, that:S) -> Self::Output {
                self.$shape(that)
            }
        }

        impl<S:IntoOwned> $op_trait<S> for AnyShape {
            type Output = $shape_trait<AnyShape,Owned<S>>;
            fn $op(self, that:S) -> Self::Output {
                self.$shape(that)
            }
        }
    )*}
}

define_shape_operator! {
    Add :: add => Union        :: union
    Sub :: sub => Difference   :: difference
    Mul :: mul => Intersection :: intersection
}
