//! This module defines all primitive Signed Distance Field (SDF) shapes such as circle or
//! rectangle. Learn more about SDFs: https://en.wikipedia.org/wiki/Signed_distance_function

// === Non-Standard Linter Configuration ===
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use crate::prelude::*;
use crate::system::gpu::shader::glsl::traits::*;

use crate::display::shape::primitive::def::class::AnyShape;
use crate::display::shape::primitive::def::class::ShapeRef;
use crate::display::shape::primitive::shader::canvas;
use crate::display::shape::primitive::shader::canvas::Canvas;
use crate::display::shape::Grow;
use crate::display::shape::Var;
use crate::system::gpu::shader::glsl::Glsl;

use inflector::Inflector;



// ===========================
// === GlslShapeDefinition ===
// ===========================

/// Class of primitive SDF shapes.
pub trait GlslShapeDefinition {
    /// Gets the SDF definition for the given shape.
    fn glsl_shape_definition() -> String;
}



// ====================================
// === Prim Shape Definition Macros ===
// ====================================

/// Defines SDF shapes and appropriate shape wrappers.
///
/// SDF shapes are immutable. They keep their internal state behind an `Rc` barrier by using the
/// `ShapeRef` newtype. Their internal state is generated into the `mutable` module. The shape
/// definition syntax accepted by this macro is similar to both a struct and a function definition.
///
/// The body of the shape definition should be a valid GLSL function code. The function will be
/// provided with two parameters:
///   - The current position point as `vec2 position`.
///   - All input parameters bound to this shader from the material definition as `Env env`.
///
/// The result of this shader should be a new `BoundSdf` instance. For more information about
/// the types and available helper functions in GLSL, please refer to the GLSL definitions in
/// `src/display/shape/primitive/def/glsl/*.glsl` files.
///
/// This macro will also generate a `all_shapes_glsl_definitions` function which returns a GLSL code
/// containing all shapes definitions in one place.

macro_rules! define_sdf_shapes {
    ( $( $(#$meta:tt)* $name:ident $args:tt $body:tt )* ) => {

        /// Contains mutable shapes definitions.
        pub mod mutable {
            use super::*;
            $(_define_sdf_shape_mutable_part! {$name $args $body} )*
        }
        $(_define_sdf_shape_immutable_part! {$(#$meta)* $name $args $body} )*

        /// GLSL definition of all shapes.
        pub fn all_shapes_glsl_definitions() -> String {
            vec![$($name::glsl_shape_definition()),*].join("\n\n")
        }
    };
}

/// See the docs of `define_sdf_shapes`.
macro_rules! _define_sdf_shape_immutable_part {
    ( $(#$meta:tt)* $name:ident ( $($field:ident : $field_type:ty),* $(,)? ) $body:tt ) => {

        /// Smart shape type.
        $(#$meta)*
        pub type $name = ShapeRef<mutable::$name>;

        /// Smart shape constructor.
        $(#$meta)*
        pub fn $name <$($field:Into<Var<$field_type>>),*> ( $($field : $field),* ) -> $name {
            ShapeRef::new(mutable::$name::new($($field),*))
        }

        impl canvas::Draw for $name {
            fn draw(&self, canvas:&mut Canvas) -> canvas::Shape {
                let args = vec!["position".to_string(), $(self.$field.glsl().into()),* ].join(",");
                let code = format!("{}({})",self.glsl_name,args);
                canvas.define_shape(self.id(),&code)
            }
        }

        impl GlslShapeDefinition for $name {
            fn glsl_shape_definition() -> String {
                let name = stringify!($name).to_snake_case();
                let body = stringify!($body);
                let args = vec!["vec2 position".to_string(), $(
                    format!("{} {}", <$field_type>::glsl_prim_type(), stringify!($field))
                ),*].join(", ");
                format!("BoundSdf {name} ({args}) {body}")
            }
        }

        impl AsOwned for $name { type Owned = $name; }

        impl From<$name> for AnyShape {
            fn from(t:$name) -> Self {
                Self::new(t)
            }
        }

        impl From<&$name> for AnyShape {
            fn from(t:&$name) -> Self {
                Self::new(t.clone())
            }
        }

        impl $name {$(
            /// Field accessor.
            pub fn $field(&self) -> &Var<$field_type> {
                &self.unwrap().$field
            }
        )*}
    }
}

/// See the docs of `define_sdf_shapes`.
macro_rules! _define_sdf_shape_mutable_part {
    ( $name:ident ( $($field:ident : $field_type:ty),* $(,)? ) { $($code:tt)* } ) => {

        /// The shape definition.
        #[allow(missing_docs)]
        #[derive(Debug,Clone)]
        pub struct $name {
            pub glsl_name : Glsl,
            $(pub $field  : Var<$field_type>),*
        }

        impl $name {
            /// Constructor.
            #[allow(clippy::new_without_default)]
            pub fn new <$($field:Into<Var<$field_type>>),*> ( $($field : $field),* ) -> Self {
                let glsl_name = stringify!($name).to_snake_case().into();
                $(let $field = $field.into();)*
                Self {glsl_name,$($field),*}
            }
        }
    };
}



// ===================
// === Prim Shapes ===
// ===================

define_sdf_shapes! {

    // === Empty ===

    EmptyShape () {
        return bound_sdf(FLOAT_MAX,bounding_box(0.0,0.0));
    }

    // === Infinite ===

    Plane () {
        return bound_sdf(FLOAT_MIN,bounding_box(0.0,0.0));
    }

    HalfPlane () {
        return bound_sdf(position.y, bounding_box(0.0,0.0));
    }

    /// Cuts the provided angle from a plane. The angle faces upwards, so the angle of PI is equal
    /// to the upper half-plane. Negative values and values over 2*PI will cause the shape to flip
    /// vertically. In case you want a more consistent behavior, use the slightly less efficient
    /// `PlaneAngle` instead.
    BottomHalfPlane () {
        return bound_sdf(-position.y, bounding_box(0.0,0.0));
    }

    /// Cuts the provided angle from a plane. The angle faces upwards, so the angle of PI is equal
    /// to the upper half-plane. Negative angle values are displayed the same was as positive ones.
    /// In case the angle is bigger than 2*PI, the overlapping part of the angle would be treated as
    /// angle subtraction, so 3*PI is upper half-plane, and angle of 4*PI gives the same result as
    /// the angle of 0. In case you do not need such behavior, you can use slightly more efficient
    /// `PlaneAngleFast` instead.
    PlaneAngle (angle:Radians) {
        float pi_2       = 2.0 * PI;
        float angle_norm = value(angle) / pi_2;
              angle_norm = 1.0 - abs(mod(angle_norm,2.0)-1.0);
        float angle_rad  = angle_norm * pi_2;
        float off        = angle_norm - 0.5; // Fixes artifacts with 0 and 360 degrees.
        float distance   = abs(position).x*cos(angle_rad/2.0) - position.y*sin(angle_rad/2.0) - off;
        return bound_sdf(distance,bounding_box(0.0,0.0));
    }

    PlaneAngleFast (angle:Radians) {
        float v_angle  = value(angle);
        float off      = 0.5; // Fixes artifacts with 0 degrees.
        float distance = abs(position).x*cos(v_angle/2.0) - position.y*sin(v_angle/2.0) + off;
        return bound_sdf(distance,bounding_box(0.0,0.0));
    }

    Line (width:f32) {
        return bound_sdf(abs(position.y) - width/2.0, bounding_box(0.0,width));
    }


    // === RoundedLineSegment ===

    /// A line segment from `start` to `end` with rounded endpoints.
    Segment (start:Vector2<Pixels>, end:Vector2<Pixels>, width:Pixels) {
        // The implementation of this shape was adapted from here:
        // https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm

        float half_width = width / 2.0;

        vec2 delta = end - start;
        // A value between 0.0 and 1.0 indicating the position of the point on the line segment that
        // is closest to `position`. 0.0 stands for the origin and 1.0 for `target`.
        float projection   = clamp(dot(position-start,delta)/dot(delta,delta),0.0,1.0);
        vec2 closest_point = start + projection * delta;

        float left         = min(start.x,end.x) - half_width;
        float right        = max(start.x,end.x) + half_width;
        float bottom       = min(start.y,end.y) - half_width;
        float top          = max(start.y,end.y) + half_width;
        BoundingBox bounds = bounding_box(left,right,bottom,top);

        float distance = length(position - closest_point) - half_width;
        return bound_sdf(distance,bounds);
    }


    // === Ellipse ===

    Circle (radius:Pixels) {
        return bound_sdf(length(position)-radius, bounding_box(radius,radius));
    }

    Ellipse (x_radius:f32, y_radius:f32) {
        // The implementation of this shape was adapted from here:
        // https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm

        vec2 ab  = vec2(x_radius,y_radius);
        position = abs(position);
        if (position.x > position.y) {
            position = position.yx;
            ab       = ab.yx;
        }
        float l  = ab.y * ab.y - ab.x * ab.x;
        float m  = ab.x * position.x / l;
        float m2 = m * m;
        float n  = ab.y * position.y / l;
        float n2 = n * n;
        float c  = (m2 + n2 - 1.0) / 3.0;
        float c3 = c * c * c;
        float q  = c3 + m2 * n2 * 2.0;
        float d  = c3 + m2 * n2;
        float g  = m + m * n2;
        float co;
        if (d < 0.0) {
            float h  = acos(q / c3) / 3.0;
            float s  = cos(h);
            float t  = sin(h) * sqrt(3.0);
            float rx = sqrt(-c * (s + t + 2.0) + m2);
            float ry = sqrt(-c * (s - t + 2.0) + m2);
            co = (ry + sign(l) * rx + abs(g) / (rx * ry) - m) / 2.0;
        } else {
            float h  = 2.0 * m * n * sqrt(d);
            float s  = sign(q + h) * pow(abs(q + h), 1.0 / 3.0);
            float u  = sign(q - h) * pow(abs(q - h), 1.0 / 3.0);
            float rx = -s - u - c * 4.0 + 2.0 * m2;
            float ry = (s - u) * sqrt(3.0);
            float rm = sqrt(rx * rx + ry * ry);
            co       = (ry / sqrt(rm - rx) + 2.0 * g / rm - m) / 2.0;
        }
        vec2 r             = ab * vec2(co, sqrt(1.0 - co * co));
        float dist         = length(r - position) * sign(position.y - r.y);
        BoundingBox bounds = bounding_box(2.0 * x_radius,2.0 * y_radius);
        return bound_sdf(dist,bounds);
    }


    // === Rectangle ===

    Rect (size:Vector2<Pixels>) {
        vec2  dir  = abs(position) - size/2.0;
        float dist = max(min(dir,0.0)) + length(max(dir,0.0));
        return bound_sdf(dist,bounding_box(size));
    }

    RoundedRectByCorner
    ( size        : Vector2<Pixels>
    , top_left    : Pixels
    , top_right   : Pixels
    , bottom_left : Pixels
    , bottom_right: Pixels ) {
        float top_weight    = clamp(size.x / (top_left    + top_right)   , 0.0, 1.0);
        float bottom_weight = clamp(size.x / (bottom_left + bottom_right), 0.0, 1.0);
        float left_weight   = clamp(size.y / (top_left    + bottom_left) , 0.0, 1.0);
        float right_weight  = clamp(size.y / (top_right   + bottom_right), 0.0, 1.0);

        float tl = min(top_weight    , left_weight)  * top_left;
        float tr = min(top_weight    , right_weight) * top_right;
        float bl = min(bottom_weight , left_weight)  * bottom_left;
        float br = min(bottom_weight , right_weight) * bottom_right;

        size /= 2.0;

        bool is_top_left     = position.x <  -size.x + tl && position.y >  size.y - tl;
        bool is_top_right    = position.x >   size.x - tr && position.y >  size.y - tr;
        bool is_bottom_left  = position.x <  -size.x + bl && position.y < -size.y + bl;
        bool is_bottom_right = position.x >   size.x - br && position.y < -size.y + br;

        float dist;
        if      (is_top_left)     {dist = length(position - vec2(-size.x + tl,  size.y - tl)) - tl;}
        else if (is_top_right)    {dist = length(position - vec2( size.x - tr,  size.y - tr)) - tr;}
        else if (is_bottom_left)  {dist = length(position - vec2(-size.x + bl, -size.y + bl)) - bl;}
        else if (is_bottom_right) {dist = length(position - vec2( size.x - br, -size.y + br)) - br;}
        else {
            vec2 dir = abs(position) - size;
            dist = min(max(dir.x,dir.y),0.0) + length(max(dir,0.0));
        }
        return bound_sdf(dist,bounding_box(size));
    }


    // === Triangle ===

    /// Isosceles Triangle pointing up.
    /// This is an exact SDF. The calculated distance is exact both inside and outside of shape's
    /// bounds. Growing it will result in a triangle with rounded corners.
    /// Adapted from https://iquilezles.org/articles/distfunctions2d/
    Triangle (width:f32, height:f32) {
        vec2 q = vec2(width * 0.5, height);
        vec2 p = vec2(abs(position.x), height * 0.5 - position.y);
        vec2 a = p - q * clamp(dot(p,q) / dot(q,q), 0.0, 1.0);
        vec2 b = p - q * vec2(clamp(p.x / q.x, 0.0, 1.0), 1.0);
        float s = -sign(q.y);
        vec2 d1 = vec2(dot(a,a), s * (p.x * q.y - p.y * q.x));
        vec2 d2 = vec2(dot(b,b), s * (p.y - q.y));
        vec2 d = min(d1, d2);
        float dist = -sqrt(d.x) * sign(d.y);
        return bound_sdf(dist,bounding_box(width * 0.5, height * 0.5));
    }


    // === Uneven Capsule ===

    /// An upright capsule shape with ends of independent width. In other words, the convex hull of
    /// two circles where the radius of both circles can be chosen independently. The origin is at
    /// the center of the lower circle.
    ///
    /// # Arguments
    /// * `radius_top`    - Radius of the top circle.
    /// * `radius_bottom` - Radius of the bottom circle.
    /// * `inner_height`  - Distance between the centers of the two circles.
    UnevenCapsule (radius_top:Pixels, radius_bottom:Pixels, inner_height:Pixels) {
        position.x = abs(position.x);
        float b    = (radius_bottom-radius_top) / inner_height;
        float a    = sqrt(1.0-b*b);
        float k    = dot(position,vec2(-b,a));
        float dist;
        if (k < 0.0) {
            dist = length(position) - radius_bottom;
        } else if (k > a * inner_height) {
            dist = length(position-vec2(0.0,inner_height)) - radius_top;
        } else {
            dist = dot(position,vec2(a,b)) - radius_bottom;
        }

        float max_radius   = max(radius_top,radius_bottom);
        float min_x        = -max_radius;
        float max_x        = max_radius;
        float min_y        = -radius_bottom;
        float max_y        = inner_height + radius_top;
        BoundingBox bounds = bounding_box(min_x,max_x,min_y,max_y);

        return bound_sdf(dist,bounds);
    }


    // === Five Star ===

    /// A five-pointed star.
    ///
    /// # Arguments
    /// * `radius` - Distance of the outer points to the center.
    /// * `ratio`  - Distance of the inner points to the center, relative to `radius`.
    FiveStar (radius:Pixels, ratio:f32) {
        // The implementation of this shape was adapted from here:
        // https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm

        const vec2 k1      = vec2(0.809016994375,-0.587785252292);
        const vec2 k2      = vec2(-k1.x,k1.y);
        position.x         = abs(position.x);
        position          -= 2.0 * max(dot(k1,position),0.0) * k1;
        position          -= 2.0 * max(dot(k2,position),0.0) * k2;
        position.x         = abs(position.x);
        position.y        -= radius;
        vec2 ba            = ratio * vec2(-k1.y,k1.x) - vec2(0,1);
        float h            = clamp(dot(position,ba)/dot(ba,ba),0.0,radius);
        float dist         = length(position-ba*h) * sign(position.y*ba.x-position.x*ba.y);
        BoundingBox bounds = bounding_box(2.0*radius,2.0*radius);
        return bound_sdf(dist,bounds);
    }


    // === Arc ===

    RoundedArc (radius:Pixels, angle:Radians, width:Pixels) {
        // The implementation of this shape was adapted from here:
        // https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm

        position.x = abs(position.x);
        vec2 scb   = vec2(sin(value(angle)/2.0),cos(value(angle)/2.0));
        float ra   = radius;
        float rb   = width / 2.0;
        float k    = (scb.y*position.x>scb.x*position.y) ? dot(position,scb) : length(position);
        float dist = sqrt(max(0.0,dot(position,position) + ra * ra - 2.0 * ra * k)) - rb;

        BoundingBox bounds = bounding_box(2.0*radius+width,2.0*radius+width);
        return bound_sdf(dist,bounds);
    }
}



// ==============================
// === Prim Shapes Operations ===
// ==============================

impl Plane {
    /// Cuts angle from the plane.
    pub fn cut_angle<T: Into<Var<Radians>>>(&self, t: T) -> PlaneAngle {
        PlaneAngle(t)
    }

    /// Cuts angle from the plane.
    pub fn cut_angle_fast<T: Into<Var<Radians>>>(&self, t: T) -> PlaneAngleFast {
        PlaneAngleFast(t)
    }
}

impl Rect {
    /// Sets the radius of all the corners.
    pub fn corners_radius<T>(&self, radius: T) -> Grow<Rect>
    where T: Into<Var<Pixels>> {
        let size = self.size();
        let min_size = Min::min(size.x(), size.y());
        let radius = Min::min(min_size * 0.5, radius.into());
        let offset = Var::<Vector2<Pixels>>::from(format!("vec2({} * 2.0)", radius.glsl()));
        Grow(Rect(size - offset), radius)
    }

    /// Sets the radiuses of each of the corners.
    pub fn corners_radiuses<T1, T2, T3, T4>(
        &self,
        top_left: T1,
        top_right: T2,
        bottom_left: T3,
        bottom_right: T4,
    ) -> RoundedRectByCorner
    where
        T1: Into<Var<Pixels>>,
        T2: Into<Var<Pixels>>,
        T3: Into<Var<Pixels>>,
        T4: Into<Var<Pixels>>,
    {
        RoundedRectByCorner(self.size(), top_left, top_right, bottom_left, bottom_right)
    }

    /// Sets the radiuses of the left corners.
    pub fn left_corners_radius<T>(&self, radius: T) -> RoundedRectByCorner
    where T: Into<Var<Pixels>> {
        let radius = radius.into();
        let top_left = radius.clone();
        let bottom_left = radius;
        let top_right = 0.pixels();
        let bottom_right = 0.pixels();
        RoundedRectByCorner(self.size(), top_left, top_right, bottom_left, bottom_right)
    }

    /// Sets the radiuses of the right corners.
    pub fn right_corners_radius<T>(&self, radius: T) -> RoundedRectByCorner
    where T: Into<Var<Pixels>> {
        let radius = radius.into();
        let top_left = 0.pixels();
        let bottom_left = 0.pixels();
        let top_right = radius.clone();
        let bottom_right = radius;
        RoundedRectByCorner(self.size(), top_left, top_right, bottom_left, bottom_right)
    }
}
