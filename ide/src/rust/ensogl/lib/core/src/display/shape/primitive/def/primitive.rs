//! This module defines all primitive Signed Distance Field (SDF) shapes such as circle or
//! rectangle. Learn more about SDFs: https://en.wikipedia.org/wiki/Signed_distance_function

#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use crate::prelude::*;

use inflector::Inflector;

use crate::display::shape::primitive::def::class::AnyShape;
use crate::display::shape::primitive::def::class::ShapeRef;
use crate::display::shape::primitive::shader::canvas::Canvas;
use crate::display::shape::primitive::shader::canvas;
use crate::display::shape::Var;
use crate::system::gpu::shader::glsl::Glsl;
use crate::system::gpu::shader::glsl::traits::*;



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
                iformat!("BoundSdf {name} ({args}) {body}")
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
        return bound_sdf(abs(position.y)-width, bounding_box(0.0,width));
    }


    // === Ellipse ===

    Circle (radius:Pixels) {
        return bound_sdf(length(position)-radius, bounding_box(radius,radius));
    }

    // For better performance, this implementation does not provide the exact distance to the shape
    // boundary. Instead, it behaves like a circle that is stretched along one axis and shortened
    // along the other.
    Ellipse (x_radius:f32, y_radius:f32) {
        float a2   = x_radius / y_radius;
        float b2   = y_radius / x_radius;
        float px2  = position.x * position.x;
        float py2  = position.y * position.y;
        float dist = sqrt(b2 * px2 + a2 * py2) - sqrt(x_radius * y_radius);
        return bound_sdf(dist, bounding_box(x_radius,y_radius));
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

    Triangle (width:f32, height:f32) {
        vec2  norm  = normalize(vec2(height,width/2.0));
        float pos_y = -position.y - height/2.0;
        float dist  = max(abs(position).x*norm.x + position.y*norm.y - height/2.0*norm.y, pos_y);
        return bound_sdf(dist,bounding_box(width,height/2.0));
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
}



// ==============================
// === Prim Shapes Operations ===
// ==============================

impl Plane {
    /// Cuts angle from the plane.
    pub fn cut_angle<T:Into<Var<Radians>>>(&self, t:T) -> PlaneAngle {
        PlaneAngle(t)
    }

    /// Cuts angle from the plane.
    pub fn cut_angle_fast<T:Into<Var<Radians>>>(&self, t:T) -> PlaneAngleFast {
        PlaneAngleFast(t)
    }
}

impl Rect {
    /// Sets the radius of all the corners.
    pub fn corners_radius<T>(&self, radius:T) -> RoundedRectByCorner
    where T : Into<Var<Pixels>> {
        let radius       = radius.into();
        let top_left     = radius.clone();
        let top_right    = radius.clone();
        let bottom_left  = radius.clone();
        let bottom_right = radius;
        RoundedRectByCorner(self.size(),top_left,top_right,bottom_left,bottom_right)
    }

    /// Sets the radiuses of each of the corners.
    pub fn corners_radiuses<T1,T2,T3,T4>
    (&self, top_left:T1, top_right:T2, bottom_left:T3, bottom_right:T4) -> RoundedRectByCorner
    where T1 : Into<Var<Pixels>> ,
          T2 : Into<Var<Pixels>> ,
          T3 : Into<Var<Pixels>> ,
          T4 : Into<Var<Pixels>> {
        RoundedRectByCorner(self.size(),top_left,top_right,bottom_left,bottom_right)
    }

    /// Sets the radiuses of the left corners.
    pub fn left_corners_radius<T>(&self, radius:T) -> RoundedRectByCorner
    where T : Into<Var<Pixels>> {
        let radius       = radius.into();
        let top_left     = radius.clone();
        let bottom_left  = radius;
        let top_right    = 0.pixels();
        let bottom_right = 0.pixels();
        RoundedRectByCorner(self.size(),top_left,top_right,bottom_left,bottom_right)
    }

    /// Sets the radiuses of the right corners.
    pub fn right_corners_radius<T>(&self, radius:T) -> RoundedRectByCorner
        where T : Into<Var<Pixels>> {
        let radius       = radius.into();
        let top_left     = 0.pixels();
        let bottom_left  = 0.pixels();
        let top_right    = radius.clone();
        let bottom_right = radius;
        RoundedRectByCorner(self.size(),top_left,top_right,bottom_left,bottom_right)
    }
}
