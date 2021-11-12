//! This module contains definitions of all shape modifiers, such as transformations, boolean
//! operations, growing / shrinking shapes, etc.

#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use crate::prelude::*;

use crate::data::color::*;
use crate::display::shape::primitive::def::class::AnyShape;
use crate::display::shape::primitive::def::class::ShapeRef;
use crate::display::shape::primitive::def::var::Var;
use crate::display::shape::primitive::shader::canvas;
use crate::display::shape::primitive::shader::canvas::Canvas;



// ========================================
// === Compound Shape Definition Macros ===
// ========================================

/// Defines modifier canvas shapes. A modifier canvas shape is a struct, which owns one or more
/// other canvas shapes and modifies them according to some rule. For example, the `Translate`
/// modifier shape is a translated version of its child.
///
/// It defines `canvas::Draw` method for each modifier definition which first draws all children to
/// the canvas and then runs a canvas method of the same name as the modifier. For example, for
/// `Union`, it first draws its two children and then calls `canvas.union` with results of the draw
/// commands.
macro_rules! define_modifiers {
    ( $($name:ident $lname:ident $shapes:tt $fields:tt)* ) => {
        /// Contains mutable shapes definitions.
        pub mod mutable {
            use super::*;
            $(_define_modifier_data! {$name $shapes $fields})*
        }

        /// Contains immutable shapes definitions.
        pub mod immutable {
            use super::*;
            $(_define_modifier! {$name $lname $shapes $fields})*
        }
    }
}

macro_rules! _define_modifier_data {
    ($name:ident ($($shape_field:ident),*$(,)?) ($($field:ident : $field_type:ty),*$(,)?)) => {

        /// Shape type definition.
        #[allow(missing_docs)]
        #[derive(Debug)]
        pub struct $name<$($shape_field),*> {
            $(pub $shape_field : $shape_field),*,
            $(pub $field       : Var<$field_type>),*
        }
        impl<$($shape_field),*> $name<$($shape_field),*> {
            /// Constructor.
            pub fn new<$($field:Into<Var<$field_type>>),*>
            ($($shape_field:$shape_field),*,$($field:$field),*) -> Self {
                $(let $field = $field.into();)*
                Self {$($shape_field),*,$($field),*}
            }
        }

        impl<$($shape_field),*> AsOwned for $name<$($shape_field),*> {
            type Owned = $name<$($shape_field),*>;
        }

    }
}

macro_rules! _define_modifier {
    ($name:ident $lname:ident ($($shape_field:ident),*$(,)?) ($($field:ident : $field_type:ty),*$(,)?)) => {
        /// Shape type definition.
        pub type $name<$($shape_field),*> =
            ShapeRef<mutable::$name<$($shape_field),*>>;

        /// Smart constructor.
        pub fn $name<$($shape_field:IntoOwned),*,$($field:Into<Var<$field_type>>),*>
        ( $($shape_field:$shape_field),*,$($field:$field),*) -> $name<$(Owned<$shape_field>),*> {
            ShapeRef::new(mutable::$name::new($($shape_field.into()),*,$($field),*))
        }

        impl<$($shape_field),*> AsOwned for $name<$($shape_field),*> {
            type Owned = $name<$($shape_field),*>;
        }

        impl<$($shape_field:'static+canvas::Draw),*> From<$name<$($shape_field),*>> for AnyShape {
            fn from(t:$name<$($shape_field),*>) -> Self {
                Self::new(t)
            }
        }

        impl<$($shape_field:'static+canvas::Draw),*> From<&$name<$($shape_field),*>> for AnyShape {
            fn from(t:&$name<$($shape_field),*>) -> Self {
                Self::new(t.clone())
            }
        }

        impl<$($shape_field:canvas::Draw),*> canvas::Draw for $name<$($shape_field),*> {
            fn draw(&self, canvas:&mut Canvas) -> canvas::Shape {
                $(let $shape_field = self.$shape_field.draw(canvas);)*
                canvas.$lname(self.id() $(,$shape_field)* $(,&self.$field)*)
            }
        }
    }
}



// =======================
// === Compound Shapes ===
// =======================

pub use immutable::*;
define_modifiers! {
    Translate    translate     (child)         (v:Vector2<Pixels>)
    Rotation     rotation      (child)         (angle:Radians)
    Scale        scale         (child)         (value:f32)
    Union        union         (child1,child2) ()
    Difference   difference    (child1,child2) ()
    Intersection intersection  (child1,child2) ()
    Fill         fill          (child)         (color:Rgba)
    PixelSnap    pixel_snap    (child)         ()
    Grow         grow          (child)         (value:f32)
    Shrink       shrink        (child)         (value:f32)
    Repeat       repeat        (child)         (tile_size:Vector2<Pixels>)
}
