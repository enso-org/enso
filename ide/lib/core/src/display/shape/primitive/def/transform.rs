//! This module contains definitions of all primitive shapes transformations, like translation, or
//! rotation.

#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use crate::display::shape::primitive::def::class::Shape;
use crate::display::shape::primitive::def::class::ShapeRef;
use crate::display::shape::primitive::shader::canvas::Canvas;
use crate::display::shape::primitive::shader::canvas::CanvasShape;
use crate::display::shape::primitive::shader::data::ShaderData;



// ========================================
// === Compound Shape Definition Macros ===
// ========================================

/// Defines compound canvas shapes.
///
/// For the following input:
/// ```compile_fail
/// define_compound_shapes! {
///    Translate(child)(x:f32,y:f32)
/// }
/// ```
///
/// The macro generates:
/// ```compile_fail
/// pub mod mutable {
///     use super::*;
///
///     pub struct Translate<child> {
///         pub child : child,
///         pub x     : String,
///         pub y     : String,
///     }
///
///     impl<child:Shape> Translate<child> {
///         pub fn new<x:ShaderData<f32>,y:ShaderData<f32>>(child:&child,x:x,y:y) -> Self {
///             let child = child.clone();
///             let x     = x.to_glsl();
///             let y     = y.to_glsl();
///             Self {child,x,y}
///         }
///     }
/// }
///
/// pub mod immutable {
///     use super::*;
///
///     pub type Translate<child> = ShapeRef<mutable::Translate<child>>;
///     pub fn Translate<child:Shape,x:ShaderData<f32>,y:ShaderData<f32>>
///     (child:&child,x:x,y:y) -> Translate<child> {
///         ShapeRef::new(mutable::Translate::new(child,x,y))
///     }
/// }
/// ```

macro_rules! define_compound_shapes {
    ( $($name:ident $shapes:tt $fields:tt)* ) => {
        /// Contains mutable shapes definitions.
        pub mod mutable {
            use super::*;
            $(_define_compound_shape_data! {$name $shapes $fields})*
        }

        /// Contains immutable shapes definitions.
        pub mod immutable {
            use super::*;
            $(_define_compound_shape! {$name $shapes $fields})*
        }
    }
}

macro_rules! _define_compound_shape_data {
    ($name:ident ($($shape_field:ident),*$(,)?) ($($field:ident : $field_type:ty),*$(,)?)) => {

        /// Shape type definition.
        #[allow(missing_docs)]
        pub struct $name<$($shape_field),*> {
            $(pub $shape_field : $shape_field),*,
            $(pub $field       : String      ),*
        }

        impl<$($shape_field:Shape),*> $name<$($shape_field),*> {
            /// Constructor.
            pub fn new<$($field:ShaderData<$field_type>),*>
            ($($shape_field:&$shape_field),*,$($field:$field),*) -> Self {
                $(let $shape_field = $shape_field.clone();)*
                $(let $field       = $field.to_glsl();)*
                Self {$($shape_field),*,$($field),*}
            }
        }
    }
}

macro_rules! _define_compound_shape {
    ($name:ident ($($shape_field:ident),*$(,)?) ($($field:ident : $field_type:ty),*$(,)?)) => {
        /// Shape type definition.
        pub type $name<$($shape_field),*> =
            ShapeRef<mutable::$name<$($shape_field),*>>;

        /// Smart constructor.
        pub fn $name<$($shape_field:Shape),*,$($field:ShaderData<$field_type>),*>
        ( $($shape_field:&$shape_field),*,$($field:$field),*) -> $name<$($shape_field),*> {
            ShapeRef::new(mutable::$name::new($($shape_field),*,$($field),*))
        }
    }
}


// =======================
// === Compound Shapes ===
// =======================

use immutable::*;

define_compound_shapes! {
    Translate(child)(x:f32,y:f32)
    Union(child1,child2)()
}

impl<Child:Shape> Shape for Translate<Child> {
    fn draw(&self, canvas:&mut Canvas) -> CanvasShape {
        let s1 = self.child.draw(canvas);
        canvas.translate(self.id(),s1,&self.x,&self.y)
    }
}

impl<Child1:Shape,Child2:Shape> Shape for Union<Child1,Child2> {
    fn draw(&self, canvas:&mut Canvas) -> CanvasShape {
        let s1 = self.child1.draw(canvas);
        let s2 = self.child2.draw(canvas);
        canvas.union(self.id(),s1,s2)
    }
}
