//! This module defines a "shape system". It is a wrapper over a "sprite system" and it defines
//! the required default material parameters.

use crate::prelude::*;

use super::def;

use crate::display;
use crate::display::shape::primitive::shader;
use crate::display::symbol::geometry::SpriteSystem;
use crate::display::symbol::geometry::Sprite;
use crate::display::symbol::material;
use crate::display::symbol::material::Material;
use crate::display::scene::Scene;
use crate::system::gpu::types::*;
use crate::display::object::traits::*;
use crate::system::gpu::data::buffer::item::Storable;



// ===================
// === ShapeSystem ===
// ===================

/// Definition of a shape management system.
///
/// Please note that you would rather not need to use it directly, as it would require manual
/// management of buffer handlers. In order to automate the management, there is
/// `ShapeSystemInstance` and the `define_shape_system` macro.
///
/// Under the hood, it is a specialized version of `SpriteSystem`.
#[allow(missing_docs)]
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct ShapeSystem {
    #[shrinkwrap(main_field)]
    pub sprite_system : SpriteSystem,
    pub material      : Rc<RefCell<Material>>,
}

impl ShapeSystem {
    /// Constructor.
    pub fn new<'t,S,Sh:def::Shape>(scene:S, shape:&Sh) -> Self
    where S : Into<&'t Scene> {
        let sprite_system = SpriteSystem::new(scene);
        let material      = Rc::new(RefCell::new(Self::surface_material()));
        let this          = Self {sprite_system,material};
        this.set_shape(shape);
        this
    }

    // TODO
    // We should handle these attributes in a nicer way. Currently, they are hardcoded here and we
    // use magic to access them in shader builders.
    /// Defines a default material of this system.
    fn surface_material() -> Material {
        let mut material = Material::new();
        material.add_input  ("pixel_ratio"  , 1.0);
        material.add_input  ("zoom"         , 1.0);
        material.add_input  ("time"         , 0.0);
        material.add_input  ("symbol_id"    , 0);
        material.add_input  ("display_mode" , 0);
        material.add_output ("id"           , Vector4::<u32>::new(0,0,0,0));
        material
    }

    /// Replaces the shape definition.
    pub fn set_shape<S:def::Shape>(&self, shape:&S) {
        let code = shader::builder::Builder::run(shape);
        self.material.borrow_mut().set_code(code);
        self.reload_material();
    }

    /// Define a new shader input.
    pub fn add_input<T:material::Input + Storable>(&self, name:&str, t:T) -> Buffer<T>
    where AnyBuffer: From<Buffer<T>> {
        self.material.borrow_mut().add_input(name,t);
        let buffer = self.sprite_system.symbol().surface().instance_scope().add_buffer(name);
        self.reload_material();
        buffer
    }

    /// Regenerate the shader with the current material.
    fn reload_material(&self) {
        self.sprite_system.set_material(&*self.material.borrow());
    }
}

impl<'t> From<&'t ShapeSystem> for &'t display::object::Node {
    fn from(shape_system:&'t ShapeSystem) -> Self {
        shape_system.sprite_system.display_object()
    }
}

/// Type for every `ShapeSystem` with automatic buffer management. The easiest way to define sych a
/// shape system instance is by using the `define_shape_system` macro.
pub trait ShapeSystemInstance : 'static + CloneRef {
    /// The shape type of this shape system definition.
    type Shape : Shape<System=Self>;
    /// Constructor.
    fn new(scene:&Scene) -> Self;
    /// New shape constructor.
    fn new_instance(&self) -> Self::Shape;
}

/// Type for every shape with automatic attribute management. The easiest way to define such a
/// shape is by using the `define_shape_system` macro.
pub trait Shape : display::Object + Debug + Sized {
    /// The shape system instance this shape belongs to.
    type System : ShapeSystemInstance<Shape=Self>;
    /// Accessor for the underlying sprite object.
    fn sprite(&self) -> &Sprite;
}

/// Accessor for the `Shape::System` associated type.
pub type ShapeSystemOf<T> = <T as Shape>::System;


/// Defines 'Shape' and 'ShapeSystem' structures. The generated Shape is a newtype for `Sprite`
/// and the shader attributes. The generated 'ShapeSystem' is a newtype for the `ShapeSystem` and
/// the required buffer handlers.
#[macro_export]
macro_rules! define_shape_system {
    (
        ($($gpu_param : ident : $gpu_param_type : ty),* $(,)?)
        {$($body:tt)*}
    ) => {

        // =============
        // === Shape ===
        // =============

        /// Shape definition.
        #[derive(Clone,Debug)]
        #[allow(missing_docs)]
        pub struct Shape {
            pub sprite : Sprite,
            $(pub $gpu_param : Attribute<$gpu_param_type>),*
        }

        impl $crate::display::shape::system::Shape for Shape {
            type System = ShapeSystem;
            fn sprite(&self) -> &Sprite {
                &self.sprite
            }
        }

        impl<'t> From<&'t Shape> for &'t display::object::Node {
            fn from(t:&'t Shape) -> Self {
                &t.sprite.display_object()
            }
        }


        // ==============
        // === System ===
        // ==============

        /// Shape system definition.
        #[derive(Clone,CloneRef,Debug)]
        #[allow(missing_docs)]
        pub struct ShapeSystem {
            pub shape_system : $crate::display::shape::ShapeSystem,
            $(pub $gpu_param : Buffer<$gpu_param_type>),*
        }

        impl $crate::display::shape::ShapeSystemInstance for ShapeSystem {
            type Shape = Shape;

            fn new(scene:&Scene) -> Self {
                let shape_system = $crate::display::shape::ShapeSystem::new(scene,&Self::shape_def());
                $(
                    let name       = stringify!($gpu_param);
                    let value      = $crate::system::gpu::data::default::gpu_default::<$gpu_param_type>();
                    let $gpu_param = shape_system.add_input(name,value);
                )*
                Self {shape_system,$($gpu_param),*}
            }

            fn new_instance(&self) -> Self::Shape {
                let sprite = self.shape_system.new_instance();
                let id     = sprite.instance_id;
                $(let $gpu_param = self.$gpu_param.at(id);)*
                Shape {sprite, $($gpu_param),*}
            }
        }

        impl ShapeSystem {
            /// The canvas shape definition.
            pub fn shape_def() -> AnyShape {
                $(
                    let $gpu_param  : Var<$gpu_param_type> =
                        concat!("input_",stringify!($gpu_param)).into();
                    // Silencing warnings about not used shader input variables.
                    let _unused = &$gpu_param;
                )*
                $($body)*
            }
        }
    };
}
