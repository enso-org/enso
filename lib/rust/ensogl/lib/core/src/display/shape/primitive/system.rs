//! This module defines a "shape system". It is a wrapper over a "sprite system" and it defines
//! the required default material parameters.

use crate::prelude::*;

use super::def;

use crate::display;
use crate::display::scene::Scene;
use crate::display::shape::primitive::shader;
use crate::display::symbol::geometry::compound::sprite;
use crate::display::symbol::geometry::Sprite;
use crate::display::symbol::geometry::SpriteSystem;
use crate::display::symbol::material;
use crate::display::symbol::material::Material;
use crate::system::gpu::data::attribute;
use crate::system::gpu::data::buffer::item::Storable;
use crate::system::gpu::types::*;



// =====================
// === ShapeSystemId ===
// =====================

newtype_prim_no_default_no_display! {
    /// The ID of a user generated shape system.
    ShapeSystemId(std::any::TypeId);
}



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
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct ShapeSystem {
    #[shrinkwrap(main_field)]
    pub sprite_system:  SpriteSystem,
    pub shape:          Rc<RefCell<def::AnyShape>>,
    pub material:       Rc<RefCell<Material>>,
    pub pointer_events: Rc<Cell<bool>>,
}

impl ShapeSystem {
    /// Constructor.
    pub fn new<'t, S, Sh>(scene: S, shape: Sh) -> Self
    where
        S: Into<&'t Scene>,
        Sh: Into<def::AnyShape>, {
        let shape = shape.into();
        let sprite_system = SpriteSystem::new(scene);
        let material = Rc::new(RefCell::new(Self::surface_material()));
        let pointer_events = Rc::new(Cell::new(true));
        let shape = Rc::new(RefCell::new(shape));
        let this = Self { sprite_system, shape, material, pointer_events };
        this.reload_shape();
        this
    }

    // TODO
    // We should handle these attributes in a nicer way. Currently, they are hardcoded here and we
    // use magic to access them in shader builders.
    /// Defines a default material of this system.
    fn surface_material() -> Material {
        let mut material = Material::new();
        material.add_input("pixel_ratio", 1.0);
        material.add_input("z_zoom_1", 1.0);
        material.add_input("time", 0.0);
        material.add_input("symbol_id", 0);
        material.add_input("display_mode", 0);
        material.add_output("id", Vector4::<f32>::zero());
        material
    }

    /// Enables or disables pointer events on this shape system. All shapes of a shape system which
    /// has pointer events disabled would be completely transparent for the mouse (they would pass
    /// through all mouse events).
    pub fn set_pointer_events(&self, val: bool) {
        self.pointer_events.set(val);
        self.reload_shape();
    }

    /// Replaces the shape definition.
    pub fn set_shape<S: Into<def::AnyShape>>(&self, shape: S) {
        let shape = shape.into();
        *self.shape.borrow_mut() = shape;
        self.reload_shape();
    }

    /// Generates the shape again. It is used after some parameters are changed, like setting new
    /// `pointer_events` value.
    fn reload_shape(&self) {
        let code = shader::builder::Builder::run(&*self.shape.borrow(), self.pointer_events.get());
        self.material.borrow_mut().set_code(code);
        self.reload_material();
    }

    /// Define a new shader input.
    pub fn add_input<T: material::Input + Storable>(&self, name: &str, t: T) -> Buffer<T>
    where AnyBuffer: From<Buffer<T>> {
        self.material.borrow_mut().add_input(name, t);
        let buffer = self.sprite_system.symbol().surface().instance_scope().add_buffer(name);
        self.reload_material();
        buffer
    }

    /// Regenerate the shader with the current material.
    fn reload_material(&self) {
        self.sprite_system.set_material(&*self.material.borrow());
    }
}

impl display::Object for ShapeSystem {
    fn display_object(&self) -> &display::object::Instance {
        self.sprite_system.display_object()
    }
}



// ===========================
// === ShapeSystemInstance ===
// ===========================

/// Trait for user defined shape systems. The easiest way to define custom shape system is by using
/// the `define_shape_system` macro.
pub trait ShapeSystemInstance: 'static + CloneRef {
    /// The ID of the shape system.
    fn id() -> ShapeSystemId;
    /// Constructor.
    fn new(scene: &Scene) -> Self;
    /// The [`ShapeSystem`] instance of the user defined shape system.
    fn shape_system(&self) -> &ShapeSystem;
    /// List of shape systems this shape system should always be drawn on above of. See the
    /// [`crate::display::scene::Layers`] documentation to learn more about compile time shapes
    /// ordering relations.
    fn above() -> Vec<ShapeSystemId>;
    /// List of shape system this shape system should always be drawn on below of. See the
    /// [`crate::display::scene::Layers`] documentation to learn more about compile time shapes
    /// ordering relations.
    fn below() -> Vec<ShapeSystemId>;
}

/// Trait for static (scene-instantiated) shape systems. Read docs of [`ShapeSystemInstance`], and
/// [`DynShapeSystemInstance`] to learn more.
pub trait StaticShapeSystemInstance: ShapeSystemInstance {
    /// The shape type of this shape system definition.
    type Shape: Shape<System = Self>;
    /// New shape constructor.
    fn new_instance(&self) -> Self::Shape;
}

/// Trait for dynamic (possibly scene-non-instantiated) shape systems. Read docs of
/// [`ShapeSystemInstance`], and [`StaticShapeSystemInstance`] to learn more.
pub trait DynShapeSystemInstance: ShapeSystemInstance {
    /// The dynamic shape type of this shape system definition.
    type DynamicShape: DynamicShape<System = Self>;
    /// New shape instantiation. Used to bind a shape to a specific scene implementation.
    fn instantiate(&self, shape: &Self::DynamicShape) -> attribute::InstanceIndex;
}

/// Abstraction for every entity which is associated with a shape system (user generated one). For
/// example, all defined shapes are associated with a shape system, and thus they implement this
/// trait.
pub trait KnownShapeSystemId {
    /// The ID of a user defined shape system.
    fn shape_system_id() -> ShapeSystemId;
}



// =============
// === Shape ===
// =============

/// Type for every shape bound to a specific scene and GPU buffers. The easiest way to define such a
/// shape is by using the `define_shape_system` macro.
pub trait Shape: display::Object + CloneRef + Debug + Sized {
    /// The shape system instance this shape belongs to.
    type System: StaticShapeSystemInstance<Shape = Self>;
    /// Accessor for the underlying sprite.
    fn sprite(&self) -> &Sprite;
    /// Check if given mouse-event-target means this shape.
    fn is_this_target(&self, target: display::scene::PointerTarget) -> bool {
        self.sprite().is_this_target(target)
    }
}

/// Type for every shape which can, but does not have to be bound to a specific scene and GPU
/// buffers. Dynamic shapes can be created freely and will be bound to a scene after being attached
/// as scene children and an update frame event will be emitted.
///
/// Dynamic shapes contain copy of all shape parameters and use them to set up the GPU parameters
/// on bound.
///
/// The easiest way to define such a shape is by using the `define_shape_system` macro.
pub trait DynamicShape: display::Object + CloneRef + Debug + Sized {
    /// The static version of the shape. Dynamic shapes can be associated with one or more static
    /// shapes after they are placed on the stage and initialized.
    type StaticShape: Shape;
    /// The shape system instance this shape belongs to.
    type System: DynShapeSystemInstance<DynamicShape = Self>;
    /// Constructor.
    fn new(logger: impl AnyLogger) -> Self;
    /// Accessor for the underlying sprite, if the shape is initialized.
    fn sprites(&self) -> Vec<Sprite>;
    /// The "canvas" size of the shape. It defines the bounding-box for the shape drawing area.
    fn size(&self) -> &DynamicParam<sprite::Size>;
    /// Check if given pointer-event-target means this object.
    fn is_this_target(&self, target: display::scene::PointerTarget) -> bool {
        self.sprites().into_iter().any(|sprite| sprite.is_this_target(target))
    }
}

/// Internal utilities for managing [`DynamicShape`]s. You would normally not need to ever use it
/// explicitly, however, it is exposed as public interface as it is required for user-defined shape
/// systems. Again, instead of implementing shape systems from scratch, you'd rather use the
/// `define_shape_system!` macro.
pub trait DynamicShapeInternals: DynamicShape {
    /// Add a new [`Shape`] instance to this dynamic shape. Please note that dynamic shapes can be
    /// attached with multiple [`Shape`]s at the same time if they are placed on multiple scene
    /// [`Layer`]s. Then, each layer has a separate shape system, and thus, a separate [`Shape`] to
    /// represent this [`DynamicShape`].
    fn add_instance(&self, shape: Self::StaticShape);

    /// Drop all [`Shape`] instanced assigned to this dynamic shape with the `add_instance` method.
    fn drop_instances(&self);
}


// === Type Families ===

/// Accessor for the `Shape::System` associated type.
pub type ShapeSystemOf<T> = <T as Shape>::System;

/// Accessor for the `Shape::System` associated type.
pub type DynShapeSystemOf<T> = <T as DynamicShape>::System;



// ====================
// === DynamicParam ===
// ====================

/// A dynamic version of shape parameter. In case the shape was initialized and bound to the
/// GPU, the `attribute` will be initialized as well and will point to the right buffer
/// section. Otherwise, changing the parameter will not have any visual effect, however,
/// all the changes will be recorded and applied as soon as the shape will get initialized.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = "T::Item:Default"))]
#[derivative(Debug(bound = "T::Item:Copy+Debug, T:Debug"))]
#[allow(missing_docs)]
pub struct DynamicParam<T: HasItem> {
    cache:      Rc<Cell<T::Item>>,
    attributes: Rc<RefCell<Vec<T>>>,
}

impl<T> DynamicParam<T>
where
    T: CellProperty,
    T::Item: Copy,
{
    /// Set the parameter value.
    pub fn set(&self, value: T::Item) {
        self.cache.set(value);
        for attribute in &*self.attributes.borrow() {
            attribute.set(value)
        }
    }

    /// Get the parameter value.
    pub fn get(&self) -> T::Item {
        self.cache.get()
    }
}

/// Internal utilities for managing [`DynamicParam`]s. You would normally not need to ever use it
/// explicitly, however, it is exposed as public interface as it is required for user-defined shape
/// systems. Again, instead of implementing shape systems from scratch, you'd rather use the
/// `define_shape_system!` macro.
pub trait DynamicParamInternals<T> {
    /// Add a new binding to an attribute. This is done for every [`DynamicParam`] after the
    /// [`DynamicShape`] gets initialized with a new [`Shape`].
    fn add_attribute_binding(&self, attribute: T);
    /// Remove all attribute bindings. This is used, for example, when the [`DynamicShape`] gets
    /// removed from the display hierarchy.
    fn remove_attributes_bindings(&self);
}

impl<T> DynamicParamInternals<T> for DynamicParam<T>
where
    T: CellProperty,
    T::Item: Copy,
{
    fn remove_attributes_bindings(&self) {
        *self.attributes.borrow_mut() = default();
    }

    fn add_attribute_binding(&self, attribute: T) {
        attribute.set(self.cache.get());
        self.attributes.borrow_mut().push(attribute);
    }
}



// ==============
// === Macros ===
// ==============

/// Defines 'Shape' and 'ShapeSystem' structures. The generated Shape is a newtype for `Sprite`
/// and the shader attributes. The generated 'ShapeSystem' is a newtype for the `ShapeSystem` and
/// the required buffer handlers.
#[macro_export]
macro_rules! define_shape_system {
    (
        $(above = [$($always_above_1:tt $(::$always_above_2:tt)*),*];)?
        $(below = [$($always_below_1:tt $(::$always_below_2:tt)*),*];)?
        ($style:ident : Style $(,$gpu_param : ident : $gpu_param_type : ty)* $(,)?) {$($body:tt)*}
    ) => {
        $crate::_define_shape_system! {
            $(above = [$($always_above_1 $(::$always_above_2)*),*];)?
            $(below = [$($always_below_1 $(::$always_below_2)*),*];)?
            [$style] ($($gpu_param : $gpu_param_type),*){$($body)*}
        }
    };

    (
        $(above = [$($always_above_1:tt $(::$always_above_2:tt)*),*];)?
        $(below = [$($always_below_1:tt $(::$always_below_2:tt)*),*];)?
        ($($gpu_param : ident : $gpu_param_type : ty),* $(,)?) {$($body:tt)*}
    ) => {
        $crate::_define_shape_system! {
            $(above = [$($always_above_1 $(::$always_above_2)*),*];)?
            $(below = [$($always_below_1 $(::$always_below_2)*),*];)?
            [style] ($($gpu_param : $gpu_param_type),*){$($body)*}
        }
    }
}

/// Internal helper for `define_shape_system`.
#[macro_export]
macro_rules! _define_shape_system {
    (
        $(above = [$($always_above_1:tt $(::$always_above_2:tt)*),*];)?
        $(below = [$($always_below_1:tt $(::$always_below_2:tt)*),*];)?
        [$style:ident]
        ($($gpu_param : ident : $gpu_param_type : ty),* $(,)?)
        {$($body:tt)*}
    ) => {

        pub use shape_system_definition::Shape;
        pub use shape_system_definition::ShapeSystem;
        pub use shape_system_definition::DynamicShape;
        pub use shape_system_definition::DynamicShapeParams;
        pub use shape_system_definition::View;

        // FIXME: To be investigated why it's needed. We should not use shorter names, but it's not
        //        obvious why they appear in the scope here.
        #[allow(unused_qualifications)]
        mod shape_system_definition {
            use super::*;
            use $crate::display;
            use $crate::display::symbol::geometry::compound::sprite;
            use $crate::display::symbol::geometry::Sprite;
            use $crate::system::gpu;
            use $crate::system::gpu::data::Attribute;



            // =============
            // === Shape ===
            // =============

            /// An initialized, GPU-bound shape definition. All changed parameters are immediately
            /// reflected in the [`Buffer`] and will be synchronised with GPU before next frame is
            /// drawn.
            #[derive(Clone,CloneRef,Debug)]
            #[allow(missing_docs)]
            pub struct Shape {
                pub sprite : Sprite,
                $(pub $gpu_param : Attribute<$gpu_param_type>),*
            }

            impl Deref for Shape {
                type Target = Sprite;
                fn deref(&self) -> &Self::Target {
                    &self.sprite
                }
            }

            impl display::shape::system::Shape for Shape {
                type System = ShapeSystem;
                fn sprite(&self) -> &Sprite {
                    &self.sprite
                }
            }

            impl display::Object for Shape {
                fn display_object(&self) -> &display::object::Instance {
                    self.sprite.display_object()
                }
            }



            // ==========================
            // === DynamicShapeParams ===
            // ==========================

            /// Parameters of the [`DynamicShape`].
            #[derive(Clone,CloneRef,Debug,Default)]
            #[allow(missing_docs)]
            pub struct DynamicShapeParams {
                pub size:display::shape::system::DynamicParam<sprite::Size>,
                $(pub $gpu_param:display::shape::system::DynamicParam<Attribute<$gpu_param_type>>),*
            }



            // ====================
            // === DynamicShape ===
            // ====================

            /// A dynamic version of the [`Shape`]. In case the shape was initialized and bound to
            /// the GPU, the parameters will be initialized as well and will point to the right
            /// buffers sections. Otherwise, changing a parameter will not have any visual effect,
            /// however, all the changes will be recorded and applied as soon as the shape will get
            /// initialized.
            #[derive(Clone,CloneRef,Debug)]
            #[allow(missing_docs)]
            pub struct DynamicShape {
                display_object : display::object::Instance,
                shapes         : Rc<RefCell<Vec<Shape>>>,
                params         : DynamicShapeParams,
            }

            impl Deref for DynamicShape {
                type Target = DynamicShapeParams;
                fn deref(&self) -> &Self::Target {
                    &self.params
                }
            }

            impl display::shape::system::DynamicShape for DynamicShape {
                type StaticShape = Shape;
                type System      = ShapeSystem;

                fn new(logger:impl AnyLogger) -> Self {
                    let logger : Logger = Logger::new_sub(&logger,"dyn_shape");
                    let display_object  = display::object::Instance::new(&logger);
                    let shapes          = default();
                    let params          = default();
                    Self {display_object,shapes,params}
                }

                fn sprites(&self) -> Vec<Sprite> {
                    self.shapes.borrow().iter().map(|t|t.sprite.clone_ref()).collect()
                }

                fn size(&self) -> &display::shape::system::DynamicParam<sprite::Size> {
                    &self.size
                }
            }

            impl display::shape::system::DynamicShapeInternals for DynamicShape {
                fn add_instance(&self, shape:Shape) {
                    self.display_object.add_child(&shape);
                    self.params.size.add_attribute_binding(shape.sprite.size.clone_ref());
                    $(
                        let gpu_param = shape.$gpu_param.clone_ref();
                        self.params.$gpu_param.add_attribute_binding(gpu_param);
                    )*
                    self.shapes.borrow_mut().push(shape);
                }

                fn drop_instances(&self) {
                    for shape in mem::take(&mut *self.shapes.borrow_mut()) {
                        self.display_object.remove_child(&shape);
                    }
                    self.params.size.remove_attributes_bindings();
                    $(self.params.$gpu_param.remove_attributes_bindings();)*
                }
            }

            impl display::Object for DynamicShape {
                fn display_object(&self) -> &display::object::Instance {
                    &self.display_object
                }
            }



            // ============
            // === View ===
            // ============

            /// A view of the defined shape. You can place the view in your objects and it will
            /// automatically initialize on-demand.
            pub type View = $crate::gui::component::ShapeView<DynamicShape>;

            impl display::shape::KnownShapeSystemId for DynamicShape {
                fn shape_system_id() -> display::shape::ShapeSystemId {
                    ShapeSystem::shape_system_id()
                }
            }



            // ===================
            // === ShapeSystem ===
            // ===================

            /// Shape system allowing the creation of new [`Shape`]s and instantiation of
            /// [`DynamicShape`]s.
            #[derive(Clone,CloneRef,Debug)]
            #[allow(missing_docs)]
            pub struct ShapeSystem {
                pub shape_system : display::shape::ShapeSystem,
                style_watch      : display::shape::StyleWatch,
                $(pub $gpu_param : gpu::data::Buffer<$gpu_param_type>),*
            }

            impl display::shape::ShapeSystemInstance for ShapeSystem {
                fn id() -> display::shape::ShapeSystemId {
                    std::any::TypeId::of::<ShapeSystem>().into()
                }

                fn new(scene:&display::scene::Scene) -> Self {
                    let style_watch  = display::shape::StyleWatch::new(&scene.style_sheet);
                    let shape_def    = Self::shape_def(&style_watch);
                    let shape_system = display::shape::ShapeSystem::new(scene,&shape_def);
                    $(
                        let name = stringify!($gpu_param);
                        let val  = gpu::data::default::gpu_default::<$gpu_param_type>();
                        let $gpu_param = shape_system.add_input(name,val);
                    )*
                    Self {shape_system,style_watch,$($gpu_param),*} . init_refresh_on_style_change()
                }

                fn shape_system(&self) -> &display::shape::ShapeSystem {
                    &self.shape_system
                }

                fn above() -> Vec<ShapeSystemId> {
                    vec![ $($($always_above_1 $(::$always_above_2)* :: ShapeSystem :: id()),*)? ]
                }
                fn below() -> Vec<ShapeSystemId> {
                    vec![ $($($always_below_1 $(::$always_below_2)* :: ShapeSystem :: id()),*)? ]
                }
            }

            impl display::shape::StaticShapeSystemInstance for ShapeSystem {
                type Shape = Shape;

                fn new_instance(&self) -> Self::Shape {
                    let sprite = self.shape_system.new_instance();
                    let id     = sprite.instance_id;
                    $(let $gpu_param = self.$gpu_param.at(id);)*
                    Shape {sprite, $($gpu_param),*}
                }
            }

            impl display::shape::DynShapeSystemInstance for ShapeSystem {
                type DynamicShape = DynamicShape;

                fn instantiate(&self, dyn_shape:&Self::DynamicShape)
                -> gpu::data::attribute::InstanceIndex {
                    let sprite = self.shape_system.new_instance();
                    let id     = sprite.instance_id;
                    $(let $gpu_param = self.$gpu_param.at(id);)*
                    let shape = Shape {sprite, $($gpu_param),*};
                    dyn_shape.add_instance(shape);
                    id
                }
            }

            impl display::shape::KnownShapeSystemId for ShapeSystem {
                fn shape_system_id() -> display::shape::ShapeSystemId {
                    std::any::TypeId::of::<ShapeSystem>().into()
                }
            }

            impl ShapeSystem {
                fn init_refresh_on_style_change(self) -> Self {
                    let shape_system = self.shape_system.clone_ref();
                    let style_watch  = self.style_watch.clone_ref();
                    self.style_watch.set_on_style_change(move || {
                        shape_system.set_shape(&Self::shape_def(&style_watch));
                    });
                    self
                }

                /// The canvas shape definition.
                pub fn shape_def
                (__style_watch__:&display::shape::StyleWatch)
                -> display::shape::primitive::def::AnyShape {
                    #[allow(unused_imports)]
                    use display::style::data::DataMatch; // Operations styles.

                    __style_watch__.reset();
                    let $style  = __style_watch__;
                    // Silencing warnings about not used style.
                    let _unused = &$style;
                    $(
                        let $gpu_param : display::shape::primitive::def::Var<$gpu_param_type> =
                            concat!("input_",stringify!($gpu_param)).into();
                        // Silencing warnings about not used shader input variables.
                        let _unused = &$gpu_param;
                    )*
                    $($body)*
                }
            }
        }
    };
}
