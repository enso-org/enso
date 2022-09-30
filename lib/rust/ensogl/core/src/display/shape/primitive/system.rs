//! This module defines a "shape system". It is a wrapper over a "sprite system" and it defines
//! the required default material parameters.

use crate::prelude::*;
use crate::system::gpu::types::*;

use crate::display;
use crate::display::scene::Scene;
use crate::display::shape::primitive::shader;
use crate::display::symbol;
use crate::display::symbol::geometry::compound::sprite;
use crate::display::symbol::geometry::Sprite;
use crate::display::symbol::geometry::SpriteSystem;
use crate::display::symbol::material;
use crate::display::symbol::material::Material;
use crate::system::gpu::data::buffer::item::Storable;
use crate::system::gpu::data::InstanceIndex;

use super::def;



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
    /// Enables or disables pointer events on this shape system. All shapes of a shape system which
    /// has pointer events disabled would be completely transparent for the mouse (they would pass
    /// through all mouse events).
    pub pointer_events: Immutable<bool>,
}

impl ShapeSystem {
    /// Constructor.
    #[profile(Detail)]
    pub fn new<'t, S, Sh>(scene: S, shape: Sh, pointer_events: bool) -> Self
    where
        S: Into<&'t Scene>,
        Sh: Into<def::AnyShape>, {
        let shape = shape.into();
        let sprite_system = SpriteSystem::new(scene);
        let material = Rc::new(RefCell::new(Self::surface_material()));
        let pointer_events = Immutable(pointer_events);
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
        material.add_input("display_mode", 0);
        material.add_output("id", Vector4::<f32>::zero());
        material
    }

    /// Replaces the shape definition.
    #[profile(Detail)]
    pub fn set_shape<S: Into<def::AnyShape>>(&self, shape: S) {
        let shape = shape.into();
        *self.shape.borrow_mut() = shape;
        self.reload_shape();
    }

    /// Generates the shape again. It is used after some parameters are changed, like setting new
    /// `shape`.
    fn reload_shape(&self) {
        let code = shader::builder::Builder::run(&*self.shape.borrow(), *self.pointer_events);
        self.material.borrow_mut().set_code(code);
        self.reload_material();
    }

    /// Define a new shader input.
    #[profile(Debug)]
    pub fn add_input<T: material::Input + Storable>(&self, name: &str, t: T) -> Buffer<T>
    where AnyBuffer: From<Buffer<T>> {
        self.material.borrow_mut().add_input(name, t);
        let buffer = self.sprite_system.symbol().surface().instance_scope().add_buffer(name);
        self.reload_material();
        buffer
    }

    /// Regenerate the shader with the current material.
    #[profile(Detail)]
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

// /// Trait for user defined shape systems. The easiest way to define custom shape system is by
// using /// the `define_shape_system` macro.
// pub trait ShapeSystemInstance: 'static + CloneRef {
//     /// The ID of the shape system.
//     fn id() -> ShapeSystemId;
//     /// Constructor.
//     fn new(scene: &Scene) -> Self;
//     /// The [`ShapeSystem`] instance of the user defined shape system.
//     fn shape_system(&self) -> &ShapeSystem;
//     /// List of shape systems this shape system should always be drawn on above of. See the
//     /// [`crate::display::scene::Layers`] documentation to learn more about compile time shapes
//     /// ordering relations.
//     fn above() -> Vec<ShapeSystemId>;
//     /// List of shape system this shape system should always be drawn on below of. See the
//     /// [`crate::display::scene::Layers`] documentation to learn more about compile time shapes
//     /// ordering relations.
//     fn below() -> Vec<ShapeSystemId>;
// }

// /// Trait for static (scene-instantiated) shape systems. Read docs of [`ShapeSystemInstance`],
// and /// [`DynShapeSystemInstance`] to learn more.
// pub trait StaticShapeSystemInstance: ShapeSystemInstance {
//     // /// The shape type of this shape system definition.
//     // type Shape: ShapeDefinition<System = Self>;
//     /// New shape constructor.
//     fn new_instance(&self) -> Self::Shape;
// }
//
// /// Trait for dynamic (possibly scene-non-instantiated) shape systems. Read docs of
// /// [`ShapeSystemInstance`], and [`StaticShapeSystemInstance`] to learn more.
// pub trait DynShapeSystemInstance: ShapeSystemInstance {
//     // /// The dynamic shape type of this shape system definition.
//     // type DynamicShape: DynamicShape<System = Self>;
//     /// New shape instantiation. Used to bind a shape to a specific scene implementation.
//     fn instantiate(&self, shape: &Self::DynamicShape) -> symbol::GlobalInstanceId;
// }

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

// /// Type for every shape bound to a specific scene and GPU buffers. The easiest way to define
// such a /// shape is by using the `define_shape_system` macro.
// pub trait ShapeDefinition: CloneRef + Debug + Sized {
//     /// The shape system instance this shape belongs to.
//     type System: StaticShapeSystemInstance<Shape = Self>;
//     /// Accessor for the underlying sprite.
//     fn sprite(&self) -> &Sprite;
//     /// Check if given mouse-event-target means this shape.
//     fn is_this_target(&self, target: display::scene::PointerTargetId) -> bool {
//         self.sprite().is_this_target(target)
//     }
// }

pub trait ShapeDefinition2: 'static + Sized {
    type Params: Debug + Clone + CloneRef;
    type DynParams: Debug + Clone + CloneRef + Default;
    type GpuParams: Debug + Clone + CloneRef;
    fn pointer_events() -> bool;
    fn above() -> Vec<ShapeSystemId>;
    fn below() -> Vec<ShapeSystemId>;
    fn shape_def(
        style_watch: &display::shape::StyleWatch,
    ) -> display::shape::primitive::def::AnyShape;
    fn new_params(gpu_params: &Self::GpuParams, id: InstanceIndex) -> Self::Params;
    fn new_gpu_params(shape_system: &display::shape::ShapeSystem) -> Self::GpuParams;
    fn add_dyn_params_instance(dyn_params: &Self::DynParams, shape: &ShapeInstance<Self>);
    fn drop_instances(dyn_params: &Self::DynParams);
}

// /// Type for every shape which can, but does not have to be bound to a specific scene and GPU
// /// buffers. Dynamic shapes can be created freely and will be bound to a scene after being
// attached /// as scene children and an update frame event will be emitted.
// ///
// /// Dynamic shapes contain copy of all shape parameters and use them to set up the GPU parameters
// /// on bound.
// ///
// /// The easiest way to define such a shape is by using the `define_shape_system` macro.
// pub trait DynamicShape: display::Object + CloneRef + Debug + Default + Sized {
//     /// The static version of the shape. Dynamic shapes can be associated with one or more static
//     /// shapes after they are placed on the stage and initialized.
//     type StaticShape: ShapeDefinition;
//     /// The shape system instance this shape belongs to.
//     type System: DynShapeSystemInstance<DynamicShape = Self>;
//     /// Constructor.
//     fn new() -> Self;
//     /// Accessor for the underlying sprite, if the shape is initialized.
//     fn sprites(&self) -> Vec<Sprite>;
//     /// The "canvas" size of the shape. It defines the bounding-box for the shape drawing area.
//     fn size(&self) -> &DynamicParam<sprite::Size>;
//     /// Check if given pointer-event-target means this object.
//     fn is_this_target(&self, target: display::scene::PointerTargetId) -> bool {
//         self.sprites().into_iter().any(|sprite| sprite.is_this_target(target))
//     }
// }

// /// Internal utilities for managing [`DynamicShape`]s. You would normally not need to ever use it
// /// explicitly, however, it is exposed as public interface as it is required for user-defined
// shape /// systems. Again, instead of implementing shape systems from scratch, you'd rather use
// the /// `define_shape_system!` macro.
// pub trait DynamicShapeInternals {
//     /// Add a new [`Shape`] instance to this dynamic shape. Please note that dynamic shapes can
// be     /// attached with multiple [`Shape`]s at the same time if they are placed on multiple
// scene     /// [`Layer`]s. Then, each layer has a separate shape system, and thus, a separate
// [`Shape`] to     /// represent this [`DynamicShape`].
//     fn add_instance(&self, shape: Self::StaticShape);
//
//     /// Drop all [`Shape`] instanced assigned to this dynamic shape with the `add_instance`
// method.     fn drop_instances(&self);
// }


// === Type Families ===

// /// Accessor for the `Shape::System` associated type.
// pub type ShapeSystemOf<T> = <T as ShapeDefinition>::System;
//
// /// Accessor for the `Shape::System` associated type.
// pub type DynShapeSystemOf<T> = <T as DynamicShape>::System;



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
    value:      Rc<Cell<T::Item>>,
    attributes: Rc<RefCell<Vec<T>>>,
}

impl<T> DynamicParam<T>
where
    T: CellProperty,
    T::Item: Copy,
{
    /// Set the parameter value.
    pub fn set(&self, value: T::Item) {
        self.value.set(value);
        for attribute in &*self.attributes.borrow() {
            attribute.set(value)
        }
    }

    /// Get the parameter value.
    pub fn get(&self) -> T::Item {
        self.value.get()
    }

    /// Modify the parameter value.
    pub fn modify(&self, f: impl FnOnce(T::Item) -> T::Item) {
        self.set(f(self.get()))
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
        attribute.set(self.value.get());
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
        $(pointer_events = $pointer_events:tt;)?
        ($style:ident : Style $(,$gpu_param : ident : $gpu_param_type : ty)* $(,)?) {$($body:tt)*}
    ) => {
        $crate::_define_shape_system! {
            $(above = [$($always_above_1 $(::$always_above_2)*),*];)?
            $(below = [$($always_below_1 $(::$always_below_2)*),*];)?
            $(pointer_events = $pointer_events;)?
            [$style] ($($gpu_param : $gpu_param_type),*){$($body)*}
        }
    };

    (
        $($arg:ident = $arg_val:tt;)*
        ($($gpu_param : ident : $gpu_param_type : ty),* $(,)?) {$($body:tt)*}
    ) => {
        $crate::define_shape_system! {
            $($arg = $arg_val;)*
            (style : Style, $($gpu_param : $gpu_param_type),*){$($body)*}
        }
    }
}

#[derive(CloneRef, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[clone_ref(bound = "")]
pub struct ShapeInstance<Shape: ShapeDefinition2> {
    #[deref]
    pub params: Shape::Params,
    pub sprite: Sprite,
}

impl<Shape: ShapeDefinition2> display::Object for ShapeInstance<Shape> {
    fn display_object(&self) -> &display::object::Instance {
        self.sprite.display_object()
    }
}

#[derive(CloneRef, Deref, Derivative)]
#[clone_ref(bound = "")]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[allow(missing_docs)]
pub struct ShapeSystemY<S: ShapeDefinition2> {
    #[deref]
    gpu_params:       S::GpuParams,
    pub shape_system: ShapeSystem,
    style_watch:      crate::display::shape::StyleWatch,
}


impl<S: ShapeDefinition2> ShapeSystemY<S> {
    pub fn id() -> display::shape::ShapeSystemId {
        std::any::TypeId::of::<S>().into()
    }

    #[profile(Debug)]
    pub fn new(scene: &display::scene::Scene) -> Self {
        let style_watch = display::shape::StyleWatch::new(&scene.style_sheet);
        let shape_def = S::shape_def(&style_watch);
        let events = S::pointer_events();
        let shape_system = display::shape::ShapeSystem::new(scene, &shape_def, events);
        let gpu_params = S::new_gpu_params(&shape_system);
        Self { shape_system, style_watch, gpu_params }.init_refresh_on_style_change()
    }

    pub fn shape_system(&self) -> &display::shape::ShapeSystem {
        &self.shape_system
    }

    pub fn above() -> Vec<ShapeSystemId> {
        S::above()
    }
    pub fn below() -> Vec<ShapeSystemId> {
        S::below()
    }

    #[profile(Debug)]
    pub fn new_instance(&self) -> ShapeInstance<S> {
        let sprite = self.shape_system.new_instance();
        let id = sprite.instance_id;
        let params = S::new_params(&self.gpu_params, id);
        ShapeInstance { sprite, params }
    }

    #[profile(Debug)]
    pub fn instantiate(&self, dyn_shape: &DynamicShape<S>) -> symbol::GlobalInstanceId {
        let sprite = self.shape_system.new_instance();
        let instance_id = sprite.instance_id;
        let global_id = sprite.global_instance_id;
        let params = S::new_params(&self.gpu_params, instance_id);
        let shape = ShapeInstance { sprite, params };
        dyn_shape.add_instance(shape);
        global_id
    }
}

impl<S: ShapeDefinition2> display::shape::KnownShapeSystemId for ShapeSystemY<S> {
    fn shape_system_id() -> display::shape::ShapeSystemId {
        std::any::TypeId::of::<S>().into()
    }
}

impl<S: ShapeDefinition2> ShapeSystemY<S> {
    #[profile(Debug)]
    fn init_refresh_on_style_change(self) -> Self {
        let shape_system = self.shape_system.clone_ref();
        let style_watch = self.style_watch.clone_ref();
        self.style_watch.set_on_style_change(move || {
            shape_system.set_shape(&S::shape_def(&style_watch));
        });
        self
    }
}



/// A dynamic version of the [`Shape`]. In case the shape was initialized and bound to
/// the GPU, the parameters will be initialized as well and will point to the right
/// buffers sections. Otherwise, changing a parameter will not have any visual effect,
/// however, all the changes will be recorded and applied as soon as the shape will get
/// initialized.
#[derive(CloneRef, Deref, Derivative)]
#[clone_ref(bound = "")]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(Default(bound = ""))]
#[allow(missing_docs)]
pub struct DynamicShape<S: ShapeDefinition2> {
    #[deref]
    params:         S::DynParams,
    display_object: display::object::Instance,
    shapes:         Rc<RefCell<Vec<ShapeInstance<S>>>>,
}

impl<S: ShapeDefinition2> DynamicShape<S> {
    #[profile(Debug)]
    pub fn new() -> Self {
        default()
    }

    pub fn sprites(&self) -> Vec<Sprite> {
        self.shapes.borrow().iter().map(|t| t.sprite.clone_ref()).collect()
    }

    // fn size(&self) -> &display::shape::system::DynamicParam<sprite::Size> {
    //     &self.size
    // }

    #[profile(Debug)]
    pub fn add_instance(&self, shape: ShapeInstance<S>) {
        self.display_object.add_child(&shape);
        S::add_dyn_params_instance(&self.params, &shape);
        self.shapes.borrow_mut().push(shape);
    }

    #[profile(Debug)]
    pub fn drop_instances(&self) {
        for shape in mem::take(&mut *self.shapes.borrow_mut()) {
            self.display_object.remove_child(&shape);
        }
        S::drop_instances(&self.params);
    }
}

impl<S: ShapeDefinition2> display::shape::KnownShapeSystemId for DynamicShape<S> {
    fn shape_system_id() -> display::shape::ShapeSystemId {
        ShapeSystemY::<S>::shape_system_id()
    }
}

impl<S: ShapeDefinition2> display::Object for DynamicShape<S> {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

/// Internal helper for `define_shape_system`.
#[macro_export]
macro_rules! _define_shape_system {
    (
        $(above = [$($always_above_1:tt $(::$always_above_2:tt)*),*];)?
        $(below = [$($always_below_1:tt $(::$always_below_2:tt)*),*];)?
        $(pointer_events = $pointer_events:tt;)?
        [$style:ident]
        ($($gpu_param : ident : $gpu_param_type : ty),* $(,)?)
        {$($body:tt)*}
    ) => {

        pub use shape_system_definition::Shape;
        // pub use shape_system_definition::ShapeSystemX;
        // pub use shape_system_definition::DynamicShape;
        pub use shape_system_definition::DynamicParams;
        pub use shape_system_definition::View;

        // FIXME: To be investigated why it's needed. We should not use shorter names, but it's not
        //        obvious why they appear in the scope here.
        #[allow(unused_qualifications)]
        mod shape_system_definition {
            use super::*;
            use $crate::display;
            use $crate::display::symbol;
            use $crate::display::symbol::geometry::compound::sprite;
            use $crate::display::symbol::geometry::Sprite;
            use $crate::system::gpu;
            use $crate::system::gpu::data::Attribute;
            use $crate::display::shape::DynamicParamInternals;
            // use $crate::display::shape::DynamicShapeInternals;
            use $crate::display::shape::ShapeSystemId;
            use $crate::display::shape::ShapeOps;
            use $crate::display::shape::Var;
            use $crate::display::shape::PixelDistance;
            use $crate::display::shape::system::DynamicParam;
            use $crate::system::gpu::data::InstanceIndex;



            // =============
            // === Shape ===
            // =============

            #[derive(Clone, Copy, Debug)]
            pub struct MyShape;

            impl ShapeDefinition2 for MyShape {
                type Params = Params;
                type DynParams = DynamicParams;
                type GpuParams = GpuParams;
                fn pointer_events() -> bool {
                    let out = true;
                    $(let out = $pointer_events;)?
                    out
                }
                fn above() -> Vec<ShapeSystemId> {
                    vec![ $($($always_above_1 $(::$always_above_2)* :: ShapeSystem :: id()),*)? ]
                }
                fn below() -> Vec<ShapeSystemId> {
                    vec![ $($($always_below_1 $(::$always_below_2)* :: ShapeSystem :: id()),*)? ]
                }

                fn new_params(gpu_params:&Self::GpuParams, id: InstanceIndex) -> Self::Params {
                    $(let $gpu_param = gpu_params.$gpu_param.at(id);)*
                    Self::Params {$($gpu_param),*}
                }

                fn new_gpu_params(shape_system: &display::shape::ShapeSystem) -> Self::GpuParams {
                    $(
                        let name = stringify!($gpu_param);
                        let val  = gpu::data::default::gpu_default::<$gpu_param_type>();
                        let $gpu_param = shape_system.add_input(name,val);
                    )*
                    Self::GpuParams {$($gpu_param),*}
                }

                fn add_dyn_params_instance(dyn_params:&Self::DynParams, shape: &ShapeInstance<Self>){
                    dyn_params.size.add_attribute_binding(shape.sprite.size.clone_ref());
                    $(
                        let gpu_param = shape.$gpu_param.clone_ref();
                        dyn_params.$gpu_param.add_attribute_binding(gpu_param);
                    )*
                }

                fn drop_instances(dyn_params:&Self::DynParams) {
                    dyn_params.size.remove_attributes_bindings();
                    $(dyn_params.$gpu_param.remove_attributes_bindings();)*
                }

                fn shape_def(__style_watch__: &display::shape::StyleWatch)
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

            /// An initialized, GPU-bound shape definition. All changed parameters are immediately
            /// reflected in the [`Buffer`] and will be synchronised with GPU before next frame is
            /// drawn.
            #[derive(Clone,CloneRef,Debug)]
            #[allow(missing_docs)]
            pub struct Params {
                $(pub $gpu_param : Attribute<$gpu_param_type>),*
            }

            /// Parameters of the [`DynamicShape`].
            #[derive(Clone,CloneRef,Debug,Default)]
            #[allow(missing_docs)]
            pub struct DynamicParams {
                pub size: DynamicParam<sprite::Size>,
                $(pub $gpu_param: DynamicParam<Attribute<$gpu_param_type>>),*
            }

             #[derive(Clone, CloneRef, Debug)]
            #[allow(missing_docs)]
            pub struct GpuParams {
                $(pub $gpu_param: gpu::data::Buffer<$gpu_param_type>),*
            }

            pub type Shape = ShapeInstance<MyShape>;


            // ============
            // === View ===
            // ============

            /// A view of the defined shape. You can place the view in your objects and it will
            /// automatically initialize on-demand.
            pub type View = $crate::gui::component::ShapeView<MyShape>;
        }
    };
}


// ==============
// === Shapes ===
// ==============

mod shape {
    use super::*;
    use crate::data::color;
    use crate::display::shape::*;

    crate::define_shape_system! {
        (style:Style) {
            let circle1    = Circle(50.px());
            let circle_bg  = circle1.translate_x(-(50.0.px()));
            let circle_sub = circle1.translate_y(-(50.0.px()));
            let rect       = Rect((100.0.px(),100.0.px()));
            let shape      = circle_bg + rect - circle_sub;
            let shape      = shape.fill(color::Rgba::new(0.3, 0.3, 0.3, 1.0));
            shape.into()
        }
    }
}



// ===================
// === Entry Point ===
// ===================

use crate::display::navigation::navigator::Navigator;
use crate::display::world::World;
use wasm_bindgen::prelude::*;

/// The example entry point.
#[entry_point(test)]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let view1 = shape::View::new();
    view1.size.set(Vector2::new(300.0, 300.0));
    view1.mod_position(|t| *t = Vector3::new(50.0, 50.0, 0.0));

    world.add_child(&view1);
    world.keep_alive_forever();

    frp::new_network! { network
        trace view1.events.mouse_over;
        trace view1.events.mouse_out;
        trace view1.events.mouse_down;
    }

    world
        .on
        .before_frame
        .add(move |_time| {
            let _keep_alive = &network;
            let _keep_alive = &view1;
            let _keep_alive = &navigator;
        })
        .forget();
}
