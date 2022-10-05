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


pub trait InstanceParamsTrait {
    fn swap(&self, other: &Self);
}


pub trait CustomSystemData<S: Shape> {
    fn new(scene: &Scene, data: &ShapeSystemStandardData<S>) -> Self;
}

impl<S: Shape> CustomSystemData<S> for () {
    fn new(_scene: &Scene, _data: &ShapeSystemStandardData<S>) -> Self {
        ()
    }
}



// =============
// === Shape ===
// =============

pub trait Shape: 'static + Sized {
    type InstanceParams: Debug + InstanceParamsTrait;
    type GpuParams: Debug;
    type SystemData: CustomSystemData<Self>;
    fn pointer_events() -> bool;
    fn always_above() -> &'static [ShapeSystemId];
    fn always_below() -> &'static [ShapeSystemId];
    fn new_instance_params(
        sprite: &Sprite,
        gpu_params: &Self::GpuParams,
        id: InstanceIndex,
    ) -> Self::InstanceParams;
    fn new_gpu_params(shape_system: &display::shape::ShapeSystemModel) -> Self::GpuParams;
    fn shape_def(
        style_watch: &display::shape::StyleWatch,
    ) -> display::shape::primitive::def::AnyShape;
}



// =====================
// === ShapeInstance ===
// =====================

/// A visible shape instance, bound to a particular [`ShapeSystem`]. Shape instances are stored and
/// managed by [`ShapeProxy`]s.
#[allow(missing_docs)]
#[derive(Deref, Derivative)]
#[derivative(Debug(bound = ""))]
pub struct ShapeInstance<S: Shape> {
    #[deref]
    pub params:     S::InstanceParams,
    pub sprite:     RefCell<Sprite>,
    display_object: display::object::Instance,
}

impl<S: Shape> display::Object for ShapeInstance<S> {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl<S: Shape> ShapeInstance<S> {
    pub(crate) fn swap(&self, other: &Self) {
        self.params.swap(&other.params);
        self.sprite.swap(&other.sprite);
        for child in other.display_object.remove_all_children() {
            self.display_object.add_child(&child);
        }
    }
}



// // ==================
// // === ShapeProxy ===
// // ==================
//
// /// A proxy for zero or more [`ShapeInstance`]s. Proxies can be created and modified even if no
// /// [`ShapeInstance`] is created yet. [`ShapeInstance`]s are bound to a particular
// [`ShapeSystem`]. /// They can be destroyed and created on demand (e.g. after moving the shape to
// a different layer, /// because each layer has its own copy of [`ShapeSystem`]). The
// [`ShapeProxy`] is an interface for /// all created [`ShapeInstance`]s. In case there are no
// [`ShapeInstance`]s, changing a parameter /// will not have any visual effect, however, all the
// changes will be recorded and applied as soon /// as a new [`ShapeInstance`] appears.
// #[allow(missing_docs)]
// #[derive(Deref, Derivative)]
// #[derivative(Debug(bound = ""))]
// #[derivative(Default(bound = ""))]
// #[allow(missing_docs)]
// pub struct ShapeProxy<S: Shape> {
//     #[deref]
//     proxy_params:   S::ProxyParams,
//     display_object: display::object::Instance,
//     // In most cases, there will be zero or one instance. Rarely, a shape can be added to two or
//     // more layers at the same time. Thus, using a small vec with a capacity of 2 is a good usage
//     // approximation.
//     instances:      Rc<RefCell<SmallVec<[ShapeInstance<S>; 2]>>>,
// }
//
// impl<S: Shape> ShapeProxy<S> {
//     /// Add a new shape instance and bind its parameters to the proxy parameters.
//     #[profile(Debug)]
//     pub fn add_instance(&self, instance: ShapeInstance<S>) {
//         self.display_object.add_child(&instance);
//         S::bind_proxy_params(&self.proxy_params, &instance);
//         self.instances.borrow_mut().push(instance);
//     }
//
//     /// Drop all shape instances. Unbnind the parameters.
//     #[profile(Debug)]
//     pub fn drop_all_instances(&self) {
//         mem::take(&mut *self.instances.borrow_mut());
//         S::drop_proxy_params_bindings(&self.proxy_params);
//     }
// }
//
// impl<S: Shape> display::shape::KnownShapeSystemId for ShapeProxy<S> {
//     fn shape_system_id() -> display::shape::ShapeSystemId {
//         ShapeSystem::<S>::id()
//     }
// }
//
// impl<S: Shape> display::Object for ShapeProxy<S> {
//     fn display_object(&self) -> &display::object::Instance {
//         &self.display_object
//     }
// }



// ===================
// === ShapeSystem ===
// ===================

#[derive(CloneRef, Deref, Derivative)]
#[clone_ref(bound = "")]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = "S::SystemData: Debug"))]
pub struct ShapeSystem<S: Shape> {
    data: Rc<ShapeSystemData<S, S::SystemData>>,
}


#[derive(Deref, Derivative)]
#[derivative(Debug(bound = "UserData: Debug"))]
pub struct ShapeSystemData<S: Shape, UserData> {
    #[deref]
    pub standard: ShapeSystemStandardData<S>,
    pub user:     UserData,
}

#[derive(Deref, Derivative)]
#[derivative(Debug(bound = ""))]
pub struct ShapeSystemStandardData<S: Shape> {
    #[deref]
    gpu_params:  S::GpuParams,
    pub model:   ShapeSystemModel,
    style_watch: crate::display::shape::StyleWatch,
}

impl<S: Shape> ShapeSystem<S> {
    pub fn id() -> display::shape::ShapeSystemId {
        std::any::TypeId::of::<S>().into()
    }

    pub fn sprite_system(&self) -> &SpriteSystem {
        &self.data.model.sprite_system
    }

    #[profile(Debug)]
    pub fn new(scene: &display::scene::Scene) -> Self {
        let style_watch = display::shape::StyleWatch::new(&scene.style_sheet);
        let shape_def = S::shape_def(&style_watch);
        let events = S::pointer_events();
        let model = display::shape::ShapeSystemModel::new(scene, &shape_def, events);
        let gpu_params = S::new_gpu_params(&model);
        let standard = ShapeSystemStandardData { gpu_params, model, style_watch };
        let user = CustomSystemData::<S>::new(scene, &standard);
        standard.model.init();
        let data = Rc::new(ShapeSystemData { standard, user });
        Self { data }.init_refresh_on_style_change()
    }

    #[profile(Debug)]
    pub fn new_instance(&self) -> ShapeInstance<S> {
        let sprite = self.model.sprite_system.new_instance();
        let id = sprite.instance_id;
        let params = S::new_instance_params(&sprite, &self.gpu_params, id);
        let display_object = display::object::Instance::new();
        display_object.add_child(&sprite);
        let sprite = RefCell::new(sprite);
        ShapeInstance { sprite, params, display_object }
    }

    // #[profile(Debug)]
    // pub fn instantiate(&self, dyn_shape: &ShapeProxy<S>) -> symbol::GlobalInstanceId {
    //     panic!();
    //     // let sprite = self.model.sprite_system.new_instance();
    //     // let instance_id = sprite.instance_id;
    //     // let global_id = sprite.global_instance_id;
    //     // let params = S::new_instance_params(&self.gpu_params, instance_id);
    //     // let shape = ShapeInstance { sprite, params };
    //     // dyn_shape.add_instance(shape);
    //     // global_id
    // }

    #[profile(Debug)]
    pub fn instantiate(&self) -> (ShapeInstance<S>, symbol::GlobalInstanceId) {
        let sprite = self.model.sprite_system.new_instance();
        let instance_id = sprite.instance_id;
        let global_id = sprite.global_instance_id;
        let params = S::new_instance_params(&sprite, &self.gpu_params, instance_id);
        let display_object = display::object::Instance::new();
        display_object.add_child(&sprite);
        let sprite = RefCell::new(sprite);
        let shape = ShapeInstance { sprite, params, display_object };
        (shape, global_id)
    }
}

impl<S: Shape> ShapeSystem<S> {
    #[profile(Debug)]
    fn init_refresh_on_style_change(self) -> Self {
        let model = &self.model;
        let style_watch = self.style_watch.clone_ref();
        self.style_watch.set_on_style_change(f!(() model.set_shape(&S::shape_def(&style_watch))));
        self
    }
}

/// Definition of a shape management system.
///
/// Please note that you would rather not need to use it directly, as it would require manual
/// management of buffer handlers. In order to automate the management, there is
/// `ShapeSystemInstance` and the `define_shape_system` macro.
///
/// Under the hood, it is a specialized version of `SpriteSystem`.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct ShapeSystemModel {
    pub sprite_system:  SpriteSystem,
    pub shape:          Rc<RefCell<def::AnyShape>>,
    pub material:       Rc<RefCell<Material>>,
    /// Enables or disables pointer events on this shape system. All shapes of a shape system which
    /// has pointer events disabled would be completely transparent for the mouse (they would pass
    /// through all mouse events).
    pub pointer_events: Immutable<bool>,
}

impl ShapeSystemModel {
    /// Constructor.
    #[profile(Detail)]
    pub fn new<'t, S, Sh>(scene: S, shape: Sh, pointer_events: bool) -> Self
    where
        S: Into<&'t Scene>,
        Sh: Into<def::AnyShape>, {
        let shape = shape.into();
        let sprite_system = SpriteSystem::new();
        let material = Rc::new(RefCell::new(Self::surface_material()));
        let pointer_events = Immutable(pointer_events);
        let shape = Rc::new(RefCell::new(shape));
        Self { sprite_system, shape, material, pointer_events }
    }

    fn init(&self) {
        self.reload_shape();
    }

    // TODO
    // We should handle these attributes in a nicer way. Currently, they are hardcoded here, and we
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

impl display::Object for ShapeSystemModel {
    fn display_object(&self) -> &display::object::Instance {
        self.sprite_system.display_object()
    }
}


/// Abstraction for every entity which is associated with a shape system (user generated one). For
/// example, all defined shapes are associated with a shape system, and thus they implement this
/// trait.
pub trait KnownShapeSystemId {
    /// The ID of a user defined shape system.
    fn shape_system_id() -> ShapeSystemId;
}



// ==================
// === ProxyParam ===
// ==================

#[derive(Debug)]
#[allow(missing_docs)]
pub struct ProxyParam<T> {
    attribute: RefCell<T>,
}

impl<T> ProxyParam<T>
where
    T: CellProperty,
    T::Item: Copy + Debug,
{
    pub fn new(attribute: T) -> Self {
        let attribute = RefCell::new(attribute);
        Self { attribute }
    }

    /// Set the parameter value.
    pub fn set(&self, value: T::Item) {
        self.attribute.borrow_mut().set(value)
    }

    /// Get the parameter value.
    pub fn get(&self) -> T::Item {
        self.attribute.borrow().get()
    }

    /// Modify the parameter value.
    pub fn modify(&self, f: impl FnOnce(T::Item) -> T::Item) {
        self.set(f(self.get()))
    }

    pub fn swap(&self, other: &Self) {
        other.set(self.get());
        self.attribute.swap(&other.attribute);
    }
}

// /// Internal utilities for managing [`ProxyParam`]s. You would normally not need to ever use it
// /// explicitly, however, it is exposed as public interface as it is required for user-defined
// shape /// systems. Again, instead of implementing shape systems from scratch, you'd rather use
// the /// `define_shape_system!` macro.
// pub trait DynamicParamInternals<T> {
//     /// Add a new binding to an attribute. This is done for every [`ProxyParam`] after the
//     /// [`ShapeProxy`] gets initialized with a new [`Shape`].
//     fn add_attribute_binding(&self, attribute: T);
//     /// Remove all attribute bindings. This is used, for example, when the [`ShapeProxy`] gets
//     /// removed from the display hierarchy.
//     fn remove_attributes_bindings(&self);
// }
//
// impl<T> DynamicParamInternals<T> for ProxyParam<T>
// where
//     T: CellProperty,
//     T::Item: Copy,
// {
//     fn remove_attributes_bindings(&self) {
//         *self.attributes.borrow_mut() = default();
//     }
//
//     fn add_attribute_binding(&self, attribute: T) {
//         attribute.set(self.value.get());
//         self.attributes.borrow_mut().push(attribute);
//     }
// }



// ==============
// === Macros ===
// ==============


#[macro_export]
macro_rules! define_shape_system {
    (
        $(SystemData($system_data:ident))?
        $(above = [$($always_above_1:tt $(::$always_above_2:tt)*),*];)?
        $(below = [$($always_below_1:tt $(::$always_below_2:tt)*),*];)?
        $(pointer_events = $pointer_events:tt;)?
        ($style:ident : Style $(,$gpu_param : ident : $gpu_param_type : ty)* $(,)?) {$($body:tt)*}
    ) => {
        $crate::_define_shape_system! {
            $(SystemData($system_data))?
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



/// Internal helper for `define_shape_system`.
#[macro_export]
macro_rules! _define_shape_system {
    (
        $(SystemData($system_data:ident))?
        $(above = [$($always_above_1:tt $(::$always_above_2:tt)*),*];)?
        $(below = [$($always_below_1:tt $(::$always_below_2:tt)*),*];)?
        $(pointer_events = $pointer_events:tt;)?
        [$style:ident]
        ($($gpu_param : ident : $gpu_param_type : ty),* $(,)?)
        {$($body:tt)*}
    ) => {

        pub use shape_system_definition::Shape;
        // pub use shape_system_definition::ShapeSystemX;
        // pub use shape_system_definition::ShapeProxy;
        // pub use shape_system_definition::ProxyParams;
        pub use shape_system_definition::View;

        // FIXME: To be investigated why it's needed. We should not use shorter names, but it's not
        //        obvious why they appear in the scope here.
        #[allow(unused_qualifications)]
        mod shape_system_definition {
            use super::*;
            use $crate::prelude::*;
            use $crate::display;
            use $crate::display::symbol;
            use $crate::display::symbol::geometry::compound::sprite;
            use $crate::display::symbol::geometry::Sprite;
            use $crate::system::gpu;
            use $crate::system::gpu::data::Attribute;
            // use $crate::display::shape::DynamicParamInternals;
            use $crate::display::shape::ShapeInstance;
            use $crate::display::shape::ShapeSystemId;
            use $crate::display::shape::ShapeOps;
            use $crate::display::shape::Var;
            use $crate::display::shape::PixelDistance;
            use $crate::display::shape::system::ProxyParam;
            use $crate::system::gpu::data::InstanceIndex;
            use $crate::data::color;
            use $crate::display::shape::*;


            // =============
            // === Shape ===
            // =============

            #[derive(Clone, Copy, Debug)]
            pub struct Shape;

            impl $crate::display::shape::system::Shape for Shape {
                type InstanceParams = InstanceParams;
                type GpuParams = GpuParams;
                type SystemData = ($($system_data)?);
                fn pointer_events() -> bool {
                    let out = true;
                    $(let out = $pointer_events;)?
                    out
                }

                fn always_above() -> &'static [ShapeSystemId] {
                    &[ $($($always_above_1 $(::$always_above_2)* :: ShapeSystemModel :: id()),*)? ]
                }

                fn always_below() -> &'static [ShapeSystemId] {
                    &[ $($($always_below_1 $(::$always_below_2)* :: ShapeSystemModel :: id()),*)? ]
                }

                fn new_instance_params(sprite: &Sprite, gpu_params:&Self::GpuParams, id: InstanceIndex) -> Self::InstanceParams {
                    let size = ProxyParam::new(sprite.size.clone_ref());
                    $(let $gpu_param = ProxyParam::new(gpu_params.$gpu_param.at(id));)*
                    Self::InstanceParams { size, $($gpu_param),* }
                }

                fn new_gpu_params(shape_system: &display::shape::ShapeSystemModel) -> Self::GpuParams {
                    $(
                        let name = stringify!($gpu_param);
                        let val  = gpu::data::default::gpu_default::<$gpu_param_type>();
                        let $gpu_param = shape_system.add_input(name,val);
                    )*
                    Self::GpuParams {$($gpu_param),*}
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
            #[derive(Debug)]
            #[allow(missing_docs)]
            pub struct InstanceParams {
                pub size: ProxyParam<sprite::Size>,
                $(pub $gpu_param : ProxyParam<Attribute<$gpu_param_type>>),*
            }

            impl InstanceParamsTrait for InstanceParams {
                fn swap(&self, other: &Self) {
                    self.size.swap(&other.size);
                    $(self.$gpu_param.swap(&other.$gpu_param);)*
                }
            }

            #[derive(Clone, CloneRef, Debug)]
            #[allow(missing_docs)]
            pub struct GpuParams {
                $(pub $gpu_param: gpu::data::Buffer<$gpu_param_type>),*
            }


            // ============
            // === View ===
            // ============

            /// A view of the defined shape. You can place the view in your objects and it will
            /// automatically initialize on-demand.
            pub type View = $crate::gui::component::ShapeView<Shape>;
        }
    };
}


// ==============
// === Shapes ===
// ==============

mod shape {
    use super::*;


    crate::define_shape_system! {
        (style:Style, foo:f32) {
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
