//! Root module for GUI related components.

//! NOTE
//! This file is under a heavy development. It contains commented lines of code and some code may
//! be of poor quality. Expect drastic changes.

use crate::prelude::*;

use crate::animation::physics::inertia;
use crate::animation::physics::inertia::DynSimulator;
use crate::animation::easing;
use crate::display::object::traits::*;
use crate::display::scene::MouseTarget;
use crate::display::scene::Scene;
use crate::display::scene::ShapeRegistry;
use crate::display::shape::primitive::system::Shape;
use crate::display;

use enso_frp as frp;



// =======================
// === ShapeViewEvents ===
// =======================

/// FRP event endpoints exposed by each shape view. In particular these are all mouse events
/// which are triggered by mouse interactions after the shape view is placed on the scene.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct ShapeViewEvents {
    pub network    : frp::Network,
    pub mouse_down : frp::Source,
    pub mouse_over : frp::Source,
    pub mouse_out  : frp::Source,
    pub on_drop    : frp::Source,
}

impl ShapeViewEvents {
    fn new() -> Self {
        frp::new_network! { network
            on_drop    <- source_();
            mouse_down <- source_();
            mouse_over <- source_();
            mouse_out  <- source_();

            is_mouse_over <- any (mouse_over,mouse_out).toggle();
            out_on_drop   <- on_drop.gate(&is_mouse_over);
            eval_ out_on_drop (mouse_out.emit(()));
        }
        Self {network,mouse_down,mouse_over,mouse_out,on_drop}
    }
}

impl MouseTarget for ShapeViewEvents {
    fn mouse_down (&self) -> &frp::Source { &self.mouse_down }
    fn mouse_over (&self) -> &frp::Source { &self.mouse_over }
    fn mouse_out  (&self) -> &frp::Source { &self.mouse_out  }
}





// =================
// === ShapeView ===
// =================

/// Automatically managed view of a `Shape`. The view is initially empty and is filled with a
/// reference to an existing `Shape` as soon as it is placed on the scene and the scene is updated.
/// As soon as it is removed from the scene, the shape is freed.
#[derive(Clone,CloneRef,Debug)]
#[clone_ref(bound="S:CloneRef")]
#[allow(missing_docs)]
pub struct ShapeView<S:Shape> {
    model : Rc<ShapeViewModel<S>>
}

impl<S:Shape> Deref for ShapeView<S> {
    type Target = Rc<ShapeViewModel<S>>;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

#[derive(Debug)]
#[allow(missing_docs)]
pub struct ShapeViewModel<S:Shape> {
    pub registry       : ShapeRegistry,
    pub shape          : S,
    pub display_object : display::object::Instance,
    pub events         : ShapeViewEvents,
}

impl<S:Shape> Drop for ShapeViewModel<S> {
    fn drop(&mut self) {
        for sprite in self.shape.sprites() {
            let symbol_id   = sprite.symbol_id();
            let instance_id = *sprite.instance_id;
            self.registry.remove_mouse_target(symbol_id,instance_id);
        }
        self.events.on_drop.emit(());
    }
}
///// A structure containing data which is constructed or dropped when the `ShapeView` is added or
///// removed from the scene.
//#[derive(Clone,CloneRef,Debug)]
//pub struct ShapeViewData<T:ShapeViewDefinition> {
//    /// A data associated with the shape. In simple cases, this data could be just a marker struct.
//    /// In more complex examples, it could contain callback handles. For example, for a cursor
//    /// implementation, its `data` contains a callback listening to scene size change in order to
//    /// update the `shape` dimensions.
//    pub phantom : PhantomData<T>,
//    /// A shape instance. Refer to `Shape` docs to learn more.
//    pub shape : T::Shape,
//}

impl<S:Shape> ShapeView<S> {
    /// Constructor.
    pub fn new(logger:impl AnyLogger, scene:&Scene) -> Self {
        let logger         = Logger::sub(logger,"shape_view");
        let display_object = display::object::Instance::new(logger);
        let registry       = scene.shapes.clone_ref();
        let shape          = registry.new_instance::<S>();
        let events         = ShapeViewEvents::new();
        display_object.add_child(&shape);
        for sprite in shape.sprites() {
            let events      = events.clone_ref();
            let symbol_id   = sprite.symbol_id();
            let instance_id = *sprite.instance_id;
            registry.insert_mouse_target(symbol_id,instance_id,events);
        }

        let model = Rc::new(ShapeViewModel {registry,display_object,events,shape});
        Self {model}
    }

//    fn init(self) -> Self {
//        self.init_on_show();
//        self.init_on_hide();
//        self
//    }
//
//    fn init_on_show(&self) {
//        let weak_data   = Rc::downgrade(&self.data);
//        let weak_parent = self.display_object.downgrade();
//        let events      = self.events.clone_ref();
//        self.display_object.set_on_show_with(move |scene| {
//            let shape_registry: &ShapeRegistry = &scene.shapes;
//            weak_data.upgrade().for_each(|self_data| {
//                weak_parent.upgrade().for_each(|parent| {
//                    let shape = shape_registry.new_instance::<T::Shape>();
//                    parent.add_child(&shape);
//                    for sprite in shape.sprites() {
//                        let events      = events.clone_ref();
//                        let symbol_id   = sprite.symbol_id();
//                        let instance_id = *sprite.instance_id;
//                        shape_registry.insert_mouse_target(symbol_id,instance_id,events);
//                    }
//                    let data = T::new(&shape,scene,shape_registry);
//                    let data = ShapeViewData {data,shape};
//                    *self_data.borrow_mut() = Some(data);
//                })
//            });
//        });
//    }
//
//    fn init_on_hide(&self) {
//        let weak_data = Rc::downgrade(&self.data);
//        self.display_object.set_on_hide_with(move |scene| {
//            let shape_registry: &ShapeRegistry = &scene.shapes;
//            weak_data.upgrade().for_each(|data| {
//                data.borrow().for_each_ref(|data| {
//                    for sprite in data.shape.sprites() {
//                        let symbol_id   = sprite.symbol_id();
//                        let instance_id = *sprite.instance_id;
//                        shape_registry.remove_mouse_target(symbol_id,instance_id);
//                    }
//                });
//                *data.borrow_mut() = None;
//            });
//        });
//    }
}

impl<T:Shape> display::Object for ShapeView<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

///// Definition of a new shape view. In simple cases this could be a marker struct. To learn more
///// refer to documentation of `ShapeViewData` and example usages in components.
//pub trait ShapeViewDefinition : CloneRef + 'static {
//    /// Associated shape instance type.
//    type Shape : Shape;
////    fn new(shape:&Self::Shape, scene:&Scene, shape_registry:&ShapeRegistry) -> Self;
//}



// ==================
// === Animatable ===
// ==================

/// Newtype that indicates that the wrapped value is valid to be used in animations.
#[derive(Debug)]
pub struct AnimationLinearSpace<T> {
    /// Wrapped value representing the animation space value.
    pub value: T
}

/// Indicate what datatype to use in the animation space representation.
pub trait HasAnimationSpaceRepr {
    /// Representation in animation space. Needs to support linear interpolation and all
    /// pre-requisites of `inertia::Value`.
    type AnimationSpaceRepr: inertia::Value;
}

/// HasAnimationSpaceRepr::AnimationSpaceRepr getter.
pub type AnimationSpaceRepr<T> = <T as HasAnimationSpaceRepr>::AnimationSpaceRepr;

/// Strongly typed `AnimationSpaceRepr`
pub type AnimationLinearSpaceRepr<T> = AnimationLinearSpace<AnimationSpaceRepr<T>>;

pub trait Animatable = HasAnimationSpaceRepr + BiInto<AnimationLinearSpaceRepr<Self>>;

/// Convert the animation space value to the respective `Animatable`.
pub fn from_animation_space<T:Animatable>(value:AnimationSpaceRepr<T>) -> T {
    AnimationLinearSpace{value}.into()
}

/// Convert `Animatable` to respective animation space value.
pub fn into_animation_space_repr<T:Animatable>(t:T) -> AnimationSpaceRepr<T> {
    t.into().value
}

macro_rules! define_self_animatable {
    ($type:ty ) => {
        impl HasAnimationSpaceRepr for $type { type AnimationSpaceRepr = $type; }

        impl From<$type> for AnimationLinearSpace<$type> {
            fn from(value:$type) -> AnimationLinearSpace<$type> {
                 AnimationLinearSpace{value}
            }
        }

        impl Into<$type> for AnimationLinearSpace<$type> {
            fn into(self) -> $type {
                self.value
            }
        }
    }
}

define_self_animatable!(f32);
define_self_animatable!(Vector2);
define_self_animatable!(Vector3);
define_self_animatable!(Vector4);



// =================
// === Animation ===
// =================

/// Simulator used to run the animation.
pub type AnimationSimulator<T> = DynSimulator<AnimationSpaceRepr<T>>;

/// Smart animation handler. Contains of dynamic simulation and frp endpoint. Whenever a new value
/// is computed, it is emitted via the endpoint.
#[derive(CloneRef,Derivative,Debug)]
#[derivative(Clone(bound=""))]
#[allow(missing_docs)]
pub struct Animation<T:Animatable+frp::Data> {
    pub target : frp::Any<T>,
    pub value  : frp::Stream<T>,
    pub skip   : frp::Any,
}

#[allow(missing_docs)]
impl<T:Animatable+frp::Data> Animation<T> {
    /// Constructor. The initial value of the animation is set to `default`.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            value_src <- any_mut::<T>();
        }
        let simulator = AnimationSimulator::<T>::new(
            Box::new(f!((t) value_src.emit(from_animation_space::<T>(t))))
        );
        frp::extend! { network
            target <- any_mut::<T>();
            skip   <- any_mut::<()>();
            eval  target ((t) simulator.set_target_value(into_animation_space_repr(t.clone())));
            eval_ skip   (simulator.skip());
        }
        let value = value_src.into();
        network.store(&simulator);
        Self{target,value,skip}
    }

    /// Constructor. The initial value is provided explicitly.
    pub fn new_with_init(network:&frp::Network, init:T) -> Self {
        let this = Self::new(network);
        this.target.emit(init);
        this.skip.emit(());
        this
    }

    /// Constructor. There is no initial value. The first emitted `target` value will be used
    /// without animation.
    pub fn new_non_init(network:&frp::Network) -> Self {
        let this = Self::new(network);
        frp::extend! { network
            init      <- any_mut();
            on_init   <- this.target.gate_not(&init);
            init      <+ this.target.constant(true);
            this.skip <+ on_init.constant(());
        }
        this
    }
}



// =============
// === Tween ===
// =============

/// Smart tween handler. Contains tween animator and frp endpoint. Whenever a new value is computed,
/// it is emitted via the endpoint.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
#[allow(missing_docs)]
pub struct Tween {
    #[shrinkwrap(main_field)]
    pub animator : easing::DynAnimator<f32,easing::QuadInOut>,
    pub value    : frp::Stream<f32>,
}

impl Tween {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            def target = source::<f32>();
        }
        let f        = easing::quad_in_out();
        let animator = easing::DynAnimator::new_not_started(0.0,1.0,f,Box::new(f!((t) target.emit(t))));
        let value    = target.into();
        Self {animator,value}
    }
}



// ============================
// === DEPRECATED Animation ===
// ============================

/// Smart animation handler. Contains of dynamic simulation and frp endpoint. Whenever a new value
/// is computed, it is emitted via the endpoint.
///
/// # DEPRECATION
/// This component is deprecated. Use `Animation` instead, which exposes much more FRP-oriented API
/// than this component. The transition to new version should be straightforward but requires some
/// attention. The functionalities should be the same and should be accessible by similar API with
/// two differences:
///
/// 1. The API bases on FRP now, which means that the usage can probably be refactored to look
///    much nicer.
///
/// 2. After setting the value for the first time, the value is provided as the output value without
///    any animation. This is different behavior from the previous implementation, where even
///    setting the value for the first time would create animation between `default()` value and the
///    new target. If your code depends on this behavior, it needs to be changed.
#[derive(CloneRef,Derivative,Debug,Shrinkwrap)]
#[derivative(Clone(bound=""))]
#[allow(missing_docs)]
#[allow(non_camel_case_types)]
pub struct DEPRECATED_Animation<T:Animatable> {
    #[shrinkwrap(main_field)]
    pub simulator : DynSimulator<T::AnimationSpaceRepr>,
    pub value     : frp::Stream<T>,
}

#[allow(missing_docs)]
impl<T:Animatable+frp::Data> DEPRECATED_Animation<T> {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            def target = source::<T>();
        }
        let simulator = DynSimulator::<T::AnimationSpaceRepr>::new(Box::new(f!((t) {
             target.emit(from_animation_space::<T>(t))
        })));
        let value = target.into();
        Self {simulator,value}
    }

    pub fn set_value(&self, value:T) {
        let animation_space_repr = value.into();
        self.simulator.set_value(animation_space_repr.value);
    }

    pub fn value(&self) -> T {
        let value = self.simulator.value();
        from_animation_space(value)
    }

    pub fn set_target_value(&self, target_value:T) {
        let state:AnimationLinearSpace<_> = target_value.into();
        self.simulator.set_target_value(state.value);
    }

    pub fn target_value(&self) -> T {
        let value = self.simulator.target_value();
        from_animation_space(value)
    }
}
