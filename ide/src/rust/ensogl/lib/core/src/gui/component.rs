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
#[derive(Clone,CloneRef,Debug,Deref)]
#[clone_ref(bound="S:CloneRef")]
#[allow(missing_docs)]
pub struct ShapeView<S:Shape> {
    model : Rc<ShapeViewModel<S>>
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



// =================
// === Animation ===
// =================

/// Smart animation handler. Contains of dynamic simulation and frp endpoint. Whenever a new value
/// is computed, it is emitted via the endpoint.
#[derive(CloneRef,Derivative,Debug,Shrinkwrap)]
#[derivative(Clone(bound=""))]
#[allow(missing_docs)]
pub struct Animation<T> {
    #[shrinkwrap(main_field)]
    pub simulator : DynSimulator<T>,
    pub value     : frp::Stream<T>,
}

impl<T:inertia::Value> Animation<T> {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            def target = source::<T>();
        }
        let simulator = DynSimulator::<T>::new(Box::new(f!((t) target.emit(t))));
        let value     = target.into();
        Self {simulator,value}
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
