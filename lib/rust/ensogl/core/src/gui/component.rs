//! Root module for GUI related components.

use crate::display::object::traits::*;
use crate::prelude::*;

use crate::application::Application;
use crate::display;
use crate::display::scene;
use crate::display::scene::layer::WeakLayer;
use crate::display::scene::Scene;
use crate::display::scene::ShapeRegistry;
use crate::display::shape::primitive::system::DynamicShape;
use crate::display::shape::primitive::system::DynamicShapeInternals;
use crate::display::symbol;


// ==============
// === Export ===
// ==============

pub use crate::display::scene::PointerTarget;



// =================
// === ShapeView ===
// =================

/// A view for a shape definition. The view manages the lifetime and scene-registration of a shape
/// instance. In particular, it registers / unregisters callbacks for shape initialization and mouse
/// events handling.
#[derive(Clone, CloneRef, Debug)]
#[clone_ref(bound = "S:CloneRef")]
#[allow(missing_docs)]
pub struct ShapeView<S> {
    model: Rc<ShapeViewModel<S>>,
}

impl<S> Deref for ShapeView<S> {
    type Target = Rc<ShapeViewModel<S>>;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl<S: DynamicShapeInternals + 'static> ShapeView<S> {
    /// Constructor.
    pub fn new(logger: impl AnyLogger) -> Self {
        let model = Rc::new(ShapeViewModel::new(logger));
        Self { model }.init()
    }

    fn init(self) -> Self {
        self.init_on_scene_layer_changed();
        self
    }

    fn init_on_scene_layer_changed(&self) {
        let weak_model = Rc::downgrade(&self.model);
        self.display_object().set_on_scene_layer_changed(move |scene, old_layers, new_layers| {
            if let Some(model) = weak_model.upgrade() {
                model.on_scene_layers_changed(scene, old_layers, new_layers)
            }
        });
    }
}

impl<S> HasContent for ShapeView<S> {
    type Content = S;
}



// ======================
// === ShapeViewModel ===
// ======================

/// Model of [`ShapeView`].
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct ShapeViewModel<S> {
    shape:               S,
    pub events:          PointerTarget,
    pub registry:        RefCell<Option<ShapeRegistry>>,
    pub pointer_targets: RefCell<Vec<symbol::GlobalInstanceId>>,
}

impl<S> Deref for ShapeViewModel<S> {
    type Target = S;
    fn deref(&self) -> &Self::Target {
        &self.shape
    }
}

impl<S> Drop for ShapeViewModel<S> {
    fn drop(&mut self) {
        self.unregister_existing_mouse_targets();
        self.events.on_drop.emit(());
    }
}

impl<S: DynamicShapeInternals> ShapeViewModel<S> {
    fn on_scene_layers_changed(
        &self,
        scene: &Scene,
        old_layers: &[WeakLayer],
        new_layers: &[WeakLayer],
    ) {
        self.drop_from_all_scene_layers(old_layers);
        for weak_layer in new_layers {
            if let Some(layer) = weak_layer.upgrade() {
                self.add_to_scene_layer(scene, &layer)
            }
        }
    }

    fn drop_from_all_scene_layers(&self, old_layers: &[WeakLayer]) {
        for layer in old_layers {
            if let Some(layer) = layer.upgrade() {
                let (instance_count, shape_system_id, _) =
                    layer.shape_system_registry.drop_instance::<S>();
                if instance_count == 0 {
                    layer.remove_shape_system(shape_system_id);
                }
            }
        }
        self.shape.drop_instances();
        self.unregister_existing_mouse_targets();
    }
}

impl<S: DynamicShape> ShapeViewModel<S> {
    /// Constructor.
    pub fn new(logger: impl AnyLogger) -> Self {
        let shape = S::new(logger);
        let events = PointerTarget::new();
        let registry = default();
        let pointer_targets = default();
        ShapeViewModel { shape, events, registry, pointer_targets }
    }

    fn add_to_scene_layer(&self, scene: &Scene, layer: &scene::Layer) {
        let instance = layer.instantiate(scene, &self.shape);
        scene.shapes.insert_mouse_target(instance.global_instance_id, self.events.clone_ref());
        self.pointer_targets.borrow_mut().push(instance.global_instance_id);
        *self.registry.borrow_mut() = Some(scene.shapes.clone_ref());
    }
}

impl<S> ShapeViewModel<S> {
    fn unregister_existing_mouse_targets(&self) {
        if let Some(registry) = &*self.registry.borrow() {
            for global_instance_id in mem::take(&mut *self.pointer_targets.borrow_mut()) {
                registry.remove_mouse_target(global_instance_id);
            }
        }
    }
}

impl<T: display::Object> display::Object for ShapeViewModel<T> {
    fn display_object(&self) -> &display::object::Instance {
        self.shape.display_object()
    }
}

impl<T: display::Object> display::Object for ShapeView<T> {
    fn display_object(&self) -> &display::object::Instance {
        self.shape.display_object()
    }
}



// ==============
// === Widget ===
// ==============

// === WidgetData ===

// We use type bounds here, because Drop implementation requires them
#[derive(Debug)]
struct WidgetData<Model: 'static, Frp: 'static> {
    app:            Application,
    display_object: display::object::Instance,
    frp:            std::mem::ManuallyDrop<Frp>,
    model:          std::mem::ManuallyDrop<Rc<Model>>,
}

impl<Model: 'static, Frp: 'static> WidgetData<Model, Frp> {
    pub fn new(
        app: &Application,
        frp: Frp,
        model: Rc<Model>,
        display_object: display::object::Instance,
    ) -> Self {
        Self {
            app: app.clone_ref(),
            display_object,
            frp: std::mem::ManuallyDrop::new(frp),
            model: std::mem::ManuallyDrop::new(model),
        }
    }
}

impl<Model: 'static, Frp: 'static> Drop for WidgetData<Model, Frp> {
    fn drop(&mut self) {
        self.display_object.unset_parent();
        // Taking the value from `ManuallyDrop` requires us to not use it anymore.
        // This is clearly the case, because the structure will be soon dropped anyway.
        #[allow(unsafe_code)]
        unsafe {
            let frp = std::mem::ManuallyDrop::take(&mut self.frp);
            let model = std::mem::ManuallyDrop::take(&mut self.model);
            self.app.display.collect_garbage(frp);
            self.app.display.collect_garbage(model);
        }
    }
}


// === Widget ===

/// The EnsoGL widget abstraction.
///
/// The widget is a visible element with FRP logic. The structure contains a FRP network and a
/// model.
///
/// * The `Frp` structure should own `frp::Network` structure handling the widget's logic. It's
///   recommended to create one with [`crate::application::frp::define_endpoints`] macro.
/// * The `Model` is any structure containing shapes, properties, subwidgets etc.  manipulated by
///   the FRP network.
///
/// # Dropping Widget
///
/// Upon dropping Widget structure, the object will be hidden, but both FRP and Model will
/// not be dropped at once. Instead, they will be passed to the EnsoGL's garbage collector,
/// because handling all effects of hiding object and emitting appropriate events  (for example:
/// the "on_hide" event of [`core::gui::component::ShapeEvents`]) may take a several frames, and
/// we want to have FRP network alive to handle those effects. Thus, both FRP and model will
/// be dropped only after all process of object hiding will be handled.
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Widget<Model: 'static, Frp: 'static> {
    data: Rc<WidgetData<Model, Frp>>,
}

impl<Model: 'static, Frp: 'static> Deref for Widget<Model, Frp> {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.data.frp
    }
}

impl<Model: 'static, Frp: 'static> Widget<Model, Frp> {
    /// Create a new widget.
    pub fn new(
        app: &Application,
        frp: Frp,
        model: Rc<Model>,
        display_object: display::object::Instance,
    ) -> Self {
        Self { data: Rc::new(WidgetData::new(app, frp, model, display_object)) }
    }

    /// Get the FRP structure. It is also a result of deref-ing the widget.
    pub fn frp(&self) -> &Frp {
        &self.data.frp
    }

    /// Get the Model structure.
    pub fn model(&self) -> &Model {
        &self.data.model
    }

    /// Reference to the application the Widget belongs to. It's required for handling model and
    /// FRP garbage collection, but also may be helpful when, for example, implementing
    /// `application::View`.
    pub fn app(&self) -> &Application {
        &self.data.app
    }
}

impl<Model: 'static, Frp: 'static> display::Object for Widget<Model, Frp> {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        &self.data.display_object
    }
}
