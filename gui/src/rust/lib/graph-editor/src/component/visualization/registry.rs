//! The `Registry` provides a mechanism to store `visualization::Class`es for all available visualizations. It
//! provides functionality to register new factories, as well as get suitable factories for
//! a specific data type.
//!
//! Example
//! --------
//! ```no_run
//! use graph_editor::component::visualization::Registry;
//! use graph_editor::component::visualization::EnsoType;
//! use graph_editor::component::visualization::JsSourceClass;
//!
//! // Instantiate a pre-populated registry.
//! let registry = Registry::with_default_visualizations();
//! // Add a new class that creates visualizations defined in JS.
//! registry.register_class(JsSourceClass::from_js_source_raw(r#"
//!     class BubbleVisualization {
//!         static inputTypes = ["[[Float,Float,Float]]"]
//!         onDataReceived(root, data) {}
//!         setSize(root, size) {}
//!     }
//!     return BubbleVisualization;
//! "#.into()).unwrap());
//!
//! // Get all factories that can render  visualization for the type `[[Float,Float,Float]]`.
//! let target_type:EnsoType = "[[Float,Float,Float]]".to_string().into();
//! assert!(registry.valid_sources(&target_type).len() > 0);
//! ```

use crate::prelude::*;

use crate::component::visualization::*;
use crate::component::visualization::renderer::example::js::get_bubble_vis_class;
use crate::component::visualization::renderer::example::native::BubbleChart;

use ensogl::display::scene::Scene;



// ==============================
// === Visualization Registry ===
// ==============================

/// HashMap that contains the mapping from `EnsoType`s to a `Vec` of `Factories. This is meant to
/// map a `EnsoType` to all `visualization::Class`es that support visualising that type.
type RegistryTypeMap = HashMap<EnsoType, Vec<Rc<dyn Class>>>;

/// The registry struct. For more information see the module description.
#[derive(Clone,CloneRef,Default,Debug)]
#[allow(missing_docs)]
pub struct Registry {
    entries : Rc<RefCell<RegistryTypeMap>>,
}

impl Registry {
    /// Return an empty `Registry`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Return a `Registry` prepopulated with default visualizations.
    pub fn with_default_visualizations() -> Self {
        let registry = Self::new();
        // FIXME use proper enso types here.
        registry.register_class(NativeConstructorClass::new(
            Signature {
                name        : "Bubble Visualization (native)".to_string(),
                input_types : vec!["[[Float,Float,Float]]".to_string().into()],
            },
            |scene:&Scene| Ok(Visualization::new(BubbleChart::new(scene)))
        ));
        registry.register_class(get_bubble_vis_class());

        registry
    }

    /// Register a new visualization class with the registry.
    pub fn register_class<T:Class+'static>(&self, class:T) {
        self.register_class_rc(Rc::new(class));
    }

    /// Register a new visualization class that's pre-wrapped in an `Rc` with the registry.
    pub fn register_class_from_handle(&self, handle:&Handle) {
        if let Some(class) = handle.class() {
            self.register_class_rc(class);
        }
    }

    fn register_class_rc(&self, class:Rc<dyn Class>) {
        let spec = class.signature();
        for dtype in &spec.input_types {
            let mut entries = self.entries.borrow_mut();
            let entry_vec = entries.entry(dtype.clone()).or_insert_with(default);
            entry_vec.push(Rc::clone(&class));
        }
    }

    /// Return all `visualization::Class`es that can create a visualization for the given datatype.
    pub fn valid_sources(&self, dtype:&EnsoType) -> Vec<Rc<dyn Class>>{
        let entries       = self.entries.borrow();
        entries.get(dtype).cloned().unwrap_or_else(default)
    }
}
