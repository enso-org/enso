//! The `Registry` provides a mechanism to store `visualization::Class`es for all available visualizations. It
//! provides functionality to register new factories, as well as get suitable factories for
//! a specific data type.

use crate::prelude::*;

use crate::builtin;
use crate::component::visualization;
use crate::data::EnsoType;

use ensogl::display::scene::Scene;
use enso_prelude::CloneRef;



// ================
// === Registry ===
// ================

/// The registry struct. For more information see the module description.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Registry {
    path_map : Rc<RefCell<HashMap<visualization::Path,visualization::Definition>>>,
    type_map : Rc<RefCell<HashMap<EnsoType,Vec<visualization::Definition>>>>,
    logger   : Logger,
}

impl Registry {
    /// Constructor.
    pub fn new() -> Self {
        let path_map = default();
        let type_map = default();
        let logger   = Logger::new("Registry");
        Registry{path_map,type_map,logger}
    }

    /// Return a `Registry` pre-populated with default visualizations.
    pub fn with_default_visualizations() -> Self {
        let registry = Self::new();
        registry.add(builtin::visualization::native::BubbleChart::definition());
        registry.add(builtin::visualization::native::RawText::definition());
        registry.try_add_java_script(builtin::visualization::java_script::bubble_visualization());
        registry.try_add_java_script(builtin::visualization::java_script::scatter_plot_visualization());
        registry.try_add_java_script(builtin::visualization::java_script::table_view_visualization());
        registry.try_add_java_script(builtin::visualization::java_script::map_view_visualization());
        registry
    }

    /// Register a new `visualization::Definition`.
    pub fn add(&self, class:impl Into<visualization::Definition>) {
        let class = class.into();
        let sig   = &class.signature;
        self.type_map.borrow_mut().entry(sig.input_type.clone()).or_default().push(class.clone_ref());
        self.path_map.borrow_mut().entry(sig.path.clone()).insert(class);
    }

    /// Register a new `visualization::java_script::Definition`. If creating the class fails, it
    /// will not be added an warning is emitted.
    pub fn try_add_java_script(&self, class:impl Into<visualization::java_script::FallibleDefinition>) {
        let class = class.into();
        match class {
            Ok(class) => {self.add(class) },
            Err(err)    => {
                warning!(&self.logger,"Failed to add visualization class to registry due to error: \
                                       {err}")
            },
        };
    }

    /// Return all `visualization::Class`es that can create a visualization for the given datatype.
    pub fn valid_sources(&self, tp:&EnsoType) -> Vec<visualization::Definition>{
        let type_map = self.type_map.borrow();
        type_map.get(tp).cloned().unwrap_or_default()
    }

    /// Return the `visualization::Definition` registered for the given `visualization::Path`.
    pub fn definition_from_path(&self, path:&visualization::Path)
                                -> Option<visualization::Definition> {
        self.path_map.borrow().get(path).cloned()
    }

    /// Return a default visualisation class.
    pub fn default_visualisation(scene:&Scene) -> visualization::Instance {
        let instance = builtin::visualization::native::RawText::new(scene);
        instance.into()
    }

}

impl Default for Registry {
    fn default() -> Self {
        Registry::new()
    }
}
