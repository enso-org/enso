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
        registry
    }

    // FIXME: what does "pre-wrapped in an Rc" mean?
    /// Register a new visualization class that's pre-wrapped in an `Rc` with the registry.
    pub fn add(&self, class:impl Into<visualization::Definition>) {
        let class = class.into();
        let sig   = &class.signature;
        self.type_map.borrow_mut().entry(sig.input_type.clone()).or_default().push(class.clone_ref());
        self.path_map.borrow_mut().entry(sig.path.clone()).insert(class);
    }

    // FIXME: what does "pre-wrapped in an Rc" mean?
    /// Register a new visualization class that's pre-wrapped in an `Rc` with the registry.
    // FIXME: please explain the comment below. Is it worth doing now?
    /// TODO: Consider generalising the FallibleDefinition.
    pub fn try_add_java_script(&self, class:impl Into<visualization::java_script::FallibleDefinition>) {
        let class = class.into();
        match class {
            Ok(class) => {self.add(class) },
            Err(e)    => {
                self.logger.warning(|| format!("Failed to add visualization class to registry due \
                                                to error: {:?}", e))
            },
        };
    }

    /// Return all `visualization::Class`es that can create a visualization for the given datatype.
    pub fn valid_sources(&self, tp:&EnsoType) -> Vec<visualization::Definition>{
        let type_map = self.type_map.borrow();
        type_map.get(tp).cloned().unwrap_or_default()
    }

    /// Return a default visualisation class.
    pub fn default_visualisation(scene:&Scene) -> visualization::Instance {
        let instance =  builtin::visualization::native::RawText::new(scene);
        instance.into()
    }
}

impl Default for Registry {
    fn default() -> Self {
        Registry::new()
    }
}
