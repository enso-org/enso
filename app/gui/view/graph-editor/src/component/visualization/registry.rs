//! The `Registry` provides a mechanism to store `visualization::Class`es for all available
//! visualizations. It provides functionality to register new factories, as well as get suitable
//! factories for a specific data type.

use crate::prelude::*;

use crate::builtin;
use crate::component::visualization;
use crate::data::enso;

use enso_prelude::CloneRef;



// ================
// === Registry ===
// ================

/// The registry struct. For more information see the module description.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Registry {
    path_map: Rc<RefCell<HashMap<visualization::Path, visualization::Definition>>>,
    type_map: Rc<RefCell<HashMap<enso::Type, Vec<visualization::Definition>>>>,
    logger:   Logger,
}

impl Registry {
    /// Constructor.
    pub fn new() -> Self {
        let path_map = default();
        let type_map = default();
        let logger = Logger::new("Registry");
        Registry { path_map, type_map, logger }
    }

    /// Return a `Registry` pre-populated with default visualizations.
    pub fn with_default_visualizations() -> Self {
        let registry = Self::new();
        registry.add_default_visualizations();
        registry
    }

    /// Register a new `visualization::Definition`.
    pub fn add(&self, class: impl Into<visualization::Definition>) {
        let class = class.into();
        let sig = &class.signature;
        for tp in sig.input_type.alternatives() {
            self.type_map.borrow_mut().entry(tp).or_default().push(class.clone_ref());
        }
        self.path_map.borrow_mut().entry(sig.path.clone()).insert_entry(class);
    }

    /// Register a new `visualization::java_script::Definition`. If creating the class fails, it
    /// will not be added an warning is emitted.
    pub fn try_add_java_script(
        &self,
        class: impl Into<visualization::java_script::FallibleDefinition>,
    ) {
        let class = class.into();
        match class {
            Ok(class) => self.add(class),
            Err(err) => {
                warning!(
                    &self.logger,
                    "Failed to add visualization class to registry due to error: \
                                       {err}"
                )
            }
        };
    }

    /// Return all `visualization::Class`es that can create a visualization for the given datatype.
    pub fn valid_sources(&self, tp: &enso::Type) -> Vec<visualization::Definition> {
        let type_map = self.type_map.borrow();
        let any_type = enso::Type::any();
        let mut result: Vec<visualization::Definition> =
            type_map.get(tp).cloned().unwrap_or_default();
        if tp != &any_type {
            if let Some(vis_for_any) = type_map.get(&any_type) {
                result.extend(vis_for_any.iter().cloned());
            }
        }
        result
    }

    /// Return the `visualization::Definition` registered for the given `visualization::Path`.
    pub fn definition_from_path(
        &self,
        path: &visualization::Path,
    ) -> Option<visualization::Definition> {
        self.path_map.borrow().get(path).cloned()
    }

    /// Remove all visualizations from registry
    pub fn remove_all_visualizations(&self) {
        self.path_map.borrow_mut().clear();
        self.type_map.borrow_mut().clear();
    }

    /// Add default visualizations to the registry.
    pub fn add_default_visualizations(&self) {
        self.add(builtin::visualization::native::RawText::definition());
        self.try_add_java_script(builtin::visualization::java_script::scatter_plot_visualization());
        self.try_add_java_script(builtin::visualization::java_script::histogram_visualization());
        self.try_add_java_script(builtin::visualization::java_script::heatmap_visualization());
        self.try_add_java_script(builtin::visualization::java_script::table_visualization());
        self.try_add_java_script(builtin::visualization::java_script::sql_visualization());
        self.try_add_java_script(builtin::visualization::java_script::geo_map_visualization());
        self.try_add_java_script(builtin::visualization::java_script::image_base64_visualization());
        self.try_add_java_script(builtin::visualization::java_script::warnings_visualization());
    }

    /// Return a default visualisation definition.
    pub fn default_visualisation() -> visualization::Definition {
        builtin::visualization::native::RawText::definition()
    }
}

impl Default for Registry {
    fn default() -> Self {
        Registry::new()
    }
}
