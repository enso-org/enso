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
}

impl Registry {
    /// Constructor.
    pub fn new() -> Self {
        let path_map = default();
        let type_map = default();
        Registry { path_map, type_map }
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
            Err(err) => warn!("Failed to add visualization class to registry due to error: {err}"),
        };
    }

    /// Return all `visualization::Class`es that can create a visualization for the given datatype.
    pub fn valid_sources(&self, tp: &enso::Type) -> Vec<visualization::Definition> {
        let type_map = self.type_map.borrow();
        let any_type = enso::Type::any();
        // IndexSet preserves insertion order.
        let mut result: indexmap::IndexSet<visualization::Definition> = default();
        result.extend(type_map.get(tp).cloned().unwrap_or_default());
        if tp != &any_type {
            if let Some(vis_for_any) = type_map.get(&any_type) {
                result.extend(vis_for_any.iter().cloned());
            }
        }
        result.into_iter().collect()
    }

    /// Return the `visualization::Definition` that should be used as default for the given type.
    pub fn default_visualization_for_type(
        &self,
        tp: &enso::Type,
    ) -> Option<visualization::Definition> {
        // TODO[MM]: Visualizations are order by "matching the type" first, followed by and then
        // "matching any type". So we just take the first one, which should be the most appropriate
        // one. This should be replaced with the proper solution described in
        // https://github.com/enso-org/enso/issues/5195
        let valid_sources = self.valid_sources(tp);
        valid_sources.into_iter().next()
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
        // Note that the order is important. Visualizations that are added first will be
        // prioritised as default (as long as they have a matching type to the value they will
        // represent.
        self.add(builtin::visualization::native::text_visualization::text_visualization());
        self.try_add_java_script(builtin::visualization::java_script::table_visualization());
        self.try_add_java_script(builtin::visualization::java_script::scatter_plot_visualization());
        self.try_add_java_script(builtin::visualization::java_script::histogram_visualization());
        self.try_add_java_script(builtin::visualization::java_script::heatmap_visualization());
        self.try_add_java_script(builtin::visualization::java_script::sql_visualization());
        self.try_add_java_script(builtin::visualization::java_script::geo_map_visualization());
        self.try_add_java_script(builtin::visualization::java_script::image_base64_visualization());
        self.try_add_java_script(builtin::visualization::java_script::warnings_visualization());
    }

    /// Return a default visualization definition.
    pub fn default_visualization() -> visualization::Definition {
        builtin::visualization::native::text_visualization::text_visualization()
    }
}

impl Default for Registry {
    fn default() -> Self {
        Registry::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use wasm_bindgen_test::wasm_bindgen_test;

    fn assert_no_duplicates<T: Eq + Hash + Debug + Clone>(items: &[T]) {
        let vec_length = items.len();
        let set: HashSet<T> = items.iter().cloned().collect();
        assert_eq!(set.len(), vec_length, "Duplicate entries found: {items:?}");
    }

    #[wasm_bindgen_test]
    fn assert_no_duplicate_default_visualizations() {
        let registry = Registry::new();
        registry.add_default_visualizations();

        for entry in registry.type_map.borrow().values() {
            let signatures = entry.iter().map(|class| class.signature.clone()).collect_vec();
            assert_no_duplicates(&signatures);
        }
    }
}
