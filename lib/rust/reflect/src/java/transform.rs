//! Transformations of Java datamodels.

use super::*;



// ========================
// === Optional to Null ===
// ========================

/// Rewrite the typegraph to eliminate `Optional` and instead allow some fields to be `null`.
///
/// `TypeId` validity:
/// `TypeId`s that referred to `Optional` types: No long resolvable after transformation.
/// All other `TypeId`s: Unaffected.
pub fn optional_to_null(mut graph: TypeGraph) -> TypeGraph {
    let mut optional_to_class = BTreeMap::new();
    for id in graph.type_ids() {
        let class = &graph[id];
        if class.builtin && class.name == "java.util.Optional" {
            let wrapped = class.params[0];
            optional_to_class.insert(id, wrapped);
        }
    }
    let no_multilevel = "Handling of multi-level nullability has not been implemented.";
    for class in optional_to_class.values() {
        assert!(!optional_to_class.contains_key(class), "{}", no_multilevel);
    }
    for class in graph.classes_mut() {
        for field in &mut class.fields {
            match &mut field.data {
                FieldData::Object { type_, nonnull } => {
                    if let Some(mapped) = optional_to_class.get(&type_) {
                        assert!(*nonnull, "{}", no_multilevel);
                        *nonnull = false;
                        *type_ = *mapped;
                    }
                }
                _ => (),
            }
        }
    }
    for &id in optional_to_class.keys() {
        graph.remove(id);
    }
    graph
}
