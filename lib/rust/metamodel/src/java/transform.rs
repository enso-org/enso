//! Transformations of Java datamodels.

use crate::java::*;



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
    for (id, class) in graph.classes.iter() {
        if class.builtin && class.name == OPTIONAL {
            let wrapped = class.params[0];
            optional_to_class.insert(id, wrapped);
        }
    }
    let no_multilevel = "Handling of multi-level nullability has not been implemented.";
    for class in optional_to_class.values() {
        assert!(!optional_to_class.contains_key(class), "{}", no_multilevel);
    }
    for class in graph.classes.values_mut() {
        for field in &mut class.fields {
            if let FieldData::Object { type_, non_null } = &mut field.data {
                if let Some(mapped) = optional_to_class.get(type_) {
                    assert!(*non_null, "{}", no_multilevel);
                    *non_null = false;
                    *type_ = *mapped;
                }
            }
        }
    }
    for &id in optional_to_class.keys() {
        graph.classes.remove(id);
    }
    graph
}
