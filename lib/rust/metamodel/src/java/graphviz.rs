//! Generating graphical representations of Java type systems.

use super::*;
use crate::graphviz::EdgeType;
use crate::graphviz::Graph;
use crate::graphviz::Node;
use crate::graphviz::NodeType;



// =========================
// === Graphviz Graphing ===
// =========================

/// Produce a graphviz graph of the datatypes.
pub fn graph(typegraph: &TypeGraph) -> Graph {
    let mut graph = Graph::default();
    let types = &typegraph.types;
    for (i, ty) in types.iter().enumerate() {
        let ty = match ty.as_ref() {
            Some(ty) => ty,
            None => continue,
        };
        let sname = format!("{}{}", ty.name, i);
        let node_type = match &ty.abstract_ {
            true => NodeType::AbstractStruct,
            false => NodeType::Struct,
        };
        let label = ty.name.clone();
        let primitive = ty.builtin;
        graph.nodes.insert(sname.clone(), Node { primitive, node_type, label });
        if let Some(&parent) = ty.parent.as_ref() {
            let id = parent.0;
            let sparent = format!("{}{}", types[id].as_ref().unwrap().name, id);
            graph.edges.push((sparent.clone(), sname.clone(), EdgeType::Subtype));
        }
        for field in &ty.fields {
            match &field.data {
                FieldData::Object { type_, nonnull } => {
                    let id = type_.0;
                    let sname2 = format!("{}{}", types[id].as_ref().unwrap().name, id);
                    let edgetype = match nonnull {
                        false => EdgeType::OptionalField,
                        true => EdgeType::Field,
                    };
                    graph.edges.push((sname.clone(), sname2, edgetype));
                }
                FieldData::Primitive(_) => {}
            }
        }
    }
    graph
}
