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
pub fn graph(java: &TypeGraph) -> Graph {
    let mut graph = Graph::default();
    let classes = &java.classes;
    for (id, ty) in classes.iter() {
        let sname = format!("{}{}", ty.name, id);
        let node_type = match &ty.abstract_ {
            true => NodeType::AbstractStruct,
            false => NodeType::Struct,
        };
        let label = ty.name.clone();
        let primitive = ty.builtin;
        graph.nodes.insert(sname.clone(), Node { primitive, node_type, label });
        if let Some(&parent) = ty.parent.as_ref() {
            let sparent = format!("{}{}", classes[id].name, parent);
            graph.edges.push((sparent.clone(), sname.clone(), EdgeType::Subtype));
        }
        for field in &ty.fields {
            match &field.data {
                FieldData::Object { type_, non_null } => {
                    let sname2 = format!("{}{}", classes[id].name, type_);
                    let edgetype = match non_null {
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
