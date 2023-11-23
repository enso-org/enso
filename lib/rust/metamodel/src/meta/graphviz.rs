//! Graphical representation of a `TypeGraph` with GraphViz.

use super::*;

use crate::graphviz::EdgeType;
use crate::graphviz::Graph;
use crate::graphviz::Node;
use crate::graphviz::NodeType;



// =============
// === Graph ===
// =============

/// Produce a GraphViz graph representation of the relationships between the types.
pub fn graph(typegraph: &TypeGraph) -> Graph {
    let mut graph = Graph::default();
    let types = &typegraph.types;
    for (id, ty) in types.iter() {
        let sname = format!("{}{}", ty.name, id);
        let node_type = match &ty.data {
            Data::Struct(_) if ty.abstract_ => NodeType::AbstractStruct,
            Data::Struct(_) if ty.abstract_ && ty.closed => NodeType::Enum,
            Data::Struct(_) => NodeType::Struct,
            Data::Primitive(_) => NodeType::Struct,
        };
        let primitive = matches!(&ty.data, Data::Primitive(_));
        let label = ty.name.to_string();
        graph.nodes.insert(sname.clone(), Node { primitive, node_type, label });
        if let Some(id) = ty.parent.as_ref() {
            let sparent = format!("{}{}", types[id].name, id);
            graph.edges.push((sparent.clone(), sname.clone(), EdgeType::Subtype));
        }
        match &ty.data {
            Data::Struct(fields) =>
                for Field { type_, name: _, hide: _, id: _ } in fields {
                    let sname2 = format!("{}{}", types[type_].name, type_);
                    graph.edges.push((sname.clone(), sname2, EdgeType::Field));
                },
            Data::Primitive(Primitive::U32)
            | Data::Primitive(Primitive::U64)
            | Data::Primitive(Primitive::I32)
            | Data::Primitive(Primitive::I64)
            | Data::Primitive(Primitive::Bool)
            | Data::Primitive(Primitive::Char)
            | Data::Primitive(Primitive::String) => {}
            Data::Primitive(Primitive::Sequence(t0)) => graph.edges.push((
                sname.clone(),
                format!("{}{}", types[t0].name, t0),
                EdgeType::Field,
            )),
            Data::Primitive(Primitive::Option(t0)) => graph.edges.push((
                sname.clone(),
                format!("{}{}", types[t0].name, t0),
                EdgeType::Field,
            )),
            Data::Primitive(Primitive::Result(t0, t1)) => {
                graph.edges.push((
                    sname.clone(),
                    format!("{}{}", types[t0].name, t0),
                    EdgeType::Field,
                ));
                graph.edges.push((
                    sname.clone(),
                    format!("{}{}", types[t1].name, t1),
                    EdgeType::Field,
                ));
            }
        }
    }
    graph
}
