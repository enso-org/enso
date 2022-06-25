use super::*;
use crate::graphviz::EdgeType;
use crate::graphviz::Graph;
use crate::graphviz::Node;
use crate::graphviz::NodeType;



// =============
// === Graph ===
// =============

pub fn graph(typegraph: &TypeGraph) -> Graph {
    let mut graph = Graph::default();
    let types = &typegraph.types;
    for (i, ty) in types.iter().enumerate() {
        let ty = match ty.as_ref() {
            Some(ty) => ty,
            None => continue,
        };
        let sname = format!("{}{}", ty.name, i);
        let node_type = match &ty.data {
            Data::Struct(_) if ty.abstract_ => NodeType::AbstractStruct,
            Data::Struct(_) if ty.abstract_ && ty.closed => NodeType::Enum,
            Data::Struct(_) => NodeType::Struct,
            Data::Primitive(_) => NodeType::Struct,
        };
        let primitive = matches!(&ty.data, Data::Primitive(_));
        let label = ty.name.to_string();
        graph.nodes.insert(sname.clone(), Node { primitive, node_type, label });
        let parentlike = ty.parent.iter().chain(&ty.mixins).chain(&ty.weak_interfaces);
        for parent in parentlike {
            let id = parent.0;
            let sparent = format!("{}{}", types[id].as_ref().unwrap().name, id);
            graph.edges.push((sparent.clone(), sname.clone(), EdgeType::Subtype));
        }
        match &ty.data {
            Data::Struct(fields) =>
                for Field { type_, name: _, hide: _, id: _ } in fields {
                    let id = type_.0;
                    let sname2 = format!("{}{}", types[id].as_ref().unwrap().name, id);
                    graph.edges.push((sname.clone(), sname2, EdgeType::Field));
                },
            Data::Primitive(Primitive::U32)
            | Data::Primitive(Primitive::Bool)
            | Data::Primitive(Primitive::Usize)
            | Data::Primitive(Primitive::String) => {}
            Data::Primitive(Primitive::Sequence(TypeId(t0))) => graph.edges.push((
                sname.clone(),
                format!("{}{}", types[*t0].as_ref().unwrap().name, t0),
                EdgeType::Field,
            )),
            Data::Primitive(Primitive::Option(TypeId(t0))) => graph.edges.push((
                sname.clone(),
                format!("{}{}", types[*t0].as_ref().unwrap().name, t0),
                EdgeType::Field,
            )),
            Data::Primitive(Primitive::Result(TypeId(t0), TypeId(t1))) => {
                graph.edges.push((
                    sname.clone(),
                    format!("{}{}", types[*t0].as_ref().unwrap().name, t0),
                    EdgeType::Field,
                ));
                graph.edges.push((
                    sname.clone(),
                    format!("{}{}", types[*t1].as_ref().unwrap().name, t1),
                    EdgeType::Field,
                ));
            }
        }
    }
    graph
}
