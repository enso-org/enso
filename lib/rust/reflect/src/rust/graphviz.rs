use super::*;
use crate::graphviz::EdgeType;
use crate::graphviz::Graph;
use crate::graphviz::Node;
use crate::graphviz::NodeType;



// =============
// === Graph ===
// =============

/// Generate a graph of the given type's relationships with other types.
pub fn graph<T: Reflect>() -> Graph {
    let mut to_visit = vec![T::reflect_lazy()];
    let mut types = std::collections::HashMap::new();
    while let Some(type_) = to_visit.pop() {
        let id = type_.id;
        if types.contains_key(&id) {
            continue;
        }
        let type_ = type_.evaluate();
        to_visit.extend(type_.referenced_types().into_iter());
        types.insert(id, type_);
    }
    let mut graph = Graph::default();
    let mut number = Number::default();
    for type_ in types.values() {
        let sname = format!("{}{}", type_.name, number.get(type_.id));
        let primitive = type_.is_primitive();
        let node_type = match type_.type_type() {
            TypeType::Sum => NodeType::Enum,
            TypeType::Product => NodeType::Struct,
        };
        let label = type_.name.clone();
        graph.nodes.insert(sname.clone(), Node { primitive, node_type, label });
        if let Data::Enum(enum_) = &type_.data {
            for variant in &enum_.variants {
                let svariant = format!("{}_{}", sname, variant.ident);
                let primitive = false;
                let node_type = NodeType::Variant;
                let label = variant.ident.clone();
                graph.nodes.insert(svariant.clone(), Node { primitive, node_type, label });
                graph.edges.push((sname.clone(), svariant.clone(), EdgeType::Variant));
                for ty in variant.fields.referenced_types() {
                    let ty = &types[&ty.id];
                    let sname2 = format!("{}{}", ty.name, number.get(ty.id));
                    graph.edges.push((svariant.clone(), sname2, EdgeType::Field));
                }
            }
        } else {
            for ty in type_.referenced_types() {
                let ty = &types[&ty.id];
                let sname2 = format!("{}{}", ty.name, number.get(ty.id));
                graph.edges.push((sname.clone(), sname2, EdgeType::Field));
            }
        }
    }
    graph
}

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
struct Number<T> {
    map:     std::collections::HashMap<T, u32>,
    next_id: u32,
}

impl<T: Eq + std::hash::Hash> Number<T> {
    fn get(&mut self, index: T) -> u32 {
        *self.map.entry(index).or_insert_with(|| {
            let id = self.next_id;
            self.next_id += 1;
            id
        })
    }
}
