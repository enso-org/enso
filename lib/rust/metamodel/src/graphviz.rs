//! Rendering graphical representations of data models with GraphViz.

use std::collections::BTreeSet;



/// Hide data fields that don't reference any types outside the builtin set.
const PRUNE_PRIMITIVE_LEAFS: bool = true;



// =============
// === Graph ===
// =============

/// A GraphViz graph of relationships between types.
#[derive(Default, Debug)]
pub struct Graph {
    pub(crate) nodes: std::collections::HashMap<String, Node>,
    pub(crate) edges: Vec<(String, String, EdgeType)>,
}

#[derive(Debug)]
pub(crate) struct Node {
    pub label:     String,
    pub node_type: NodeType,
    pub primitive: bool,
}

#[derive(Debug)]
pub(crate) enum NodeType {
    Struct,
    Enum,
    Variant,
    AbstractStruct,
}

#[derive(Debug)]
pub(crate) enum EdgeType {
    Variant,
    Field,
    OptionalField,
    Subtype,
}

impl std::fmt::Display for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let variant_color = "#7EA3CC";
        let primitive_attrs = vec![
            format!("style=filled"),
            format!("fillcolor={:?}", "#262626"),
            format!("fontcolor={:?}", "white"),
        ];
        let enum_attrs = vec![
            format!("style=filled"),
            format!("fillcolor={:?}", "#255C99"),
            format!("fontcolor={:?}", "white"),
        ];
        let variant_attrs = vec![
            format!("style=filled"),
            format!("fillcolor={:?}", variant_color),
            format!("shape=oval"),
        ];
        let struct_attrs = vec![
            format!("style=filled"),
            format!("fillcolor={:?}", "#B3001B"),
            format!("fontcolor={:?}", "white"),
        ];
        let abstract_struct_attrs = vec![
            format!("style=filled"),
            format!("fillcolor={:?}", "#6D1321"),
            format!("fontcolor={:?}", "white"),
        ];
        let variant_edge_attrs = vec![format!("color={:?}", variant_color)];
        let field_edge_attrs = vec![];
        let optional_field_edge_attrs = vec![format!("style=dashed")];
        let subtype_edge_attrs = vec![format!("arrowhead=dot")];
        writeln!(f, "digraph refs {{")?;
        let non_leafs: BTreeSet<_> = self.edges.iter().map(|(x, _, _)| x).cloned().collect();
        let mut pruned = BTreeSet::new();
        for (id, node) in &self.nodes {
            let mut attrs;
            if node.primitive {
                if PRUNE_PRIMITIVE_LEAFS && !non_leafs.contains(id) {
                    pruned.insert(id.clone());
                    continue;
                }
                attrs = primitive_attrs.clone();
            } else {
                match node.node_type {
                    NodeType::Struct => attrs = struct_attrs.clone(),
                    NodeType::Enum => attrs = enum_attrs.clone(),
                    NodeType::Variant => attrs = variant_attrs.clone(),
                    NodeType::AbstractStruct => attrs = abstract_struct_attrs.clone(),
                }
            }
            attrs.push(format!("label={:?}", node.label));
            let shape = match node.node_type {
                NodeType::Enum => "diamond",
                NodeType::Variant => "oval",
                NodeType::Struct => "box",
                NodeType::AbstractStruct => "diamond",
            };
            attrs.push(format!("shape={}", shape));
            writeln!(f, "{:?} [{}];", id, attrs.join(","))?;
        }
        for (x, y, edgetype) in &self.edges {
            if pruned.contains(x) || pruned.contains(y) {
                continue;
            }
            let attrs = match edgetype {
                EdgeType::Variant => &variant_edge_attrs,
                EdgeType::Field => &field_edge_attrs,
                EdgeType::OptionalField => &optional_field_edge_attrs,
                EdgeType::Subtype => &subtype_edge_attrs,
            };
            writeln!(f, "{:?} -> {:?} [{}];", x, y, attrs.join(","))?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
