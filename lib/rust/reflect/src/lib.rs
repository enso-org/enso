#![feature(map_first_last)]
#![feature(associated_type_defaults)]
#![feature(option_get_or_insert_default)]

pub mod prelude {
    pub use crate as reflect;
    pub use enso_reflect_macros::Reflect;
}

pub mod generic;
pub mod java;
pub mod rust;

pub use rust::Reflect;

pub(crate) mod graphviz {
    pub(crate) struct Node {
        pub label:     String,
        pub node_type: NodeType,
        pub primitive: bool,
    }

    pub(crate) enum NodeType {
        Struct,
        Enum,
        Variant,
        AbstractStruct,
    }

    pub(crate) enum EdgeType {
        Variant,
        Field,
        OptionalField,
        Subtype,
    }

    #[derive(Default)]
    pub(crate) struct Graph {
        pub nodes: std::collections::HashMap<String, Node>,
        pub edges: Vec<(String, String, EdgeType)>,
    }

    const PRUNE_PRIMITIVE_LEAFS: bool = true;

    impl std::fmt::Display for Graph {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let primitive_attrs = vec![
                format!("style=filled"),
                format!("fillcolor={:?}", "#262626"), // something boring
                format!("fontcolor={:?}", "white"),
            ];
            let enum_attrs = vec![
                format!("style=filled"),
                format!("fillcolor={:?}", "#255C99"), // X
                format!("fontcolor={:?}", "white"),
            ];
            let variant_attrs = vec![
                format!("style=filled"),
                format!("fillcolor={:?}", "#7EA3CC"), // light X
                format!("shape=oval"),
            ];
            let struct_attrs = vec![
                format!("style=filled"),
                format!("fillcolor={:?}", "#B3001B"), // Y
                format!("fontcolor={:?}", "white"),
            ];
            let abstract_struct_attrs = vec![
                format!("style=filled"),
                format!("fillcolor={:?}", "#6D1321"), // dark Y
                format!("fontcolor={:?}", "white"),
            ];
            let variant_edge_attrs = vec![
                format!("weight=2"),
                format!("color={:?}", "#7EA3CC"), // light X (as above)
            ];
            let field_edge_attrs = vec![];
            let optional_field_edge_attrs = vec![format!("style=dotted")];
            let subtype_edge_attrs = vec![format!("arrowhead=dot")];
            writeln!(f, "digraph refs {{")?;
            let non_leafs: std::collections::HashSet<_> =
                self.edges.iter().map(|(x, _, _)| x).cloned().collect();
            let mut pruned = std::collections::HashSet::new();
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
}
