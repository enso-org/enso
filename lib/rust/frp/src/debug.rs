//! This module defines FRP Graphviz bindings. It allows visualizing the FRP network as Graphviz
//! diagram.
//! WARNING
//! THIS MODULE IS IN A VERY WORK-IN-PROGRESS STATE. IT WILL BE CHANGED SOON.

// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]
#![allow(dead_code)]

use crate::prelude::*;



//use crate::DataType;
//


// ================
// === Graphviz ===
// ================

/// Visualization data for a nodes.
#[derive(Debug, Clone)]
pub struct VizNode {
    variant: String,
    label:   String,
}

impl VizNode {
    /// Constructor
    pub fn new(variant: String, label: String) -> Self {
        VizNode { variant, label }
    }
}


/// Visualization data for a link between nodes.
#[derive(Debug, Clone)]
pub struct VizLink {
    source_display_id: usize,
    target_display_id: usize,
    //    message_type      : DataType,
    data_type:         String,
}

impl VizLink {
    /// Constructor.
    pub fn new(source_display_id: usize, target_display_id: usize, data_type: String) -> Self {
        Self { source_display_id, target_display_id, data_type }
    }
}


/// Graphviz FRP system visualizer.
#[derive(Debug, Default)]
pub struct Graphviz {
    nodes:  HashMap<usize, VizNode>,
    labels: HashMap<usize, String>,
    links:  Vec<VizLink>,
}

impl Graphviz {
    /// Defines a new node.
    pub fn add_node<Tp: Str, Label: Str>(&mut self, id: usize, tp: Tp, label: Label) {
        let tp = tp.into();
        let label = label.into();
        self.nodes.insert(id, VizNode::new(tp, label.clone()));
        self.labels.insert(id, label);
    }
    //
    //    /// Defines a new link between nodes.
    //    pub fn add_link<S:Str>
    //    (&mut self, source:usize, target:usize, message_type:DataType, data_type:S) {
    //        let link = VizLink::new(source,target,message_type,data_type.into());
    //        self.links.push(link);
    //    }
    //
    //    /// Checks if a node with the given id is already registered in the node map.
    //    pub fn contains(&mut self, id:usize) -> bool {
    //        self.nodes.contains_key(&id)
    //    }
    //
    //    /// Takes a set of nodes and outputs a map from `display_id` to a particular node. In case
    // the    /// `display_id` points to several nodes, the node types `Hold` and `Recursive`
    // has weaker    /// preference.
    //    fn create_node_map(&self) -> HashMap<usize,VizNode> {
    //        let mut node_map : HashMap<usize,VizNode> = default();
    //        for node in self.nodes.values() {
    //            let entry        = node_map.entry(node.display_id);
    //            let merged_entry = entry.and_modify(|node2|{
    //                let variant = &node2.variant;
    //                if variant == "Hold" || variant == "Recursive" {
    //                    *node2 = node.clone();
    //                }
    //            });
    //            merged_entry.or_insert_with(|| node.clone());
    //        }
    //        node_map
    //    }
    //
    //    /// Outputs a Graphviz Dot code.
    pub fn to_code(&self) -> String {
        let mut code = String::default();
        //        let node_map = self.create_node_map();
        //
        for (idx, node) in self.nodes.iter() {
            let color = match node.variant.as_str() {
                "Toggle" => "534666",
                "Gate" => "e69d45",
                "Hold" => "308695",
                "Lambda" => "d45769",
                "Lambda2" => "d45769",
                _ => "455054",
            };
            let fill = format!("[fillcolor=\"#{color}\"]");
            let spacing = "<br/><FONT POINT-SIZE=\"5\"> </FONT><br/>";
            let variant = format!("<FONT POINT-SIZE=\"9\">{}</FONT>", node.variant);
            let label = format!("[label=< {} {spacing} {variant} >]", node.label);
            let line = format!("\n{idx} {fill} {label}");
            code.push_str(&line);
        }
        //
        //        for link in &self.links {
        //            let source    = &link.source_display_id;
        //            let target    = &link.target_display_id;
        //            let data_type = &link.data_type;
        //            let not_loop  = source != target;
        //            if not_loop {
        //                let style = match link.message_type {
        //                    DataType::Behavior => "[style=\"dashed\"]",
        //                    _ => ""
        //                };
        //                let label = if data_type == "()" { "" } else { &data_type };
        //                let label = format!("[label=\"  {label}\"]");
        //                let line  = format!("\n{source} -> {target} {style} {label}");
        //                code.push_str(&line);
        //            }
        //        }
        //
        let fonts = "[fontname=\"Helvetica Neue\" fontsize=11]";
        let node_shape = "[shape=box penwidth=0 margin=0.12 style=\"rounded,filled\"]";
        let node_style = "[fontcolor=white fillcolor=\"#5397dc\"]";
        let edge_style = "[arrowsize=.7 fontcolor=\"#555555\"]";
        let graph_cfg = format!("rankdir=TD; graph {fonts};");
        let nodes_cfg = format!("node {fonts} {node_shape} {node_style};");
        let edges_cfg = format!("edge {fonts} {edge_style};");
        format!("digraph G {{ \n{graph_cfg} \n{nodes_cfg} \n{edges_cfg} \n{code} \n}}")
    }
}

impl From<Graphviz> for String {
    fn from(cfg: Graphviz) -> String {
        cfg.to_code()
    }
}



// =======================
// === GraphvizBuilder ===
// =======================

/// Trait for every node which can be visualized.
pub trait GraphvizBuilder {
    /// Adds the current object to the builder.
    fn graphviz_build(&self, builder: &mut Graphviz);

    /// Converts the current object to Graphviz Dot syntax.
    fn to_graphviz(&self) -> String {
        let mut builder = Graphviz::default();
        self.graphviz_build(&mut builder);
        builder.into()
    }

    /// Converts the current object to Graphviz and displays it in a new tab in a web browser.
    fn display_graphviz(&self) {
        let code = self.to_graphviz();
        let url = percent_encoding::utf8_percent_encode(&code, percent_encoding::NON_ALPHANUMERIC);
        let url = format!("https://dreampuf.github.io/GraphvizOnline/#{}", url);
        crate::web::window.open_with_url_and_target(&url, "_blank").unwrap();
    }
}

pub fn display_graphviz(viz: Graphviz) {
    let code: String = viz.into();
    let url = percent_encoding::utf8_percent_encode(&code, percent_encoding::NON_ALPHANUMERIC);
    let url = format!("https://dreampuf.github.io/GraphvizOnline/#{}", url);
    crate::web::window.open_with_url_and_target(&url, "_blank").unwrap();
}


impl<T> GraphvizBuilder for T
where
    T: ContentRef,
    Content<T>: GraphvizBuilder,
{
    default fn graphviz_build(&self, builder: &mut Graphviz) {
        self.content().graphviz_build(builder)
    }
}
