#![allow(missing_docs)]

//! NOTE
//! This file is under a heavy development. It contains commented lines of code and some code may
//! be of poor quality. Expect drastic changes.

#![feature(associated_type_defaults)]
#![feature(clamp)]
#![feature(drain_filter)]
#![feature(overlapping_marker_traits)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(weak_into_raw)]
#![feature(fn_traits)]

#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

#![recursion_limit="512"]


#[warn(missing_docs)]
pub mod component;

/// Common types and functions usable in all modules of this crate.
pub mod prelude {
    pub use ensogl::prelude::*;
}

use ensogl::application;
use ensogl::prelude::*;
use ensogl::traits::*;

use crate::component::cursor::Cursor;
use crate::component::node;
use crate::component::node::Node as NodeView;
use crate::component::node::WeakNode as WeakNodeView;
use crate::component::connection::Connection as EdgeView;
use enso_frp as frp;
use enso_frp::io::keyboard;
use enso_frp::Position;
use ensogl::display::object::Id;
use ensogl::display::world::*;
use ensogl::display;
use ensogl::system::web::StyleSetter;
use ensogl::system::web;
use nalgebra::Vector2;
use ensogl::display::Scene;
use crate::component::node::port::Expression;
use crate::component::visualization::Visualization;
use crate::component::visualization;
use crate::component::visualization::example::js::constructor_sample_js_bubble_chart;
use crate::component::visualization::MockDataGenerator3D;
use crate::component::visualization::example::native;


// =====================
// === SharedHashSet ===
// =====================

#[derive(Derivative,CloneRef)]
#[derivative(Debug(bound="T:Eq+Hash+Debug, S:std::hash::BuildHasher"))]
pub struct SharedHashSet<T,S=std::collections::hash_map::RandomState> {
    pub raw : Rc<RefCell<HashSet<T,S>>>
}

impl<T,S> Clone for SharedHashSet<T,S> {
    fn clone(&self) -> Self {
        let raw = self.raw.clone();
        Self {raw}
    }
}

impl<T,S> Default for SharedHashSet<T,S>
    where T:Eq+Hash, S:Default+std::hash::BuildHasher {
    fn default() -> Self {
        let raw = default();
        Self {raw}
    }
}

impl<T,S> SharedHashSet<T,S>
where T:Eq+Hash, S:Default+std::hash::BuildHasher {
    pub fn new() -> Self {
        default()
    }

    pub fn mem_take(&self) -> HashSet<T,S> {
        mem::take(&mut *self.raw.borrow_mut())
    }
}

impl<T,S> SharedHashSet<T,S>
where T:Eq+Hash, S:std::hash::BuildHasher {
    pub fn insert(&self, t:T) -> bool {
        self.raw.borrow_mut().insert(t)
    }

    pub fn remove(&self, t:&T) -> bool {
        self.raw.borrow_mut().remove(t)
    }
}

impl<T,S> SharedHashSet<T,S> {
    pub fn clear(&self) {
        self.raw.borrow_mut().clear()
    }

    pub fn for_each<F>(&self, f:F)
    where F:FnMut(&T) {
        self.raw.borrow_mut().iter().for_each(f)
    }

    pub fn replace_with(&self, t:HashSet<T,S>) {
        *self.raw.borrow_mut() = t;
    }
}



// =====================
// === SharedHashMap ===
// =====================

#[derive(Derivative,CloneRef)]
#[derivative(Debug(bound="K:Eq+Hash+Debug, V:Debug, S:std::hash::BuildHasher"))]
pub struct SharedHashMap<K,V,S=std::collections::hash_map::RandomState> {
    pub raw : Rc<RefCell<HashMap<K,V,S>>>
}

impl<K,V,S> Clone for SharedHashMap<K,V,S> {
    fn clone(&self) -> Self {
        let raw = self.raw.clone();
        Self {raw}
    }
}

impl<K,V,S> Default for SharedHashMap<K,V,S>
where K:Eq+Hash, S:Default+std::hash::BuildHasher {
    fn default() -> Self {
        let raw = default();
        Self {raw}
    }
}

impl<K,V,S> SharedHashMap<K,V,S>
where K:Eq+Hash, S:Default+std::hash::BuildHasher {
    pub fn new() -> Self {
        default()
    }

    pub fn mem_take(&self) -> HashMap<K,V,S> {
        mem::take(&mut *self.raw.borrow_mut())
    }
}

impl<K,V,S> SharedHashMap<K,V,S>
where K:Eq+Hash, S:std::hash::BuildHasher {
    pub fn insert(&self, k:K, v:V) -> Option<V> {
        self.raw.borrow_mut().insert(k,v)
    }

    pub fn get_cloned(&self, k:&K) -> Option<V>
    where V:Clone {
        self.raw.borrow().get(k).cloned()
    }

    pub fn get_cloned_ref(&self, k:&K) -> Option<V>
    where V:CloneRef {
        self.raw.borrow().get(k).map(|t| t.clone_ref())
    }

    pub fn remove(&self, k:&K) -> Option<V> {
        self.raw.borrow_mut().remove(k)
    }
}

impl<K,V,S> SharedHashMap<K,V,S> {
    pub fn clear(&self) {
        self.raw.borrow_mut().clear()
    }

    pub fn for_each<F>(&self, f:F)
    where F:FnMut((&K,&V)) {
        self.raw.borrow_mut().iter().for_each(f)
    }

    pub fn keys(&self) -> Vec<K>
    where K:Clone {
        self.raw.borrow().keys().cloned().collect_vec()
    }
}








#[derive(Debug,Clone,CloneRef)]
pub struct Frp {
    pub inputs  : FrpInputs,
    pub outputs : FrpOutputs,
    pub status  : FrpStatus,
    pub node_release : frp::Stream<NodeId>
}

impl Deref for Frp {
    type Target = FrpInputs;
    fn deref(&self) -> &FrpInputs {
        &self.inputs
    }
}


ensogl::def_status_api! { FrpStatus
    /// Checks whether this graph editor instance is active.
    is_active,
    /// Checks whether this graph editor instance is empty.
    is_empty,
}

ensogl::def_command_api! { Commands
    /// Add a new node and place it in the origin of the workspace.
    add_node,
    /// Add a new node and place it at the mouse cursor position.
    add_node_at_cursor,
    /// Remove all selected nodes from the graph.
    remove_selected_nodes,
    /// Remove all nodes from the graph.
    remove_all_nodes,
    /// Toggle the visibility of the selected visualisations
    toggle_visualization_visibility,
    /// Set the data for the selected nodes. // TODO only has dummy functionality at the moment.
    debug_set_data_for_selected_node,
    /// Cycle the visualization for the selected nodes. TODO only has dummy functionality at the moment.
    debug_cycle_visualisation_for_selected_node,
}

impl Commands {
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            def add_node                                    = source();
            def add_node_at_cursor                          = source();
            def remove_selected_nodes                       = source();
            def remove_all_nodes                            = source();
            def toggle_visualization_visibility             = source();
            def debug_set_data_for_selected_node            = source();
            def debug_cycle_visualisation_for_selected_node = source();
        }
        Self {add_node,add_node_at_cursor,remove_selected_nodes,remove_all_nodes
             ,toggle_visualization_visibility,debug_set_data_for_selected_node
             ,debug_cycle_visualisation_for_selected_node}
    }
}



// =================
// === FrpInputs ===
// =================

#[derive(Debug,Clone,CloneRef,Shrinkwrap)]
pub struct FrpInputs {
    #[shrinkwrap(main_field)]
    commands                           : Commands,
//    pub add_node_at                    : frp::Source<Position>,
    pub connect_detached_edges_to_node : frp::Source<EdgeTarget>,
    pub connect_edge_source            : frp::Source<(EdgeId,EdgeTarget)>,
    pub connect_edge_target            : frp::Source<(EdgeId,EdgeTarget)>,
    pub connect_nodes                  : frp::Source<(EdgeTarget,EdgeTarget)>,
    pub deselect_all_nodes             : frp::Source,
    pub press_node_port                : frp::Source<(NodeId,span_tree::Crumbs)>,
    pub remove_edge                    : frp::Source<EdgeId>,
    pub select_node                    : frp::Source<NodeId>,
    pub set_node_expression            : frp::Source<(NodeId,Expression)>,
    pub set_node_position              : frp::Source<(NodeId,Position)>,
    pub set_visualization_data         : frp::Source<NodeId>,
    pub translate_selected_nodes       : frp::Source<Position>,
    pub cycle_visualization            : frp::Source<NodeId>,
    pub set_visualization              : frp::Source<(NodeId,Option<Visualization>)>,
}

impl FrpInputs {
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
//            def add_node_at                    = source();
            def connect_detached_edges_to_node = source();
            def connect_edge_source            = source();
            def connect_edge_target            = source();
            def connect_nodes                  = source();
            def deselect_all_nodes             = source();
            def press_node_port                = source();
            def remove_edge                    = source();
            def select_node                    = source();
            def set_node_expression            = source();
            def set_node_position              = source();
            def set_visualization_data         = source();
            def translate_selected_nodes       = source();
            def cycle_visualization            = source();
            def set_visualization              = source();
        }
        let commands = Commands::new(&network);
        Self {commands,remove_edge,press_node_port,set_visualization_data
             ,connect_detached_edges_to_node,connect_edge_source,connect_edge_target
             ,set_node_position,select_node,translate_selected_nodes,set_node_expression
             ,connect_nodes,deselect_all_nodes,cycle_visualization,set_visualization}
    }
}

impl application::command::FrpNetworkProvider for GraphEditor {
    fn network(&self) -> &frp::Network {
        &self.model.network
    }
}

impl application::command::CommandApi for GraphEditor {
    fn command_api_docs() -> Vec<application::command::EndpointDocs> {
        Commands::command_api_docs()
    }

    fn command_api(&self) -> Vec<application::command::CommandEndpoint> {
        self.frp.inputs.commands.command_api()
    }
}

impl application::command::StatusApi for GraphEditor {
    fn status_api_docs() -> Vec<application::command::EndpointDocs> {
        FrpStatus::status_api_docs()
    }

    fn status_api(&self) -> Vec<application::command::StatusEndpoint> {
        self.frp.status.status_api()
    }
}



// ==================
// === FrpOutputs ===
// ==================

#[derive(Debug,Clone,CloneRef)]
pub struct UnsealedFrpOutputs {
    network           : frp::Network,
    pub node_added    : frp::Merge<NodeId>,
    pub edge_added    : frp::Merge<EdgeId>,
    pub node_position : frp::Merge<(NodeId,Position)>,
}

#[allow(clippy::new_without_default)]
impl UnsealedFrpOutputs {
    pub fn new() -> Self {
        frp::new_network! { TRACE_ALL network
            def node_added    = gather();
            def edge_added    = gather();
            def node_position = gather();
        }
        Self {network,node_added,edge_added,node_position}
    }

    pub fn seal(&self) -> FrpOutputs {
        let network = self.network.clone_ref();
        frp::extend! { network
            def node_added = self.node_added.sampler();
            def edge_added = self.edge_added.sampler();
        }
        let node_position = self.node_position.clone_ref().into();
        FrpOutputs {network,node_added,edge_added,node_position}
    }
}


#[derive(Debug,Clone,CloneRef)]
pub struct FrpOutputs {
    network           : frp::Network,
    pub node_added    : frp::Sampler<NodeId>,
    pub edge_added    : frp::Sampler<EdgeId>,
    pub node_position : frp::Stream<(NodeId,Position)>,
}



// ============
// === Node ===
// ============

#[derive(Clone,CloneRef,Debug)]
pub struct Node {
    pub view      : NodeView,
    pub in_edges  : SharedHashSet<EdgeId>,
    pub out_edges : SharedHashSet<EdgeId>,
}

#[derive(Clone,Copy,Debug,Default,Display,Eq,From,Hash,Into,PartialEq)]
pub struct NodeId(pub Id);

impl From<&NodeId> for NodeId {
    fn from(t:&NodeId) -> Self { *t }
}

impl Node {
    pub fn new(view:NodeView) -> Self {
        let in_edges  = default();
        let out_edges = default();
        Self {view,in_edges,out_edges}
    }

    pub fn id(&self) -> NodeId {
        self.view.id().into()
    }
}

impl display::Object for Node {
    fn display_object(&self) -> &display::object::Instance {
        &self.view.display_object()
    }
}



// ============
// === Edge ===
// ============

#[derive(Clone,CloneRef,Debug)]
pub struct Edge {
    pub view : EdgeView,
    source   : Rc<RefCell<Option<EdgeTarget>>>,
    target   : Rc<RefCell<Option<EdgeTarget>>>,
}

#[derive(Clone,Copy,Debug,Default,Display,Eq,From,Hash,Into,PartialEq)]
pub struct EdgeId(pub Id);

impl From<&EdgeId> for EdgeId {
    fn from(t:&EdgeId) -> Self { *t }
}

impl Edge {
    pub fn new(view:EdgeView) -> Self {
        let source = default();
        let target = default();
        Self {view,source,target}
    }

    pub fn new_with_source(view:EdgeView, node_id:NodeId) -> Self {
        let port   = default();
        let source = EdgeTarget::new(node_id,port);
        let source = Rc::new(RefCell::new(Some(source)));
        let target = default();
        Self {view,source,target}
    }

    pub fn id(&self) -> EdgeId {
        self.view.id().into()
    }

    pub fn target(&self) -> Option<EdgeTarget> {
        self.target.borrow().as_ref().map(|t| t.clone_ref())
    }

    pub fn source(&self) -> Option<EdgeTarget> {
        self.source.borrow().as_ref().map(|t| t.clone_ref())
    }

    pub fn set_source(&self, source:EdgeTarget) {
        *self.source.borrow_mut() = Some(source)
    }

    pub fn set_target(&self, target:EdgeTarget) {
        *self.target.borrow_mut() = Some(target)
    }

    pub fn take_source(&self) -> Option<EdgeTarget> {
        mem::take(&mut *self.source.borrow_mut())
    }

    pub fn take_target(&self) -> Option<EdgeTarget> {
        mem::take(&mut *self.target.borrow_mut())
    }
}

impl display::Object for Edge {
    fn display_object(&self) -> &display::object::Instance {
        &self.view.display_object()
    }
}


// ==================
// === EdgeTarget ===
// ==================

#[derive(Clone,CloneRef,Debug,Default)]
pub struct EdgeTarget {
    node_id : Rc<Cell<NodeId>>,
    port    : Rc<RefCell<span_tree::Crumbs>>,
}

impl EdgeTarget {
    pub fn new(node_id:NodeId, port:span_tree::Crumbs) -> Self {
        let node_id = Rc::new(Cell::new(node_id));
        let port    = Rc::new(RefCell::new(port));
        Self {node_id,port}
    }

    pub fn new_without_port(node_id:NodeId) -> Self {
        Self::new(node_id,default())
    }

    pub fn node_id(&self) -> NodeId {
        self.node_id.get()
    }

    pub fn port(&self) -> span_tree::Crumbs {
        self.port.borrow().clone()
    }

    pub fn deep_clone(&self) -> Self {
        let node_id = self.node_id();
        let port    = self.port();
        Self::new(node_id,port)
    }
}







// =============
// === Nodes ===
// =============

#[derive(Debug,Clone,CloneRef)]
pub struct Nodes {
    pub logger   : Logger,
    pub all      : SharedHashMap<NodeId,Node>,
    pub selected : SharedHashSet<NodeId>,
}

impl Deref for Nodes {
    type Target = SharedHashMap<NodeId,Node>;
    fn deref(&self) -> &Self::Target {
        &self.all
    }
}

impl Nodes {
    pub fn new(logger:&Logger) -> Self {
        let logger   = logger.sub("nodes");
        let all      = default();
        let selected = default();
        Self {logger,all,selected}
    }
}





#[derive(Debug,Clone,CloneRef,Default)]
pub struct Edges {
    pub logger   : Logger,
    pub all      : SharedHashMap<EdgeId,Edge>,
    pub detached : SharedHashSet<EdgeId>,
}

impl Deref for Edges {
    type Target = SharedHashMap<EdgeId,Edge>;
    fn deref(&self) -> &Self::Target {
        &self.all
    }
}

impl Edges {
    pub fn new(logger:&Logger) -> Self {
        let logger   = logger.sub("edges");
        let all      = default();
        let detached = default();
        Self {logger,all,detached}
    }

    pub fn insert(&self, edge:Edge) {
        self.all.insert(edge.id(),edge);
    }
}






#[derive(Debug,CloneRef,Derivative)]
#[derivative(Clone(bound=""))]
pub struct TouchNetwork<T:frp::Data> {
    pub down     : frp::Source<T>,
    pub up       : frp::Stream<T>,
    pub is_down  : frp::Stream<bool>,
    pub selected : frp::Stream<T>
}

impl<T:frp::Data> TouchNetwork<T> {
    pub fn new(network:&frp::Network, mouse:&frp::io::Mouse) -> Self {
        frp::extend! { network
            def down          = source::<T> ();
            def down_bool     = down.map(|_| true);
            def up_bool       = mouse.release.map(|_| false);
            def is_down       = down_bool.merge(&up_bool);
            def was_down      = is_down.previous();
            def mouse_up      = mouse.release.gate(&was_down);
            def pos_on_down   = mouse.position.sample(&down);
            def pos_on_up     = mouse.position.sample(&mouse_up);
            def should_select = pos_on_up.map3(&pos_on_down,&mouse.distance,Self::check);
            def up            = down.sample(&mouse_up);
            def selected      = up.gate(&should_select);
        }
        Self {down,up,is_down,selected}
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn check(end:&Position, start:&Position, diff:&f32) -> bool {
        (end-start).length() <= diff * 2.0
    }
}

#[derive(Debug,Clone,CloneRef)]
pub struct TouchState {
    pub nodes      : TouchNetwork::<NodeId>,
    pub background : TouchNetwork::<()>,
}

impl TouchState {
    pub fn new(network:&frp::Network, mouse:&frp::io::Mouse) -> Self {
        let nodes = TouchNetwork::<NodeId>::new(&network,mouse);
        let background    = TouchNetwork::<()>::new(&network,mouse);
        Self {nodes,background}
    }
}



pub fn is_sub_crumb_of(src:&[span_tree::Crumb], tgt:&[span_tree::Crumb]) -> bool {
    if src.len() < tgt.len() { return false }
    for (s,t) in src.iter().zip(tgt.iter()) {
        if s != t { return false }
    }
    true
}

pub fn crumbs_overlap(src:&[span_tree::Crumb], tgt:&[span_tree::Crumb]) -> bool {
    is_sub_crumb_of(src,tgt) || is_sub_crumb_of(tgt,src)
}




// ===================================
// === GraphEditorModelWithNetwork ===
// ===================================

#[derive(Debug,Clone,CloneRef)]
pub struct GraphEditorModelWithNetwork {
    pub model   : GraphEditorModel,
    pub network : frp::Network,
}

impl Deref for GraphEditorModelWithNetwork {
    type Target = GraphEditorModel;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl GraphEditorModelWithNetwork {
    pub fn new<S:Into<Scene>>(scene:S, cursor:Cursor) -> Self {
        let network = frp::Network::new();
        let model   = GraphEditorModel::new(scene,cursor,&network);
        Self {model,network}
    }

    fn add_node_internal(&self, outputs:&UnsealedFrpOutputs) -> NodeId {
        let view = NodeView::new(&self.scene);
        let node = Node::new(view);
        let node_id = node.id();
        self.add_child(&node);


        let cursor = &self.cursor;
        let touch  = &self.touch_state;
        let model  = &self.model;

        frp::new_bridge_network! { [self.network, node.view.main_area.events.network]
            def _node_on_down_tagged = node.view.drag_area.events.mouse_down.map(f_!((touch) {
                touch.nodes.down.emit(node_id)
            }));
            def _cursor_mode = node.view.ports.frp.cursor_mode.map(f!((cursor)(mode) {
                cursor.frp.set_mode.emit(mode);
            }));
            def edge_added = node.view.frp.output_ports.mouse_down.map(f_!((model) {
                if let Some(node) = model.nodes.get_cloned_ref(&node_id) {
                    let view = EdgeView::new(&model.scene);
                    view.mod_position(|p| p.x = node.view.position().x + node::NODE_WIDTH/2.0);
                    view.mod_position(|p| p.y = node.view.position().y + node::NODE_HEIGHT/2.0);
                    model.add_child(&view);
                    let edge = Edge::new_with_source(view,node_id);
                    let edge_id = edge.id();
                    model.edges.insert(edge);
                    model.edges.detached.insert(edge_id);
                    node.out_edges.insert(edge_id);
                    edge_id
                } else { default() }
            }));
            outputs.edge_added.attach(&edge_added);


            def _press_node_port = node.view.ports.frp.press.map(f!((model)(crumbs){
                model.frp.press_node_port.emit((node_id,crumbs.clone()));
            }));
        }

        let chart = constructor_sample_js_bubble_chart();
        let dom_layer    = model.scene.dom.layers.front.clone_ref();
        chart.set_dom_layer(&dom_layer);

        let vis = Visualization::new(chart);
        node.view.frp.set_visualization.emit(Some(vis));


        self.nodes.insert(node_id,node);


        node_id
    }


    pub fn get_node_position(&self, node_id:NodeId) -> Option<Vector3<f32>> {
        self.nodes.get_cloned_ref(&node_id).map(|node| node.position())
    }

    // FIXME: remove
    pub fn deprecated_add_node(&self) -> WeakNodeView {
        todo!()
//        let node_id = self.add_node_internal();
//        let node    = self.nodes.get_cloned_ref(&node_id).unwrap();
//        node.view.downgrade()
    }

    // FIXME: remove
    pub fn deprecated_remove_node(&self, node:WeakNodeView) {
        if let Some(node) = node.upgrade() {
            self.nodes.remove(&node.id().into());
        }
    }
}



// ========================
// === GraphEditorModel ===
// ========================

#[derive(Debug,Clone,CloneRef)]
pub struct GraphEditorModel {
    pub logger         : Logger,
    pub display_object : display::object::Instance,
    pub scene          : Scene,
    pub cursor         : Cursor,
    pub nodes          : Nodes,
    pub edges          : Edges,
    touch_state        : TouchState,
    frp                : FrpInputs,
}

// === Public ===

impl GraphEditorModel {
    pub fn new<S:Into<Scene>>(scene:S, cursor:Cursor, network:&frp::Network) -> Self {
        let scene          = scene.into();
        let logger         = Logger::new("GraphEditor");
        let display_object = display::object::Instance::new(logger.clone());
        let nodes          = Nodes::new(&logger);
        let edges          = default();
        let frp            = FrpInputs::new(network);
        let touch_state    = TouchState::new(network,&scene.mouse.frp);
        Self {logger,display_object,scene,cursor,nodes,edges,touch_state,frp }
    }
}


// === Selection ===

impl GraphEditorModel {
    fn select_node(&self, node_id:NodeId) {
        self.deselect_all_nodes();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            self.nodes.selected.insert(node_id);
            node.view.frp.select.emit(());
        }
    }

    fn deselect_all_nodes(&self) {
        for node_id in &self.nodes.selected.mem_take() {
            if let Some(node) = self.nodes.get_cloned_ref(node_id) {
                node.view.frp.deselect.emit(());
            }
        }
    }
}


// === Remove ===

impl GraphEditorModel {
    fn remove_edge(&self, edge_id:EdgeId) {
        if let Some(edge) = self.edges.remove(&edge_id) {
            if let Some(source) = edge.take_source() {
                if let Some(source_node) = self.nodes.get_cloned_ref(&source.node_id()) {
                    source_node.out_edges.remove(&edge_id);
                }
            }

            if let Some(target) = edge.take_target() {
                if let Some(target_node) = self.nodes.get_cloned_ref(&target.node_id()) {
                    target_node.in_edges.remove(&edge_id);
                }
            }
        }
    }

    fn remove_node(&self, node_id:NodeId) {
        if let Some(node) = self.nodes.remove(&node_id) {
            for edge_id in node.in_edges  . mem_take() { self.remove_edge(edge_id); }
            for edge_id in node.out_edges . mem_take() { self.remove_edge(edge_id); }
        }
    }

    fn remove_all_nodes(&self) {
        for node_id in self.nodes.keys() {
            self.remove_node(node_id)
        }
    }

    fn remove_selected_nodes(&self) {
        for node_id in self.nodes.selected.mem_take() {
            self.remove_node(node_id);
        }
    }
}


// === Connect ===

impl GraphEditorModel {
    fn connect_detached_edges_to_node(&self, target:&EdgeTarget) {
        for edge_id in self.edges.detached.mem_take() {
            self.connect_edge_target(edge_id,target);
        }
    }

    fn connect_edge_source(&self, edge_id:EdgeId, target:&EdgeTarget) {
        if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
            if let Some(old_source) = edge.take_source() {
                if let Some(node) = self.nodes.get_cloned_ref(&old_source.node_id()) {
                    node.out_edges.remove(&edge_id);
                }
            }

            if let Some(node) = self.nodes.get_cloned_ref(&target.node_id()) {
                node.out_edges.insert(edge_id);
            }

            edge.set_source(target.deep_clone());
            self.refresh_edge_position(edge_id);
        }
    }

    fn connect_edge_target(&self, edge_id:EdgeId, target:&EdgeTarget) {
        if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
            if let Some(old_target) = edge.take_target() {
                if let Some(node) = self.nodes.get_cloned_ref(&old_target.node_id()) {
                    node.in_edges.remove(&edge_id);
                }
            }

            let target_port = target.port();
            if let Some(node) = self.nodes.get_cloned_ref(&target.node_id()) {
                let mut overlapping = vec![];
                for edge_id in node.in_edges.raw.borrow().clone().into_iter() {
                    if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
                        if let Some(edge_target) = edge.target() {
                            if crumbs_overlap(&edge_target.port(),&target_port) {
                                overlapping.push(edge_id);
                            }
                        }
                    }
                }
                for edge_id in &overlapping {
                    self.remove_edge(*edge_id)
                }
                node.in_edges.insert(edge_id);
            };

            edge.set_target(target.deep_clone());
            self.refresh_edge_position(edge_id);
        }
    }

    fn connect_nodes(&self, source:&EdgeTarget, target:&EdgeTarget) -> EdgeId {
        let edge = Edge::new(EdgeView::new(&self.scene));
        self.add_child(&edge);
        self.edges.insert(edge.clone_ref());

        let edge_id = edge.id();
        self.connect_edge_source(edge_id,source);
        self.connect_edge_target(edge_id,target);
        edge_id
    }

}


// === Position ===

impl GraphEditorModel {
    pub fn set_node_position(&self, node_id:NodeId, position:Position) {
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            node.view.mod_position(|t| {
                t.x = position.x;
                t.y = position.y;
            })
        }
    }

    pub fn refresh_edge_position(&self, edge_id:EdgeId) {
        self.refresh_edge_source_position(edge_id);
        self.refresh_edge_target_position(edge_id);
    }

    pub fn refresh_edge_source_position(&self, edge_id:EdgeId) {
        if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
            if let Some(edge_source) = edge.source() {
                if let Some(node) = self.nodes.get_cloned_ref(&edge_source.node_id()) {
                    edge.mod_position(|p| {
                        p.x = node.position().x + node::NODE_WIDTH/2.0;
                        p.y = node.position().y + node::NODE_HEIGHT/2.0;
                    });
                }
            }
        };
    }

    pub fn refresh_edge_target_position(&self, edge_id:EdgeId) {
        if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
            if let Some(edge_target) = edge.target() {
                if let Some(node) = self.nodes.get_cloned_ref(&edge_target.node_id()) {
                    let offset = node.view.ports.get_port_offset(&edge_target.port()).unwrap_or_else(|| Vector2::new(0.0,0.0));
                    let node_position = node.view.position();
                    let pos = frp::Position::new(node_position.x + offset.x, node_position.y + offset.y);
                    edge.view.events.target_position.emit(pos);
                }
            }
        };
    }
}

impl display::Object for GraphEditorModel {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}






// ===================
// === GraphEditor ===
// ===================

#[derive(Debug,Clone,CloneRef)]
pub struct GraphEditor {
    pub model : GraphEditorModelWithNetwork,
    pub frp   : Frp,
}

impl Deref for GraphEditor {
    type Target = GraphEditorModelWithNetwork;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl GraphEditor {
    pub fn add_node(&self) -> NodeId {
        self.frp.add_node.emit(());
        self.frp.outputs.node_added.value()
    }
}

impl application::command::Provider for GraphEditor {
    fn label() -> &'static str {
        "GraphEditor"
    }
}

impl application::shortcut::DefaultShortcutProvider for GraphEditor {
    fn default_shortcuts() -> Vec<application::shortcut::Shortcut> {
        use keyboard::Key;
        vec! [ Self::self_shortcut(&[Key::Character("n".into())] , "add_node_at_cursor")
             , Self::self_shortcut(&[Key::Backspace]             , "remove_selected_nodes")
             , Self::self_shortcut(&[Key::Character(" ".into())] , "toggle_visualization_visibility")
             , Self::self_shortcut(&[Key::Character("d".into())] , "debug_set_data_for_selected_node")
             , Self::self_shortcut(&[Key::Character("f".into())]  , "debug_cycle_visualisation_for_selected_node")
        ]
    }
}


macro_rules! model_bind {
    ($network:ident $model:ident . $name:ident($($arg:ident),*)) => {
        frp::extend! { $network
            def _eval = $name.map(f!(($model)(($($arg),*)) $model.$name($($arg.into()),*)));
        }
    };
}


impl application::View for GraphEditor {

    #[allow(unused_parens)]
    fn new(world:&World) -> Self {
        let scene  = world.scene();
        let cursor = Cursor::new(world.scene());
        web::body().set_style_or_panic("cursor","none");
        world.add_child(&cursor);

        let model   = GraphEditorModelWithNetwork::new(scene,cursor.clone_ref());
        let network = &model.network;
        let nodes   = &model.nodes;
        let edges   = &model.edges;
        let inputs  = &model.frp;
        let mouse   = &scene.mouse.frp;
        let touch   = &model.touch_state;

        let outputs = UnsealedFrpOutputs::new();


        frp::extend! { network

        // === Selection Target Redirection ===

        def mouse_down_target  = mouse.press.map(f_!((model) model.scene.mouse.target.get()));
        def _perform_selection = mouse_down_target.map(f!((touch,model)(target) {
            match target {
                display::scene::Target::Background  => touch.background.down.emit(()),
                display::scene::Target::Symbol {..} => {
                    if let Some(target) = model.scene.shapes.get_mouse_target(*target) {
                        target.mouse_down().emit(());
                    }
                }
            }
        }));


        // === Cursor Selection ===

        def mouse_on_down_position = mouse.position.sample(&mouse.press);
        def selection_zero         = source::<Position>();
        def selection_size_down    = mouse.position.map2(&mouse_on_down_position,|m,n|{m-n});
        def selection_size_if_down = selection_size_down.gate(&touch.background.is_down);
        def selection_size_on_down = selection_zero.sample(&mouse.press);
        def selection_size         = selection_size_if_down.merge(&selection_size_on_down);

        def _cursor_size = selection_size.map(f!((cursor)(p) {
            cursor.set_selection_size(Vector2::new(p.x,p.y));
        }));

        def _cursor_press = mouse.press.map(f!((cursor)(_) {
            cursor.frp.press.emit(());
        }));

        def _cursor_release = mouse.release.map(f!((cursor)(_) {
            cursor.frp.release.emit(());
        }));


        // === Selection ===

        def select_node        = inputs.select_node.merge(&touch.nodes.selected);
        def deselect_all_nodes = inputs.deselect_all_nodes.merge(&touch.background.selected);
        model_bind!(network model.select_node(node_id));
        model_bind!(network model.deselect_all_nodes());


        // === Connect Nodes ===

        def edge_target_press = inputs.press_node_port.map(|(id,port)| EdgeTarget::new(*id,port.clone()));
        def connect_detached_edges_to_node = edge_target_press.merge(&inputs.connect_detached_edges_to_node);
        model_bind!(network model.connect_detached_edges_to_node(target));

        let connect_edge_source = inputs.connect_edge_source.clone_ref();
        let connect_edge_target = inputs.connect_edge_target.clone_ref();
        model_bind!(network model.connect_edge_source(edge_id,target));
        model_bind!(network model.connect_edge_target(edge_id,target));

        def edge_added = inputs.connect_nodes.map(f!((model)((source,target)) {
            model.connect_nodes(source,target)
        }));
        outputs.edge_added.attach(&edge_added);


        // === Add Node ===

        def add_node_at_cursor_ = inputs.add_node_at_cursor.map(|_|());
        def add_node            = inputs.add_node.merge(&add_node_at_cursor_);

        def node_added = add_node.map(f_!((model,outputs) model.add_node_internal(&outputs)));
        outputs.node_added.attach(&node_added);



        // === Remove Node ===

        let remove_all_nodes      = inputs.remove_all_nodes.clone_ref();
        let remove_selected_nodes = inputs.remove_selected_nodes.clone_ref();
        let remove_edge           = inputs.remove_edge.clone_ref();
        model_bind!(network model.remove_all_nodes());
        model_bind!(network model.remove_selected_nodes());
        model_bind!(network model.remove_edge(edge_id));


        // === Set NodeView Expression ===

        def _set_node_expr = inputs.set_node_expression.map(f!((nodes)((node_id,expression)){
            if let Some(node) = nodes.all.raw.borrow().get(node_id) {
                node.view.ports.set_expression(expression);
            }
        }));


        // === Move Nodes ===

        def mouse_tx_if_node_pressed = mouse.translation.gate(&touch.nodes.is_down);
        def node_dragged = mouse_tx_if_node_pressed.map2(&touch.nodes.down,f!((model,nodes)(tx,node_id) {
            let new_position = match nodes.get_cloned_ref(&node_id) {
                None       => default(),
                Some(node) => {
                    node.view.mod_position(|p| { p.x += tx.x; p.y += tx.y; });
                    for edge_id in &node.in_edges.raw.borrow().clone() {
                        model.refresh_edge_target_position(*edge_id);
                    }
                    for edge_id in &node.out_edges.raw.borrow().clone() {
                        model.refresh_edge_position(*edge_id);
                    }
                    let position = node.position();
                    frp::Position::new(position.x,position.y)
                }
            };
            (*node_id,new_position)
        }));

        def _move_selected_nodes = inputs.translate_selected_nodes.map(f!((nodes)(t) {
            for node_id in &*nodes.selected.raw.borrow() {
                if let Some(node) = nodes.get_cloned(node_id) {
                    node.mod_position(|p| {
                        p.x += t.x;
                        p.y += t.y;
                    })
                }
            }
        }));


        // === Set Node Position ===

        def set_node_position_at_cursor = inputs.add_node_at_cursor.map3(&outputs.node_added,&mouse.position,|_,node_id,position| (*node_id,*position) );

        def set_node_position = inputs.set_node_position.merge(&set_node_position_at_cursor);
        outputs.node_position.attach(&set_node_position);
        outputs.node_position.attach(&node_dragged);
        model_bind!(network model.set_node_position(node_id,position));





        // === Move Edges ===

        def _move_connections = cursor.frp.position.map(f!((edges)(position) {
            edges.detached.for_each(|id| {
                if let Some(edge) = edges.get_cloned_ref(id) {
                    edge.view.events.target_position.emit(position)
                }
            })
        }));

         // === Vis Cycling ===
         def _cycle_vis= inputs.debug_cycle_visualisation_for_selected_node.map(f!((inputs,nodes)(_) {
            nodes.selected.for_each(|node| inputs.cycle_visualization.emit(node));
        }));

        // === Vis Set ===
        def _update_vis_data = inputs.set_visualization.map(f!((nodes)((node_id,vis)) {
            if let Some(node) = nodes.get_cloned_ref(node_id) {
                node.view.visualization_container.frp.set_visualization.emit(vis)
            }
        }));

        // === Vis Update Data ===

        // TODO remove this once real data is available.
        let dummy_switch  = Rc::new(Cell::new(false));
        let sample_data_generator = MockDataGenerator3D::default();
        def _set_dumy_data = inputs.debug_set_data_for_selected_node.map(f!((nodes)(_) {
            nodes.selected.for_each(|node_id| {
                let data          = Rc::new(sample_data_generator.generate_data());
                let content       = Rc::new(serde_json::to_value(data).unwrap());
                let data          = visualization::Data::JSON{ content };
                if let Some(node) = nodes.get_cloned(node_id) {
                    node.view.visualization_container.frp.set_data.emit(Some(data));
                }
            })
        }));

         def _set_dumy_data = inputs.cycle_visualization.map(f!((scene,nodes)(node_id) {
            // TODO remove dummy cycling once we have the visualization registry.
            let dc = dummy_switch.get();
            dummy_switch.set(!dc);
            let vis = if dc {
                Visualization::new(native::BubbleChart::new(&scene))
            } else {
                let chart     = constructor_sample_js_bubble_chart();
                let dom_layer = scene.dom.layers.front.clone_ref();
                chart.set_dom_layer(&dom_layer);
                Visualization::new(chart)
            };
            if let Some(node) = nodes.get_cloned_ref(node_id) {
                node.view.visualization_container.frp.set_visualization.emit(Some(vis));
            }
        }));


        // === Toggle Visualization Visibility ===

        def _toggle_selected = inputs.toggle_visualization_visibility.map(f!((nodes)(_) {
            nodes.selected.for_each(|node_id| {
                if let Some(node) = nodes.get_cloned_ref(node_id) {
                    node.view.visualization_container.frp.toggle_visibility.emit(());
                }
            });
        }));



        // === Status ===

        def is_active_src = source::<bool>();
        def is_empty_src  = source::<bool>();
        def is_active = is_active_src.sampler();
        def is_empty  = is_empty_src.sampler();

        }

        // FIXME This is a temporary solution. Should be replaced by a real thing once layout
        //       management is implemented.
        is_active_src.emit(true);

        let status = FrpStatus {is_active,is_empty};

        let node_release = touch.nodes.up.clone_ref();


        let inputs = inputs.clone_ref();
        let outputs = outputs.seal();
        let frp = Frp {inputs,outputs,status,node_release};

        Self {model,frp}
    }


}

impl display::Object for GraphEditor {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
