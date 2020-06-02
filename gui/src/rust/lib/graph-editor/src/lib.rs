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

#![recursion_limit="1024"]

#[warn(missing_docs)]
pub mod component;

/// Common types and functions usable in all modules of this crate.
pub mod prelude {
    pub use ensogl::prelude::*;
}

use crate::component::cursor;
use crate::component::node;
use crate::component::visualization::MockDataGenerator3D;
use crate::component::visualization::Visualization;
use crate::component::visualization;

use enso_frp as frp;
use enso_frp::io::keyboard;
use enso_frp::Position;
use ensogl::application::shortcut;
use ensogl::application;
use ensogl::data::color;
use ensogl::display::object::Id;
use ensogl::display::Scene;
use ensogl::display::world::*;
use ensogl::display;
use ensogl::prelude::*;
use ensogl::system::web::StyleSetter;
use ensogl::system::web;
use nalgebra::Vector2;



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

    pub fn contains(&self, value:&T) -> bool {
        self.raw.borrow().contains(value)
    }
}

impl<T,S> SharedHashSet<T,S> {
    pub fn is_empty(&self) -> bool {
        self.raw.borrow().is_empty()
    }

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

    pub fn keys(&self) -> Vec<T>
    where T:Clone {
        self.raw.borrow().iter().cloned().collect_vec()
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

    pub fn get_copied(&self, k:&K) -> Option<V>
    where V:Copy {
        self.raw.borrow().get(k).copied()
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
    /// Toggle the visibility of the selected visualizations.
    toggle_visualization_visibility,


    /// Enable nodes multi selection mode. It works like inverse mode for single node selection and like merge mode for multi node selection mode.
    enable_node_multi_select,
    /// Disable nodes multi selection mode. It works like inverse mode for single node selection and like merge mode for multi node selection mode.
    disable_node_multi_select,
    /// Toggle nodes multi selection mode. It works like inverse mode for single node selection and like merge mode for multi node selection mode.
    toggle_node_multi_select,


    /// Enable nodes merge selection mode.
    enable_node_merge_select,
    /// Disable nodes merge selection mode.
    disable_node_merge_select,
    /// Toggles nodes merge selection mode.
    toggle_node_merge_select,


    /// Enable nodes subtract selection mode.
    enable_node_subtract_select,
    /// Disable nodes subtract selection mode.
    disable_node_subtract_select,
    /// Toggle nodes subtract selection mode.
    toggle_node_subtract_select,


    /// Enable nodes inverse selection mode.
    enable_node_inverse_select,
    /// Disable nodes inverse selection mode.
    disable_node_inverse_select,
    /// Toggle nodes inverse selection mode.
    toggle_node_inverse_select,



    /// Set the data for the selected nodes. // TODO only has dummy functionality at the moment.
    debug_set_data_for_selected_node,

    /// Cycle the visualization for the selected nodes. TODO only has dummy functionality at the moment.
    debug_cycle_visualization_for_selected_node,
}

impl Commands {
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            def add_node                         = source();
            def add_node_at_cursor               = source();
            def remove_selected_nodes            = source();
            def remove_all_nodes                 = source();
            def toggle_visualization_visibility  = source();

            def enable_node_multi_select         = source();
            def disable_node_multi_select        = source();
            def toggle_node_multi_select         = source();


            def enable_node_merge_select         = source();
            def disable_node_merge_select        = source();
            def toggle_node_merge_select         = source();

            def enable_node_subtract_select      = source();
            def disable_node_subtract_select     = source();
            def toggle_node_subtract_select      = source();

            def enable_node_inverse_select       = source();
            def disable_node_inverse_select      = source();
            def toggle_node_inverse_select       = source();

            def debug_set_data_for_selected_node            = source();
            def debug_cycle_visualization_for_selected_node = source();

        }
        Self {add_node,add_node_at_cursor,remove_selected_nodes,remove_all_nodes
             ,toggle_visualization_visibility
             ,enable_node_multi_select,disable_node_multi_select,toggle_node_multi_select
             ,enable_node_merge_select,disable_node_merge_select,toggle_node_merge_select
             ,enable_node_subtract_select,disable_node_subtract_select,toggle_node_subtract_select
             ,enable_node_inverse_select,disable_node_inverse_select,toggle_node_inverse_select
             ,debug_set_data_for_selected_node,debug_cycle_visualization_for_selected_node}
    }
}



// =================
// === FrpInputs ===
// =================

#[derive(Debug,Clone,CloneRef,Shrinkwrap)]
pub struct FrpInputs {
    #[shrinkwrap(main_field)]
    commands                         : Commands,
    pub set_detached_edge_targets    : frp::Source<EdgeTarget>,
    pub set_edge_source              : frp::Source<(EdgeId,EdgeTarget)>,
    pub set_edge_target              : frp::Source<(EdgeId,EdgeTarget)>,
    pub unset_edge_source            : frp::Source<EdgeId>,
    pub unset_edge_target            : frp::Source<EdgeId>,
    pub connect_nodes                : frp::Source<(EdgeTarget,EdgeTarget)>,
    pub deselect_all_nodes           : frp::Source,
    pub press_node_input             : frp::Source<EdgeTarget>,
    pub remove_all_node_edges        : frp::Source<NodeId>,
    pub remove_all_node_input_edges  : frp::Source<NodeId>,
    pub remove_all_node_output_edges : frp::Source<NodeId>,
    pub remove_edge                  : frp::Source<EdgeId>,
    pub select_node                  : frp::Source<NodeId>,
    pub remove_node                  : frp::Source<NodeId>,
    pub set_node_expression          : frp::Source<(NodeId,node::Expression)>,
    pub set_node_position            : frp::Source<(NodeId,Position)>,
    pub translate_selected_nodes     : frp::Source<Position>,
    pub cycle_visualization          : frp::Source<NodeId>,
    pub set_visualization            : frp::Source<(NodeId,Option<Visualization>)>,
    pub register_visualization_class : frp::Source<Option<Rc<visualization::Handle>>>,
    pub set_visualization_data       : frp::Source<(NodeId,Option<visualization::Data>)>,

    hover_node_input           : frp::Source<Option<EdgeTarget>>,
    some_edge_targets_detached : frp::Source,
    all_edge_targets_attached  : frp::Source,
}

impl FrpInputs {
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            def set_detached_edge_targets    = source();
            def set_edge_source              = source();
            def set_edge_target              = source();
            def unset_edge_source            = source();
            def unset_edge_target            = source();
            def connect_nodes                = source();
            def deselect_all_nodes           = source();
            def press_node_input             = source();
            def remove_all_node_edges        = source();
            def remove_all_node_input_edges  = source();
            def remove_all_node_output_edges = source();
            def remove_edge                  = source();
            def select_node                  = source();
            def remove_node                  = source();
            def set_node_expression          = source();
            def set_node_position            = source();
            def set_visualization_data       = source();
            def translate_selected_nodes     = source();
            def cycle_visualization          = source();
            def set_visualization            = source();
            def register_visualization_class = source();

            def hover_node_input           = source();
            def some_edge_targets_detached = source();
            def all_edge_targets_attached  = source();

        }
        let commands = Commands::new(&network);
        Self {commands,remove_edge,press_node_input,remove_all_node_edges
             ,remove_all_node_input_edges,remove_all_node_output_edges,set_visualization_data
             ,set_detached_edge_targets,set_edge_source,set_edge_target
             ,unset_edge_source,unset_edge_target
             ,set_node_position,select_node,remove_node,translate_selected_nodes,set_node_expression
             ,connect_nodes,deselect_all_nodes,cycle_visualization,set_visualization
             ,register_visualization_class,some_edge_targets_detached,all_edge_targets_attached
             ,hover_node_input
             }
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

macro_rules! generate_frp_outputs {
    ($( $field:ident : $field_ty:ty ),* $(,)?) => {
        #[derive(Debug,Clone,CloneRef)]
        pub struct UnsealedFrpOutputs {
            network : frp::Network,
            $($field : frp::Any<$field_ty>),*
        }

        #[derive(Debug,Clone,CloneRef)]
        pub struct FrpOutputs {
            network : frp::Network,
            $(pub $field : frp::Sampler<$field_ty>),*
        }

        #[allow(clippy::new_without_default)]
        impl UnsealedFrpOutputs {
            pub fn new() -> Self {
                frp::new_network! { network
                    $(def $field = any_mut();)*
                }
                Self {network, $($field),*}
            }

            pub fn seal(&self) -> FrpOutputs {
                let network = self.network.clone_ref();
                frp::extend! { network
                    $(def $field = self.$field.sampler();)*
                }
                FrpOutputs {network, $($field),*}
            }
        }
    };
}


generate_frp_outputs! {
    node_added                : NodeId,
    node_removed              : NodeId,
    node_selected             : NodeId,
    node_deselected           : NodeId,
    node_position_set         : (NodeId,Position),
    node_position_set_batched : (NodeId,Position),
    node_expression_set       : (NodeId,node::Expression),

    edge_added        : EdgeId,
    edge_removed      : EdgeId,
    edge_source_set   : (EdgeId,EdgeTarget),
    edge_target_set   : (EdgeId,EdgeTarget),
    edge_source_unset : EdgeId,
    edge_target_unset : EdgeId,

    some_edge_targets_detached : (),
    all_edge_targets_attached  : (),

    connection_added    : EdgeId,
    connection_removed  : EdgeId,

    visualization_enabled  : NodeId,
    visualization_disabled : NodeId,
}




// ============
// === Node ===
// ============

#[derive(Clone,CloneRef,Debug)]
pub struct Node {
    pub view      : component::Node,
    pub in_edges  : SharedHashSet<EdgeId>,
    pub out_edges : SharedHashSet<EdgeId>,
}

#[derive(Clone,CloneRef,Copy,Debug,Default,Display,Eq,From,Hash,Into,PartialEq)]
pub struct NodeId(pub Id);

impl Node {
    pub fn new(view:component::Node) -> Self {
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
    pub view : component::Edge,
    source   : Rc<RefCell<Option<EdgeTarget>>>,
    target   : Rc<RefCell<Option<EdgeTarget>>>,
}

#[derive(Clone,CloneRef,Copy,Debug,Default,Display,Eq,From,Hash,Into,PartialEq)]
pub struct EdgeId(pub Id);

impl Edge {
    pub fn new(view:component::Edge) -> Self {
        let source = default();
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

    pub fn has_source(&self) -> bool {
        self.source.borrow().is_some()
    }

    pub fn has_target(&self) -> bool {
        self.target.borrow().is_some()
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
    pub node_id : NodeId,
    pub port    : Rc<span_tree::Crumbs>,
}

impl EdgeTarget {
    pub fn new(node_id:impl Into<NodeId>, port:span_tree::Crumbs) -> Self {
        let node_id = node_id.into();
        let port    = Rc::new(port);
        Self {node_id,port}
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
    pub logger          : Logger,
    pub all             : SharedHashMap<EdgeId,Edge>,
    pub detached_source : SharedHashSet<EdgeId>,
    pub detached_target : SharedHashSet<EdgeId>,
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
        let detached_source = default();
        let detached_target = default();
        Self {logger,all,detached_source,detached_target}
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
            down          <- source::<T> ();
            down_bool     <- down.map(|_| true);
            up_bool       <- mouse.release.map(|_| false);
            is_down       <- any (down_bool,up_bool);
            was_down      <- is_down.previous();
            mouse_up      <- mouse.release.gate(&was_down);
            pos_on_down   <- mouse.position.sample(&down);
            pos_on_up     <- mouse.position.sample(&mouse_up);
            should_select <- pos_on_up.map3(&pos_on_down,&mouse.distance,Self::check);
            up            <- down.sample(&mouse_up);
            selected      <- up.gate(&should_select);
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
        let nodes      = TouchNetwork::<NodeId>::new(&network,mouse);
        let background = TouchNetwork::<()>::new(&network,mouse);
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
    pub fn new<S:Into<Scene>>(scene:S, cursor:component::Cursor) -> Self {
        let network = frp::Network::new();
        let model   = GraphEditorModel::new(scene,cursor,&network);
        Self {model,network}
    }

    fn new_node
    ( &self
    , cursor_style : &frp::Source<cursor::Style>
    , output_press : &frp::Source<NodeId>
    ) -> NodeId {
        let view = component::Node::new(&self.scene);
        let node = Node::new(view);
        let node_id = node.id();
        self.add_child(&node);

        let touch = &self.touch_state;
        let model = &self.model;

        frp::new_bridge_network! { [self.network, node.view.main_area.events.network]
            eval_ node.view.drag_area.events.mouse_down(touch.nodes.down.emit(node_id));
            eval  node.view.ports.frp.cursor_style ((style) cursor_style.emit(style));
            eval_ node.view.frp.output_ports.mouse_down (output_press.emit(node_id));
            eval  node.view.ports.frp.press ((crumbs)
                model.frp.press_node_input.emit(EdgeTarget::new(node_id,crumbs.clone()))
            );

            eval node.view.ports.frp.hover ([model](crumbs) {
                let target = crumbs.as_ref().map(|c| EdgeTarget::new(node_id,c.clone()));
                model.frp.hover_node_input.emit(target);
            });
        }

        self.nodes.insert(node_id,node);

        node_id
    }


    pub fn get_node_position(&self, node_id:NodeId) -> Option<Vector3<f32>> {
        self.nodes.get_cloned_ref(&node_id).map(|node| node.position())
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
    pub cursor         : component::Cursor,
    pub nodes          : Nodes,
    pub edges          : Edges,
    touch_state        : TouchState,
    frp                : FrpInputs,
}

// === Public ===

impl GraphEditorModel {
    pub fn new<S:Into<Scene>>(scene:S, cursor:component::Cursor, network:&frp::Network) -> Self {
        let scene          = scene.into();
        let logger         = Logger::new("GraphEditor");
        let display_object = display::object::Instance::new(logger.clone());
        let nodes          = Nodes::new(&logger);
        let edges          = default();
        let frp            = FrpInputs::new(network);
        let touch_state    = TouchState::new(network,&scene.mouse.frp);
        Self {logger,display_object,scene,cursor,nodes,edges,touch_state,frp }
    }

    fn new_edge(&self) -> EdgeId {
        let edge    = Edge::new(component::Edge::new(&self.scene));
        let edge_id = edge.id();
        self.add_child(&edge);
        self.edges.insert(edge.clone_ref());

        let first_detached = self.edges.detached_target.is_empty();
        self.edges.detached_target.insert(edge_id);
        if first_detached {
            self.frp.some_edge_targets_detached.emit(());
        }

        edge_id
    }

    pub fn all_nodes(&self) -> Vec<NodeId> {
        self.nodes.all.keys()
    }

    pub fn selected_nodes(&self) -> Vec<NodeId> {
        self.nodes.selected.keys()
    }
}


// === Selection ===

impl GraphEditorModel {
    fn select_node(&self, node_id:impl Into<NodeId>) {
        let node_id = node_id.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            self.nodes.selected.insert(node_id);
            node.view.frp.select.emit(());
        }
    }

    fn deselect_node(&self, node_id:impl Into<NodeId>) {
        let node_id = node_id.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            self.nodes.selected.remove(&node_id);
            node.view.frp.deselect.emit(());
        }
    }
}


// === Remove ===

impl GraphEditorModel {
    fn remove_edge<E:Into<EdgeId>>(&self, edge_id:E) {
        let edge_id = edge_id.into();
        if let Some(edge) = self.edges.remove(&edge_id) {
            if let Some(source) = edge.take_source() {
                if let Some(source_node) = self.nodes.get_cloned_ref(&source.node_id) {
                    source_node.out_edges.remove(&edge_id);
                }
            }

            if let Some(target) = edge.take_target() {
                if let Some(target_node) = self.nodes.get_cloned_ref(&target.node_id) {
                    target_node.in_edges.remove(&edge_id);
                }
            }
        }
    }

    /// Warning! This function does not remove connected edges. It needs to be handled by the
    /// implementation.
    fn remove_node(&self, node_id:impl Into<NodeId>) {
        let node_id = node_id.into();
        self.nodes.remove(&node_id);
        self.nodes.selected.remove(&node_id);
    }

    fn node_in_edges(&self, node_id:impl Into<NodeId>) -> Vec<EdgeId> {
        let node_id = node_id.into();
        self.nodes.get_cloned_ref(&node_id).map(|node| {
            node.in_edges.keys()
        }).unwrap_or_default()
    }

    fn node_out_edges(&self, node_id:impl Into<NodeId>) -> Vec<EdgeId> {
        let node_id = node_id.into();
        self.nodes.get_cloned_ref(&node_id).map(|node| {
            node.out_edges.keys()
        }).unwrap_or_default()
    }

    fn node_in_and_out_edges(&self, node_id:impl Into<NodeId>) -> Vec<EdgeId> {
        let node_id = node_id.into();
        let mut edges = self.node_in_edges(node_id);
        edges.extend(&self.node_out_edges(node_id));
        edges
    }

    fn set_node_expression(&self, node_id:impl Into<NodeId>, expr:impl Into<node::Expression>) {
        let node_id = node_id.into();
        let expr    = expr.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            node.view.frp.set_expression.emit(expr);
        }
        for edge_id in self.node_out_edges(node_id) {
            self.refresh_edge_source_width(edge_id);
        }
    }

    fn is_connection(&self, edge_id:impl Into<EdgeId>) -> bool {
        let edge_id = edge_id.into();
        match self.edges.get_cloned_ref(&edge_id) {
            None    => false,
            Some(e) => e.has_source() && e.has_target()
        }
    }
}


// === Connect ===

impl GraphEditorModel {
    fn set_edge_source(&self, edge_id:EdgeId, target:impl Into<EdgeTarget>) {
        let target = target.into();
        if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
            if let Some(node) = self.nodes.get_cloned_ref(&target.node_id) {
                node.out_edges.insert(edge_id);
                edge.set_source(target);
                // FIXME: both lines require edge to refresh. Let's make it more efficient.
                self.refresh_edge_position(edge_id);
                self.refresh_edge_source_width(edge_id);
            }
        }
    }

    fn set_edge_target(&self, edge_id:EdgeId, target:impl Into<EdgeTarget>) {
        let target = target.into();
        if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
            if let Some(node) = self.nodes.get_cloned_ref(&target.node_id) {
                node.in_edges.insert(edge_id);
                edge.set_target(target);

                self.edges.detached_target.remove(&edge_id);
                let all_attached = self.edges.detached_target.is_empty();
                if all_attached {
                    self.frp.all_edge_targets_attached.emit(());
                }

                edge.view.frp.target_attached.emit(true);
                self.refresh_edge_position(edge_id);
            };
        }
    }

    fn take_edges_with_detached_targets(&self) -> HashSet<EdgeId> {
        let edges = self.edges.detached_target.mem_take();
        if !edges.is_empty() {
            self.frp.all_edge_targets_attached.emit(());
        }
        edges
    }

    fn overlapping_edges(&self, target:&EdgeTarget) -> Vec<EdgeId> {
        let mut overlapping = vec![];
        if let Some(node) = self.nodes.get_cloned_ref(&target.node_id) {
            for edge_id in node.in_edges.raw.borrow().clone().into_iter() {
                if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
                    if let Some(edge_target) = edge.target() {
                        if crumbs_overlap(&edge_target.port,&target.port) {
                            overlapping.push(edge_id);
                        }
                    }
                }
            }
        }
        overlapping
    }
}


// === Position ===

impl GraphEditorModel {
    pub fn set_node_position(&self, node_id:impl Into<NodeId>, position:impl Into<Position>) {
        let node_id  = node_id.into();
        let position = position.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            node.view.mod_position(|t| {
                t.x = position.x;
                t.y = position.y;
            });
            for edge_id in self.node_in_and_out_edges(node_id) {
                self.refresh_edge_position(edge_id);
            }
        }
    }

    pub fn node_position(&self, node_id:impl Into<NodeId>) -> Position {
        let node_id = node_id.into();
        self.nodes.get_cloned_ref(&node_id).map(|node| {
            let v_pos = node.position();
            frp::Position::new(v_pos.x, v_pos.y)
        }).unwrap_or_default()
    }

    pub fn node_pos_mod
    (&self, node_id:impl Into<NodeId>, pos_diff:impl Into<Position>) -> (NodeId,Position) {
        let node_id      = node_id.into();
        let pos_diff     = pos_diff.into();
        let new_position = if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            let node_pos = node.position();
            frp::Position::new(node_pos.x + pos_diff.x, node_pos.y + pos_diff.y)
        } else {
            default()
        };
        (node_id,new_position)
    }

    pub fn refresh_edge_position(&self, edge_id:EdgeId) {
        self.refresh_edge_source_position(edge_id);
        self.refresh_edge_target_position(edge_id);
    }

    pub fn refresh_edge_source_width(&self, edge_id:EdgeId) {
        if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
            if let Some(edge_source) = edge.source() {
                if let Some(node) = self.nodes.get_cloned_ref(&edge_source.node_id) {
                    edge.view.frp.source_width.emit(node.view.width());
                }
            }
        };
    }

    pub fn refresh_edge_source_position(&self, edge_id:EdgeId) {
        if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
            if let Some(edge_source) = edge.source() {
                if let Some(node) = self.nodes.get_cloned_ref(&edge_source.node_id) {
                    edge.mod_position(|p| {
                        p.x = node.position().x + node.view.width()/2.0;
                        p.y = node.position().y + node::NODE_HEIGHT/2.0;
                    });
                }
            }
        };
    }

    pub fn refresh_edge_target_position(&self, edge_id:EdgeId) {
        if let Some(edge) = self.edges.get_cloned_ref(&edge_id) {
            if let Some(edge_target) = edge.target() {
                if let Some(node) = self.nodes.get_cloned_ref(&edge_target.node_id) {
                    let offset = node.view.ports.get_port_offset(&edge_target.port).unwrap_or_else(|| Vector2::new(0.0,0.0));
                    let node_position = node.view.position();
                    let pos = frp::Position::new(node_position.x + offset.x, node_position.y + offset.y);
                    edge.view.frp.target_position.emit(pos);
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
    /// Add a new node and returns its ID.
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
        vec! [ Self::self_shortcut(shortcut::Action::press   (&[Key::Character("n".into())])               , "add_node_at_cursor")
             , Self::self_shortcut(shortcut::Action::press   (&[Key::Backspace])                           , "remove_selected_nodes")
             , Self::self_shortcut(shortcut::Action::press   (&[Key::Control, Key::Character(" ".into())]) , "toggle_visualization_visibility")
             , Self::self_shortcut(shortcut::Action::press   (&[Key::Meta])                                , "toggle_node_multi_select")
             , Self::self_shortcut(shortcut::Action::release (&[Key::Meta])                                , "toggle_node_multi_select")
             , Self::self_shortcut(shortcut::Action::press   (&[Key::Control])                             , "toggle_node_multi_select")
             , Self::self_shortcut(shortcut::Action::release (&[Key::Control])                             , "toggle_node_multi_select")
             , Self::self_shortcut(shortcut::Action::press   (&[Key::Shift])                               , "toggle_node_merge_select")
             , Self::self_shortcut(shortcut::Action::release (&[Key::Shift])                               , "toggle_node_merge_select")
             , Self::self_shortcut(shortcut::Action::press   (&[Key::Alt])                                 , "toggle_node_subtract_select")
             , Self::self_shortcut(shortcut::Action::release (&[Key::Alt])                                 , "toggle_node_subtract_select")
             , Self::self_shortcut(shortcut::Action::press   (&[Key::Shift,Key::Alt])                      , "toggle_node_inverse_select")
             , Self::self_shortcut(shortcut::Action::release (&[Key::Shift,Key::Alt])                      , "toggle_node_inverse_select")
             , Self::self_shortcut(shortcut::Action::press   (&[Key::Character("d".into())])               , "debug_set_data_for_selected_node")
             , Self::self_shortcut(shortcut::Action::press   (&[Key::Control, Key::Character("f".into())]) , "debug_cycle_visualization_for_selected_node")
        ]
    }
}

impl application::View for GraphEditor {
    fn new(world: &World) -> Self {
        new_graph_editor(world)
    }
}

fn enable_disable_toggle
(network:&frp::Network, enable:&frp::Source, disable:&frp::Source, toggle:&frp::Source)
-> frp::Stream<bool> {
    // FIXME: the clone_refs bellow should not be needed.
    let enable  = enable.clone_ref();
    let disable = disable.clone_ref();
    let toggle  = toggle.clone_ref();
    frp::extend! { network
        out        <- any(...);
        on_toggle  <- toggle.map2(&out,|_,t| !t);
        on_enable  <- enable.constant(true);
        on_disable <- disable.constant(false);
        out        <+ on_toggle;
        out        <+ on_enable;
        out        <+ on_disable;
    }
    out.into()
}

#[derive(Clone,Copy,Debug,Eq,PartialEq)]
pub enum SelectionMode {
    Normal,Multi,Merge,Subtract,Inverse
}

impl SelectionMode {
    pub fn single_should_select(self, was_selected:bool) -> bool {
        match self {
            Self::Normal  => true,
            Self::Merge   => true,
            Self::Multi   => !was_selected,
            Self::Inverse => !was_selected,
            _             => false
        }
    }

    pub fn single_should_deselect(self, was_selected:bool) -> bool {
        match self {
            Self::Subtract => true,
            Self::Multi    => was_selected,
            Self::Inverse  => was_selected,
            _              => false
        }
    }
}

impl Default for SelectionMode {
    fn default() -> Self {
        Self::Normal
    }
}

#[allow(unused_parens)]
fn new_graph_editor(world:&World) -> GraphEditor {
    let scene  = world.scene();
    let cursor = component::Cursor::new(world.scene());
    web::body().set_style_or_panic("cursor","none");
    world.add_child(&cursor);

    let model                  = GraphEditorModelWithNetwork::new(scene,cursor.clone_ref());
    let network                = &model.network;
    let nodes                  = &model.nodes;
    let edges                  = &model.edges;
    let inputs                 = &model.frp;
    let mouse                  = &scene.mouse.frp;
    let touch                  = &model.touch_state;
    let visualization_registry = visualization::Registry::with_default_visualizations();
    let logger                 = &model.logger;


    let outputs = UnsealedFrpOutputs::new();
    let sealed_outputs = outputs.seal(); // Done here to keep right eval order.


    // === Selection Target Redirection ===
    frp::extend! { network
    mouse_down_target <- mouse.press.map(f_!(model.scene.mouse.target.get()));
    mouse_up_target   <- mouse.release.map(f_!(model.scene.mouse.target.get()));
    background_up     <- mouse_up_target.map(|t| if t==&display::scene::Target::Background {Some(())} else {None}).unwrap();

    eval mouse_down_target([touch,model](target) {
        match target {
            display::scene::Target::Background  => touch.background.down.emit(()),
            display::scene::Target::Symbol {..} => {
                if let Some(target) = model.scene.shapes.get_mouse_target(*target) {
                    target.mouse_down().emit(());
                }
            }
        }
    });
    }


    // === Cursor Selection ===
    frp::extend! { network

    mouse_on_down_position <- mouse.position.sample(&mouse.press);
    selection_size_down    <- mouse.position.map2(&mouse_on_down_position,|m,n|{m-n});
    selection_size         <- selection_size_down.gate(&touch.background.is_down);

    on_press_style   <- mouse.press   . constant(cursor::Style::new_press());
    on_release_style <- mouse.release . constant(cursor::Style::default());


    cursor_selection_start <- selection_size.map(|p| cursor::Style::new_box_selection(Vector2::new(p.x,p.y)));
    cursor_selection_end   <- mouse.release . constant(cursor::Style::default());

    cursor_selection <- any (cursor_selection_start, cursor_selection_end);

    cursor_press     <- any (on_press_style, on_release_style);


    }


    // === Cursor Color ===
    frp::extend! { network

    let style = cursor::Style::new_color_no_animation(color::Lcha::new(0.6,0.5,0.76,1.0)).press();
    cursor_style_on_edge_drag      <- outputs.some_edge_targets_detached.constant(style);
    cursor_style_on_edge_drag_stop <- outputs.all_edge_targets_attached.constant(default());
    cursor_style_edge_drag         <- any (cursor_style_on_edge_drag,cursor_style_on_edge_drag_stop);





    }



    // === Node Select ===
    frp::extend! { network

    deselect_all_nodes <- any_(...);

    let multi_select_flag = enable_disable_toggle
        ( network
        , &inputs.enable_node_multi_select
        , &inputs.disable_node_multi_select
        , &inputs.toggle_node_multi_select
        );

    let merge_select_flag = enable_disable_toggle
        ( network
        , &inputs.enable_node_merge_select
        , &inputs.disable_node_merge_select
        , &inputs.toggle_node_merge_select
        );

    let subtract_select_flag = enable_disable_toggle
        ( network
        , &inputs.enable_node_subtract_select
        , &inputs.disable_node_subtract_select
        , &inputs.toggle_node_subtract_select
        );

    let inverse_select_flag = enable_disable_toggle
        ( network
        , &inputs.enable_node_inverse_select
        , &inputs.disable_node_inverse_select
        , &inputs.toggle_node_inverse_select
        );


    selection_mode <- all_with4
        (&multi_select_flag,&merge_select_flag,&subtract_select_flag,&inverse_select_flag,
        |multi,merge,subtract,inverse| {
            if      *multi    { SelectionMode::Multi }
            else if *merge    { SelectionMode::Merge }
            else if *subtract { SelectionMode::Subtract }
            else if *inverse  { SelectionMode::Inverse }
            else              { SelectionMode::Normal }
        }
    );

    let node_pressed = touch.nodes.selected.clone_ref();


    node_was_selected <- node_pressed.map(f!((id) model.nodes.selected.contains(id)));

    should_select <- node_pressed.map3(&selection_mode,&node_was_selected,
        |_,mode,was_selected| mode.single_should_select(*was_selected)
    );

    should_deselect <- node_pressed.map3(&selection_mode,&node_was_selected,
        |_,mode,was_selected| mode.single_should_deselect(*was_selected)
    );

    keep_selection          <- selection_mode.map(|t| *t != SelectionMode::Normal);
    deselect_on_select      <- node_pressed.gate_not(&keep_selection);
    deselect_all_nodes      <+ deselect_on_select;
    deselect_all_nodes      <+ inputs.deselect_all_nodes;

    deselect_on_bg_press    <- touch.background.selected.gate_not(&keep_selection);
    deselect_all_nodes      <+ deselect_on_bg_press;
    all_nodes_to_deselect   <= deselect_all_nodes.map(f_!(model.nodes.selected.mem_take()));
    outputs.node_deselected <+ all_nodes_to_deselect;

    node_selected           <- node_pressed.gate(&should_select);
    node_deselected         <- node_pressed.gate(&should_deselect);
    outputs.node_selected   <+ node_selected;
    outputs.node_deselected <+ node_deselected;
    }


    // === Add Node ===
    frp::extend! { network

    node_cursor_style <- source::<cursor::Style>();


    let node_output_touch = TouchNetwork::<NodeId>::new(&network,&mouse);
    trace node_output_touch.selected;

    on_connect_drag_mode   <- node_output_touch.down.constant(true);
    on_connect_follow_mode <- node_output_touch.selected.constant(false);
    connect_drag_mode      <- any (on_connect_drag_mode,on_connect_follow_mode);

    new_edge <- node_output_touch.down.map(f_!(model.new_edge()));

    outputs.edge_added <+ new_edge;
    new_edge_source <- new_edge.map2(&node_output_touch.down, move |id,node_id| (*id,EdgeTarget::new(node_id,default())));
    outputs.edge_source_set <+ new_edge_source;


    let add_node_at_cursor = inputs.add_node_at_cursor.clone_ref();
    add_node           <- any (inputs.add_node, add_node_at_cursor);
    new_node           <- add_node.map(f_!([model,node_cursor_style] model.new_node(&node_cursor_style,&node_output_touch.down)));
    outputs.node_added <+ new_node;

    node_with_position <- add_node_at_cursor.map3(&new_node,&mouse.position,|_,id,pos| (*id,*pos));
    outputs.node_position_set         <+ node_with_position;
    outputs.node_position_set_batched <+ node_with_position;

    cursor_style <- all
        [ cursor_selection
        , cursor_press
        , cursor_style_edge_drag
        , node_cursor_style
        ].fold();

    eval cursor_style ((style) cursor.frp.set_style.emit(style));
    }



    // === Node Connect ===

    frp::extend! { network

    outputs.edge_source_set <+ inputs.set_edge_source;
    outputs.edge_target_set <+ inputs.set_edge_target;

    let endpoints            = inputs.connect_nodes.clone_ref();
    edge                    <- endpoints . map(f_!(model.new_edge()));
    new_edge_source         <- endpoints . _0() . map2(&edge, |t,id| (*id,t.clone()));
    new_edge_target         <- endpoints . _1() . map2(&edge, |t,id| (*id,t.clone()));
    outputs.edge_added      <+ edge;
    outputs.edge_source_set <+ new_edge_source;
    outputs.edge_target_set <+ new_edge_target;


    port_mouse_up <- inputs.hover_node_input.sample(&mouse.release).unwrap();

    attach_all_edges        <- any (port_mouse_up, inputs.press_node_input, inputs.set_detached_edge_targets);
    detached_edge           <= attach_all_edges.map(f_!(model.take_edges_with_detached_targets()));
    new_edge_target         <- detached_edge.map2(&attach_all_edges, |id,t| (*id,t.clone()));
    outputs.edge_target_set <+ new_edge_target;

    overlapping_edges       <= outputs.edge_target_set._1().map(f!((t) model.overlapping_edges(t)));
    outputs.edge_removed    <+ overlapping_edges;

    drop_on_bg_up  <- background_up.gate(&connect_drag_mode);
    drop_edges     <- any (drop_on_bg_up,touch.background.down);
    edge_to_drop   <= drop_edges.map(f_!(model.take_edges_with_detached_targets()));
    eval edge_to_drop ((id) model.remove_edge(id));

    }



    // === Remove Node ===
    frp::extend! { network

    all_nodes       <= inputs.remove_all_nodes      . map(f_!(model.all_nodes()));
    selected_nodes  <= inputs.remove_selected_nodes . map(f_!(model.selected_nodes()));
    nodes_to_remove <- any (all_nodes, selected_nodes);
    eval nodes_to_remove ((node_id) inputs.remove_all_node_edges.emit(node_id));

    outputs.node_removed <+ nodes_to_remove;
    }


    // === Remove Edge ===
    frp::extend! { network

    rm_input_edges       <- any (inputs.remove_all_node_edges, inputs.remove_all_node_input_edges);
    rm_output_edges      <- any (inputs.remove_all_node_edges, inputs.remove_all_node_output_edges);
    input_edges_to_rm    <= rm_input_edges  . map(f!((node_id) model.node_in_edges(node_id)));
    output_edges_to_rm   <= rm_output_edges . map(f!((node_id) model.node_out_edges(node_id)));
    edges_to_rm          <- any (inputs.remove_edge, input_edges_to_rm, output_edges_to_rm);
    outputs.edge_removed <+ edges_to_rm;
    }


    // === Set Node Expression ===
    frp::extend! { network

    outputs.node_expression_set <+ inputs.set_node_expression;


    // === Move Nodes ===

    node_drag         <- mouse.translation.gate(&touch.nodes.is_down);
    was_selected      <- touch.nodes.down.map(f!((id) model.nodes.selected.contains(id)));
    tx_sel_nodes      <- any (node_drag, inputs.translate_selected_nodes);
    non_selected_drag <- tx_sel_nodes.map2(&touch.nodes.down,|_,id|vec![*id]).gate_not(&was_selected);
    selected_drag     <- tx_sel_nodes.map(f_!(model.nodes.selected.keys())).gate(&was_selected);
    nodes_to_drag     <- any (non_selected_drag, selected_drag);
    node_to_drag      <= nodes_to_drag;
    node_new_pos      <- node_to_drag.map2(&tx_sel_nodes,f!((id,tx) model.node_pos_mod(id,tx)));
    outputs.node_position_set <+ node_new_pos;

    was_drag_false        <- touch.nodes.down.constant(false);
    was_drag_true         <- node_drag.constant(true);
    was_drag              <- any (was_drag_false,was_drag_true);
    drag_finish           <- touch.nodes.up.gate(&was_drag);
    dragged_node          <= nodes_to_drag.sample(&drag_finish);
    dragged_node_pos      <- dragged_node.map(f!([model] (id) (*id,model.node_position(id))));
    outputs.node_position_set_batched <+ dragged_node_pos;


    // === Set Node Position ===

    outputs.node_position_set         <+ inputs.set_node_position;
    outputs.node_position_set_batched <+ inputs.set_node_position;
    eval outputs.node_position_set (((id,pos)) model.set_node_position(id,pos));


    // === Move Edges ===

    cursor_pos_on_detach    <- cursor.frp.position.sample(&inputs.some_edge_targets_detached);
    edge_refresh_cursor_pos <- any (cursor_pos_on_detach,cursor.frp.position);
    eval edge_refresh_cursor_pos ([edges](position) {
        edges.detached_target.for_each(|id| {
            if let Some(edge) = edges.get_cloned_ref(id) {
                edge.view.frp.target_position.emit(frp::Position::new(position.x,position.y))
            }
        })
    });

     // === Vis Cycling ===
     def _cycle_vis = inputs.debug_cycle_visualization_for_selected_node.map(f!([inputs,nodes](_) {
        nodes.selected.for_each(|node| inputs.cycle_visualization.emit(node));
    }));


    // === Vis Set ===

    def _update_vis_data = inputs.set_visualization.map(f!([nodes]((node_id,vis)) {
        if let Some(node) = nodes.get_cloned_ref(node_id) {
            node.view.visualization_container.frp.set_visualization.emit(vis)
        }
    }));

    // === Vis Update Data ===

    // TODO remove this once real data is available.
    let sample_data_generator = MockDataGenerator3D::default();
    def _set_dumy_data = inputs.debug_set_data_for_selected_node.map(f!([nodes,inputs](_) {
        nodes.selected.for_each(|node_id| {
            let data          = Rc::new(sample_data_generator.generate_data());
            let content       = Rc::new(serde_json::to_value(data).unwrap());
            let data          = visualization::Data::JSON{ content };
            inputs.set_visualization_data.emit((*node_id,Some(data)));
        })
    }));

    def _set_data = inputs.set_visualization_data.map(f!([nodes]((node_id,data)) {
         if let Some(node) = nodes.get_cloned(node_id) {
                node.view.visualization_container.frp.set_data.emit(data);
         }
     }));

     let cycle_count = Rc::new(Cell::new(0));
     def _cycle_visualization = inputs.cycle_visualization.map(f!([scene,nodes,visualization_registry,logger](node_id) {
        let visualizations = visualization_registry.valid_sources(&"[[Float,Float,Float]]".into());
        cycle_count.set(cycle_count.get() % visualizations.len());
        let vis  = &visualizations[cycle_count.get()];
        let vis  = vis.instantiate(&scene);
        let node = nodes.get_cloned_ref(node_id);
        match (vis, node) {
            (Ok(vis), Some(node))  => {
                    node.view.visualization_container.frp.set_visualization.emit(Some(vis));
            },
            (Err(e), _) =>  logger.warning(|| format!("Failed to cycle visualization: {}", e)),
            _           => {}
        };
        cycle_count.set(cycle_count.get() + 1);
    }));

    def on_visualization_enabled  = source();
    def on_visualization_disabled = source();

    def _toggle_selected = inputs.toggle_visualization_visibility.map(f!([nodes,on_visualization_enabled,on_visualization_disabled](_) {
        nodes.selected.for_each(|node_id| {
            if let Some(node) = nodes.get_cloned_ref(node_id) {
                node.view.visualization_container.frp.toggle_visibility.emit(());
                if node.view.visualization_container.is_visible() {
                    on_visualization_enabled.emit(node_id);
                } else {
                    on_visualization_disabled.emit(node_id);
                }
            }
        });
    }));

    outputs.visualization_enabled  <+ on_visualization_enabled;
    outputs.visualization_disabled <+ on_visualization_disabled;

    // === Register Visualization ===

    def _register_visualization = inputs.register_visualization_class.map(f!([visualization_registry](handle) {
        if let Some(handle) = handle {
            visualization_registry.register_class_from_handle(&handle);
        }
    }));


    // === OUTPUTS REBIND ===

    outputs.some_edge_targets_detached <+ inputs.some_edge_targets_detached;
    outputs.all_edge_targets_attached  <+ inputs.all_edge_targets_attached;

    eval outputs.edge_source_set     (((id,tgt)) model.set_edge_source(*id,tgt));
    eval outputs.edge_target_set     (((id,tgt)) model.set_edge_target(*id,tgt));
    eval outputs.node_selected       ((id) model.select_node(id));
    eval outputs.node_deselected     ((id) model.deselect_node(id));
    eval outputs.edge_removed        ((id) model.remove_edge(id));
    eval outputs.node_removed        ((id) model.remove_node(id));
    eval outputs.node_expression_set (((id,expr)) model.set_node_expression(id,expr));



    // === Edge discovery ===

    edge_endpoint_set          <- any (outputs.edge_source_set, outputs.edge_target_set)._0();
    both_endpoints_set         <- edge_endpoint_set.map(f!((id) model.is_connection(id)));
    new_connection             <- edge_endpoint_set.gate(&both_endpoints_set);
    outputs.connection_added   <+ new_connection;
    outputs.connection_removed <+ outputs.edge_removed;



    // === Status ===

    def is_active_src = source::<bool>();
    def is_empty_src  = source::<bool>();
    def is_active = is_active_src.sampler();
    def is_empty  = is_empty_src.sampler();

    // === Remove implementation ===
    outputs.node_removed <+ inputs.remove_node;

    }

    // FIXME This is a temporary solution. Should be replaced by a real thing once layout
    //       management is implemented.
    is_active_src.emit(true);

    let status = FrpStatus {is_active,is_empty};

    let node_release = touch.nodes.up.clone_ref();


    let inputs = inputs.clone_ref();
    let outputs = sealed_outputs;
    let frp = Frp {inputs,outputs,status,node_release};

    GraphEditor {model,frp}
}




impl display::Object for GraphEditor {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
