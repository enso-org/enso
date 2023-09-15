//! NOTE
//! This file is under a heavy development. It contains commented lines of code and some code may
//! be of poor quality. Expect drastic changes.

// === Features ===
#![feature(associated_type_defaults)]
#![feature(cell_update)]
#![feature(const_trait_impl)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(hash_drain_filter)]
#![feature(let_chains)]
#![feature(macro_metavar_expr)]
#![feature(option_result_contains)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(array_windows)]
#![feature(if_let_guard)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
#![allow(incomplete_features)] // To be removed, see: https://github.com/enso-org/ide/issues/1559
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "1024"]

#[warn(missing_docs)]
pub mod component;

pub mod automation;
pub mod builtin;
pub mod data;
pub mod execution_environment;
pub mod new_node_position;
#[warn(missing_docs)]
pub mod view;

mod layers;
#[warn(missing_docs)]
mod selection;
mod shortcuts;

use crate::application::command::FrpNetworkProvider;
use crate::component::node;
use crate::component::visualization;
use crate::component::visualization::instance::PreprocessorConfiguration;
use crate::data::enso;
use engine_protocol::language_server::ExecutionEnvironment;

use application::tooltip;
use enso_frp as frp;
use ensogl::application;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::display::object::Id;
use ensogl::display::shape::StyleWatchFrp;
use ensogl::display::Scene;
use ensogl::gui::cursor;
use ensogl::prelude::*;
use ensogl::system::web;
use ensogl::system::web::traits::*;
use ensogl::DEPRECATED_Animation;
use ensogl::Easing;
use ensogl_component::text;
use ensogl_component::text::buffer::selection::Selection;
use ensogl_component::tooltip::Tooltip;
use ensogl_hardcoded_theme as theme;
use span_tree::PortId;



// ==============
// === Export ===
// ==============

pub use layers::GraphLayers;



// ===============
// === Prelude ===
// ===============

/// Commonly used utilities.
pub mod prelude {
    pub use ensogl::application::command::View;
    pub use ensogl::prelude::*;
}



// =================
// === Constants ===
// =================

const SNAP_DISTANCE_THRESHOLD: f32 = 10.0;
/// Time between key down and key up event to consider it a press and hold action as opposed to a
/// simple key press.
const VIZ_PREVIEW_MODE_TOGGLE_TIME_MS: f32 = 300.0;
/// Number of frames we expect to pass during the `VIZ_PREVIEW_MODE_TOGGLE_TIME_MS` interval.
/// Assumes 60fps. We use this value to check against dropped frames during the interval.
const VIZ_PREVIEW_MODE_TOGGLE_FRAMES: i32 =
    (VIZ_PREVIEW_MODE_TOGGLE_TIME_MS / 1000.0 * 60.0) as i32;
const MAX_ZOOM: f32 = 1.0;
/// The amount of pixels that the dragged target edge overlaps with the cursor.
const CURSOR_EDGE_OVERLAP: f32 = 2.0;



// =================
// === SharedVec ===
// =================

#[derive(CloneRef, Debug, Derivative)]
#[derivative(Default(bound = ""))]
#[derivative(Clone(bound = ""))]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct SharedVec<T> {
    pub raw: Rc<RefCell<Vec<T>>>,
}

impl<T> SharedVec<T> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Append an element to the back of a collection.
    pub fn push(&self, t: T) {
        self.raw.borrow_mut().push(t);
    }

    /// Remove the first instance of `item` from the vector if the item exists.
    pub fn remove_item(&self, t: &T)
    where T: PartialEq {
        self.raw.borrow_mut().remove_item(t);
    }

    /// Return `true` if the slice contains an element with the given value.
    pub fn contains(&self, t: &T) -> bool
    where T: PartialEq {
        self.raw.borrow().contains(t)
    }

    /// Return clone of the first element of the slice, or `None` if it is empty.
    pub fn first_cloned(&self) -> Option<T>
    where T: Clone {
        self.raw.borrow().first().cloned()
    }

    /// Return clone of the last element of the slice, or `None` if it is empty.
    pub fn last_cloned(&self) -> Option<T>
    where T: Clone {
        self.raw.borrow().last().cloned()
    }

    /// Replace the collection with the default value, and return the previous value.
    pub fn mem_take(&self) -> Vec<T> {
        mem::take(&mut self.raw.borrow_mut())
    }

    /// Return the number of items in the vector.
    pub fn len(&self) -> usize {
        self.raw.borrow().len()
    }

    /// Check if the container is empty.
    pub fn is_empty(&self) -> bool {
        self.raw.borrow().is_empty()
    }
}

impl<T: Clone> SharedVec<T> {
    /// Return a vector of all items stored in the collection in order.
    pub fn items(&self) -> Vec<T> {
        self.raw.borrow().clone()
    }
}



// =====================
// === SharedHashSet ===
// =====================

#[derive(Derivative, CloneRef)]
#[derivative(Debug(bound = "T:Eq+Hash+Debug, S:std::hash::BuildHasher"))]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct SharedHashSet<T, S = std::collections::hash_map::RandomState> {
    pub raw: Rc<RefCell<HashSet<T, S>>>,
}

impl<T, S> Clone for SharedHashSet<T, S> {
    fn clone(&self) -> Self {
        let raw = self.raw.clone();
        Self { raw }
    }
}

impl<T, S> Default for SharedHashSet<T, S>
where
    T: Eq + Hash,
    S: Default + std::hash::BuildHasher,
{
    fn default() -> Self {
        let raw = default();
        Self { raw }
    }
}

impl<T, S> SharedHashSet<T, S>
where
    T: Eq + Hash,
    S: Default + std::hash::BuildHasher,
{
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn new() -> Self {
        default()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn mem_take(&self) -> HashSet<T, S> {
        mem::take(&mut *self.raw.borrow_mut())
    }
}

impl<T, S> SharedHashSet<T, S>
where
    T: Eq + Hash,
    S: std::hash::BuildHasher,
{
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn insert(&self, t: T) -> bool {
        self.raw.borrow_mut().insert(t)
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn remove(&self, t: &T) -> bool {
        self.raw.borrow_mut().remove(t)
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn contains(&self, value: &T) -> bool {
        self.raw.borrow().contains(value)
    }
}

impl<T, S> SharedHashSet<T, S> {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn is_empty(&self) -> bool {
        self.raw.borrow().is_empty()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn clear(&self) {
        self.raw.borrow_mut().clear()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn for_each<F>(&self, f: F)
    where F: FnMut(&T) {
        self.raw.borrow_mut().iter().for_each(f)
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn replace_with(&self, t: HashSet<T, S>) {
        *self.raw.borrow_mut() = t;
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn keys(&self) -> Vec<T>
    where T: Clone {
        self.raw.borrow().iter().cloned().collect_vec()
    }
}



// =====================
// === SharedHashMap ===
// =====================

#[derive(Derivative, CloneRef)]
#[derivative(Debug(bound = "K:Eq+Hash+Debug, V:Debug, S:std::hash::BuildHasher"))]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct SharedHashMap<K, V, S = std::collections::hash_map::RandomState> {
    pub raw: Rc<RefCell<HashMap<K, V, S>>>,
}

impl<K, V, S> Clone for SharedHashMap<K, V, S> {
    fn clone(&self) -> Self {
        let raw = self.raw.clone();
        Self { raw }
    }
}

impl<K, V, S> Default for SharedHashMap<K, V, S>
where
    K: Eq + Hash,
    S: Default + std::hash::BuildHasher,
{
    fn default() -> Self {
        let raw = default();
        Self { raw }
    }
}

impl<K, V, S> SharedHashMap<K, V, S>
where
    K: Eq + Hash,
    S: Default + std::hash::BuildHasher,
{
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn new() -> Self {
        default()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn mem_take(&self) -> HashMap<K, V, S> {
        mem::take(&mut *self.raw.borrow_mut())
    }
}

impl<K, V, S> SharedHashMap<K, V, S>
where
    K: Eq + Hash,
    S: std::hash::BuildHasher,
{
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn insert(&self, k: K, v: V) -> Option<V> {
        self.raw.borrow_mut().insert(k, v)
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn get_copied(&self, k: &K) -> Option<V>
    where V: Copy {
        self.with(k, |v| *v)
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn get_cloned(&self, k: &K) -> Option<V>
    where V: Clone {
        self.with(k, |v| v.clone())
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn get_cloned_ref(&self, k: &K) -> Option<V>
    where V: CloneRef {
        self.with(k, |v| v.clone_ref())
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn remove(&self, k: &K) -> Option<V> {
        self.raw.borrow_mut().remove(k)
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn contains_key(&self, key: &K) -> bool {
        self.raw.borrow().contains_key(key)
    }

    /// Performs a lookup in the map and call a function on the value if it exists. The map will be
    /// borrowed for the duration of the function call.
    pub fn with<R>(&self, k: &K, f: impl FnOnce(&V) -> R) -> Option<R> {
        self.raw.borrow().get(k).map(f)
    }
}

impl<K, V, S> SharedHashMap<K, V, S> {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn len(&self) -> usize {
        self.raw.borrow().len()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn is_empty(&self) -> bool {
        self.raw.borrow().is_empty()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn clear(&self) {
        self.raw.borrow_mut().clear()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn for_each<F>(&self, f: F)
    where F: FnMut((&K, &V)) {
        self.raw.borrow_mut().iter().for_each(f)
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn keys(&self) -> Vec<K>
    where K: Clone {
        self.raw.borrow().keys().cloned().collect_vec()
    }

    /// Get the vector of map's keys and values.
    pub fn entries(&self) -> Vec<(K, V)>
    where
        K: Clone,
        V: CloneRef, {
        self.raw.borrow().iter().map(|(k, v)| (k.clone(), v.clone_ref())).collect_vec()
    }

    /// Get the vector of map's values.
    pub fn values(&self) -> Vec<V>
    where V: Clone {
        self.raw.borrow().values().cloned().collect_vec()
    }
}



// =================
// === FrpInputs ===
// =================

/// The information about data source hinted by node creation process. For example, when creating
/// node by dropping edge, the source port should be a source for newly created node.
///
/// This is information meant to be sent to searcher, which can, for example, auto- connect the
/// source to "this" port of new node.
#[derive(Clone, CloneRef, Copy, Debug, Default, Eq, PartialEq)]
pub struct NodeSource {
    #[allow(missing_docs)]
    pub node: NodeId,
}

ensogl::define_endpoints_2! {
    Input {
        // === Layout ===

        /// The offset in the x-axis at which the part of the top bar of the graph editor should
        /// start.
        graph_editor_top_bar_offset_x (f32),


        // === Read-only mode ===

        set_read_only(bool),


        // === Edges ===

        set_connections(Vec<Connection>),

        // === Node Selection ===

        /// Node press event
        node_press(),
        /// Node press event
        node_release(),
        /// Enable nodes multi selection mode. It works like inverse mode for single node selection
        /// and like merge mode for multi node selection mode.
        enable_node_multi_select(),
        /// Disable nodes multi selection mode. It works like inverse mode for single node selection
        /// and like merge mode for multi node selection mode.
        disable_node_multi_select(),
        /// Toggle nodes multi selection mode. It works like inverse mode for single node selection
        /// and like merge mode for multi node selection mode.
        toggle_node_multi_select(),

        /// Enable nodes merge selection mode.
        enable_node_merge_select(),
        /// Disable nodes merge selection mode.
        disable_node_merge_select(),
        /// Toggles nodes merge selection mode.
        toggle_node_merge_select(),

        /// Enable nodes subtract selection mode.
        enable_node_subtract_select(),
        /// Disable nodes subtract selection mode.
        disable_node_subtract_select(),
        /// Toggle nodes subtract selection mode.
        toggle_node_subtract_select(),

        /// Enable nodes inverse selection mode.
        enable_node_inverse_select(),
        /// Disable nodes inverse selection mode.
        disable_node_inverse_select(),
        /// Toggle nodes inverse selection mode.
        toggle_node_inverse_select(),

        /// Set the node as selected. Ignores selection mode.
        // WARNING: not implemented
        select_node                  (NodeId),
        /// Set the node as deselected. Ignores selection mode.
        // WARNING: not implemented
        deselect_node                (NodeId),
        /// Set all nodes as selected. Ignores selection mode.
        select_all_nodes             (),


        // === Navigation ===

        /// Enter the last selected node.
        enter_selected_node(),
        /// Enter the node currently under the cursor.
        enter_hovered_node(),
        /// Steps out of the current node, popping the topmost stack frame from the crumb list.
        exit_node(),


        // === Node Editing ===

        /// Add a new node and place it in the origin of the workspace.
        add_node(),
        /// Start Node creation process.
        ///
        /// This event is the best to be emit in situations, when the user want to create node (in
        /// opposition to e.g. loading graph from file). It will create node and put it into edit
        /// mode. The node position may vary, depending on what is the best for the UX - for details
        /// see [`GraphEditorModel::create_node`] implementation.
        start_node_creation(),
        /// Start creation of a new Node connected to the port that is currently under the cursor.
        /// If the cursor is currently not over any node's port, this event will have no effect.
        ///
        /// The same as in the case of [`start_node_creation`], this event is intended to be
        /// emitted in situations when the user wants to interactively create a node via the UI (as
        /// opposed to e.g. when loading a graph from a file).
        start_node_creation_from_port(),

        // === Copy-Paste ===
        copy_selected_node(),
        paste_node(),


        /// Remove all selected nodes from the graph.
        remove_selected_nodes(),
        /// Remove all nodes from the graph.
        remove_all_nodes(),
        /// Enable mode in which the pressed node will be edited.
        edit_mode_on(),
        /// Disable mode in which the pressed node will be edited.
        edit_mode_off(),
        /// Stop node editing, whatever node is currently edited.
        stop_editing(),
        /// Collapse the selected nodes into a new node.
        collapse_selected_nodes(),
        /// Indicate whether this node had an error or not.
        set_node_error_status(NodeId, Option<node::error::Error>),
        /// Indicate whether this node has finished execution.
        set_node_pending_status(NodeId, bool),


        // === Visualization ===

        /// Simulates a visualization open press event. In case the event will be shortly followed
        /// by `release_visualization_visibility`, the visualization will be shown permanently. In
        /// other case, it will be disabled as soon as the `release_visualization_visibility` is
        /// emitted.
        press_visualization_visibility(),
        /// Simulates a visualization open release event. See `press_visualization_visibility` to
        /// learn more.
        release_visualization_visibility(),
        /// Cycle the visualization for the selected nodes.
        cycle_visualization_for_selected_node(),
        /// Opens the visualization for the selected node in full-screen mode.
        open_fullscreen_visualization(),
        /// The visualization currently displayed as fullscreen is
        close_fullscreen_visualization(),


        // === Scene Navigation ===

        /// Stop the scene camera from moving around, locking the scene in place.
        /// Can be used, e.g., if there is a fullscreen visualization active, or navigation should
        ///only work for a selected visualization.
        set_navigator_disabled(bool),


        // === Execution Environment ===

        /// Set the execution environments available to the graph.
        set_available_execution_environments          (Rc<Vec<ExecutionEnvironment>>),
        switch_to_design_execution_environment(),
        switch_to_live_execution_environment(),
        execution_complete(),


        // === Debug ===

        /// Enable or disable debug-only features.
        set_debug_mode(bool),

        /// Set a test visualization data for the selected nodes. Useful for testing visualizations
        /// during their development.
        debug_set_test_visualization_data_for_selected_node(),
        /// Reopen file in language server.
        ///
        /// Used as a debug or a fallback for the user when synchronization errors are spotted.
        reopen_file_in_language_server(),


        // === VCS Status ===

        set_node_vcs_status     ((NodeId, Option<node::vcs::Status>)),


        deselect_all_nodes           (),
        remove_node                  (NodeId),
        edit_node                    (NodeId),
        collapse_nodes               ((Vec<NodeId>,NodeId)),
        set_node_expression          ((NodeId,node::Expression)),
        edit_node_expression         ((NodeId, text::Range<text::Byte>, ImString)),
        set_node_skip                ((NodeId,bool)),
        set_node_freeze              ((NodeId,bool)),
        /// Set whether the output context is explicitly enabled for a node: `Some(true/false)` for
        /// enabled/disabled; `None` for no context switch expression.
        set_node_context_switch      ((NodeId, Option<bool>)),
        set_node_comment             ((NodeId,ImString)),
        set_node_position            ((NodeId,Vector2)),
        set_expression_usage_type    ((NodeId,ast::Id,Option<Type>)),
        update_node_widgets          ((NodeId,CallWidgetsConfig)),
        cycle_visualization          (NodeId),
        set_visualization            ((NodeId, Option<visualization::Path>)),
        register_visualization       (Option<visualization::Definition>),
        set_visualization_data       ((NodeId, visualization::Data)),
        set_error_visualization_data ((NodeId, visualization::Data)),
        enable_visualization         (NodeId),
        disable_visualization        (NodeId),
        /// Inform Graph Editor that attaching or updating visualization has resulted in error.
        visualization_update_failed  ((NodeId, String)),

        /// Remove from visualization registry all non-default visualizations.
        reset_visualization_registry (),
        /// Reload visualization registry
        reload_visualization_registry(),
        /// Show visualization previews on nodes without delay.
        enable_quick_visualization_preview(),
        /// Show visualization previews on nodes with delay.
        disable_quick_visualization_preview(),
        /// Enable editing preview on given node.
        show_node_editing_preview(NodeId),

        /// Drop an edge that is being dragged.
        drop_dragged_edge            (),
    }

    Output {
        // === Debug Mode ===

        debug_mode (bool),


        // === Read-only mode ===

        read_only (bool),

        // === Edge ===

        has_detached_edge (bool),
        hover_node_input (Option<EdgeEndpoint>),
        hover_node_output (Option<EdgeEndpoint>),

        // === Node ===

        node_added                 (NodeId, Option<NodeSource>, bool),
        node_removed               (NodeId),
        nodes_collapsed            ((Vec<NodeId>, NodeId)),
        node_hovered               (Switch<NodeId>),
        node_selected              (NodeId),
        node_deselected            (NodeId),
        node_position_set          ((NodeId,Vector2)),
        node_position_set_batched  ((NodeId,Vector2)),
        node_expression_set        ((NodeId,ImString)),
        node_expression_span_set   ((NodeId, span_tree::Crumbs, ImString)),
        node_expression_edited     ((NodeId,ImString,Vec<Selection<text::Byte>>)),
        node_comment_set           ((NodeId,ImString)),
        node_entered               (NodeId),
        node_exited                (),
        node_editing_started       (NodeId),
        node_editing_finished      (NodeId),
        node_action_context_switch ((NodeId, bool)),
        node_action_freeze         ((NodeId, bool)),
        node_action_skip           ((NodeId, bool)),
        node_edit_mode             (bool),
        nodes_labels_visible       (bool),
        node_incoming_edge_updates (NodeId),
        node_outgoing_edge_updates (NodeId),
        node_widget_tree_rebuilt   (NodeId),

        // === Visualization ===

        /// `None` value as a visualization path denotes a disabled visualization.
        enabled_visualization_path              (NodeId,Option<visualization::Path>),
        visualization_shown                     (NodeId,visualization::Metadata),
        visualization_hidden                    (NodeId),
        visualization_fullscreen                (Option<NodeId>),
        is_fs_visualization_displayed           (bool),
        visualization_preprocessor_changed      ((NodeId,PreprocessorConfiguration)),
        visualization_registry_reload_requested (),
        visualization_update_error ((NodeId, String)),

        on_visualization_select     (Switch<NodeId>),
        some_visualization_selected (bool),
        navigator_active (bool),

        widgets_requested                       (NodeId, ast::Id, ast::Id),
        request_import                          (ImString),

        // === Edit mode ===

        node_being_edited (Option<NodeId>),
        node_editing (bool),


        // === Copy-Paste ===

        node_copied(NodeId),
        // Paste node at position.
        request_paste_node(Vector2),

        file_dropped     (ensogl_drop_manager::File,Vector2<f32>),

        connection_made (Connection),
        connection_broken (Connection),

        default_x_gap_between_nodes (f32),
        default_y_gap_between_nodes (f32),
        min_x_spacing_for_new_nodes (f32),

        /// The selected environment mode.
        execution_environment (ExecutionEnvironment),
        /// A press of the execution environment selector play button.
        execution_environment_play_button_pressed (),
    }
}

impl FrpNetworkProvider for GraphEditor {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}



// ============
// === Node ===
// ============

#[derive(Clone, CloneRef, Debug, Deref, display::Object)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct Node {
    #[deref]
    #[display_object]
    pub view:  component::Node,
    in_edges:  SharedHashSet<EdgeId>,
    out_edges: SharedHashSet<EdgeId>,
}

#[derive(Clone, CloneRef, Copy, Debug, Default, Eq, From, Hash, Into, PartialEq, Ord, PartialOrd)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct NodeId(pub Id);

impl Node {
    /// Create a new node state from a given Node view component. Note that the node assumes its
    /// view component is not shared with other nodes.
    pub fn new(view: component::Node) -> Self {
        Self { view, in_edges: default(), out_edges: default() }
    }


    /// Get the NodeId, created from the view of this node.
    pub fn id(&self) -> NodeId {
        self.view.id().into()
    }

    /// A list of edges connected to this node's input ports.
    pub fn in_edges(&self) -> Vec<EdgeId> {
        self.in_edges.keys()
    }

    /// A list of edges connected to this node's output ports.
    pub fn out_edges(&self) -> Vec<EdgeId> {
        self.out_edges.keys()
    }

    /// Return all edges connected to this node. Ingoing and outgoing both.
    pub fn all_edges(&self) -> Vec<EdgeId> {
        self.in_edges.keys().extended(self.out_edges.keys())
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}



// ============
// === Edge ===
// ============

#[derive(Debug, Deref, display::Object)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
struct Edge {
    #[deref]
    #[display_object]
    view:       component::Edge,
    color:      color::Lcha,
    source:     Option<EdgeEndpoint>,
    target:     Option<EdgeEndpoint>,
    /// The connection that is backed by this edge view. Does not necessarily have to be the same
    /// as the `source` and `target` endpoints, as the edge can be modified purely on view
    /// layer, and the connection does not have to be immediately updated.
    connection: Option<Connection>,
}

#[derive(Clone, CloneRef, Copy, Debug, Default, Eq, From, Hash, Into, PartialEq)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct EdgeId(pub Id);

impl Edge {
    /// Create a new edge with no source or target set yet.
    fn new(view: component::Edge) -> Self {
        Self { view, color: default(), source: None, target: None, connection: None }
    }

    fn id(&self) -> EdgeId {
        self.view.id().into()
    }

    fn target(&self) -> Option<EdgeEndpoint> {
        self.target
    }

    fn source(&self) -> Option<EdgeEndpoint> {
        self.source
    }

    /// Get a detached edge that would be created if this edge was detached, such that it remained
    /// connected only to its target.
    fn as_detached_at_target(&self) -> Option<DetachedEdge> {
        Some(DetachedEdge::OnlyTarget { id: Some(self.id()), target: self.target? })
    }

    /// Get a detached edge that would be created if this edge was detached, such that it remained
    /// connected only to its source.
    fn as_detached_at_source(&self) -> Option<DetachedEdge> {
        Some(DetachedEdge::OnlySource { id: Some(self.id()), source: self.source? })
    }

    /// Set this edge's source and target endpoint, update affected node's edge sets.
    fn set_endpoints(
        &mut self,
        source: Option<EdgeEndpoint>,
        target: Option<EdgeEndpoint>,
        nodes: &Nodes,
    ) -> bool {
        let mut dirty = false;
        dirty |= self.set_source(source, nodes);
        dirty |= self.set_target(target, nodes);
        dirty
    }

    /// Set this edge's target endpoint and update node's edge sets.
    fn set_target(&mut self, target: Option<EdgeEndpoint>, nodes: &Nodes) -> bool {
        let changed = self.target != target;
        if changed {
            let old_node = self.target.map(|target| target.node_id);
            let new_node = target.map(|target| target.node_id);
            nodes.update_target_endpoint(self.id(), old_node, new_node);
            self.target = target;
            self.view.target_attached.emit(target.is_some());
        }
        changed
    }

    /// Set this edge's source endpoint and update node's edge sets.
    fn set_source(&mut self, source: Option<EdgeEndpoint>, nodes: &Nodes) -> bool {
        let changed = self.source != source;
        if changed {
            let old_node = self.source.map(|source| source.node_id);
            let new_node = source.map(|source| source.node_id);
            nodes.update_source_endpoint(self.id(), old_node, new_node);
            self.source = source;
            self.view.source_attached.emit(source.is_some());
        }
        changed
    }

    fn set_color(&mut self, color: color::Lcha) -> bool {
        let changed = self.color != color;
        if changed {
            self.color = color;
            self.view.set_color.emit(color);
        }
        changed
    }
}

impl Display for EdgeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}



// ============
// === Type ===
// ============

/// Typename information that may be associated with the given Port.
///
/// `None` means that type for the port is unknown.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Type(pub ImString);

impl Deref for Type {
    type Target = ImString;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Type {
    /// Check whether this is any type, the most generic type in Enso. The empty string is
    /// considered to be an empty type as well.
    pub fn is_any(&self) -> bool {
        self.as_str() == "Any" || self.is_empty()
    }

    /// If the type consists of a single identifier then we remove all module qualifiers:
    /// ```
    /// use ide_view_graph_editor::*;
    ///
    /// let input = Type::from("Foo.Bar.Baz.Vector".to_string());
    /// let expectation = Type::from("Vector".to_string());
    /// assert_eq!(input.abbreviate(), expectation);
    /// ```
    ///
    /// If the type contains multiple identifiers then we just abbreviate the first one:
    /// ```
    /// use ide_view_graph_editor::*;
    ///
    /// let input = Type::from("Foo.Bar.Baz.Vector Math.Number".to_string());
    /// let expectation = Type::from("Vector Math.Number".to_string());
    /// assert_eq!(input.abbreviate(), expectation);
    /// ```
    pub fn abbreviate(&self) -> Type {
        if let Some(up_to_whitespace) = self.split_whitespace().next() {
            if let Some(last_dot_index) = up_to_whitespace.rfind('.') {
                Type::from(self[last_dot_index + 1..].to_string())
            } else {
                // `self` contains no dot. We do not need to abbreaviate it.
                self.clone()
            }
        } else {
            // `self` was empty.
            Type::from("".to_string())
        }
    }
}

impl From<String> for Type {
    fn from(s: String) -> Self {
        Type(s.into())
    }
}

impl From<&String> for Type {
    fn from(s: &String) -> Self {
        Type(s.into())
    }
}

impl From<&str> for Type {
    fn from(s: &str) -> Self {
        Type(s.into())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}



// ====================
// === EdgeEndpoint ===
// ====================

/// A potential edge endpoint, i.e. a node and a port on that node.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Hash)]
pub struct EdgeEndpoint {
    /// Node at which the endpoint is located.
    pub node_id: NodeId,
    /// The source or target node port. Must always be a port of node specified by `node_id`.
    pub port:    PortId,
}

impl EdgeEndpoint {
    /// Constructor
    pub fn new(node_id: NodeId, port: PortId) -> Self {
        Self { node_id, port }
    }
}



// ==================
// === Connection ===
// ==================

/// A representation of a connection between two nodes. Each connection represents one location at
/// which a graph variable is used. Roughly corresponds to edge views, but does not contain any
/// view-specific information, e.g. detached edge state.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Hash)]
pub struct Connection {
    /// Definition site of the edge variable.
    pub source: EdgeEndpoint,
    /// An usage point of the edge variable.
    pub target: EdgeEndpoint,
}



// ============
// === Grid ===
// ============

/// Defines a snapping grid for nodes. The grid implementation is currently very simple. For each
/// node, the grid records its position and allows querying for positions close to the recorded
/// ones.
#[derive(Debug, Clone, Default)]
pub struct Grid {
    sorted_xs: Vec<f32>,
    sorted_ys: Vec<f32>,
}

impl Grid {
    /// Query the grid for a close position to the provided using the provided threshold distance.
    pub fn close_to(&self, position: Vector2<f32>, threshold: f32) -> Vector2<Option<f32>> {
        let x = Self::axis_close_to(&self.sorted_xs, position.x, threshold);
        let y = Self::axis_close_to(&self.sorted_ys, position.y, threshold);
        Vector2(x, y)
    }

    fn axis_close_to(axis: &[f32], pos: f32, threshold: f32) -> Option<f32> {
        match axis.binary_search_by(|t| t.partial_cmp(&pos).unwrap()) {
            Ok(ix) => Some(axis[ix]),
            Err(ix) => {
                let max = axis.len();
                let left_pos = if ix == 0 { None } else { Some(axis[ix - 1]) };
                let right_pos = if ix == max { None } else { Some(axis[ix]) };
                let left_dist = left_pos.map(|t| (pos - t).abs());
                let right_dist = right_pos.map(|t| (pos - t).abs());
                let left_check = left_dist.map(|t| t < threshold).unwrap_or_default();
                let right_check = right_dist.map(|t| t < threshold).unwrap_or_default();
                match (left_check, right_check) {
                    (false, false) => None,
                    (true, false) => left_pos,
                    (false, true) => right_pos,
                    (true, true) => {
                        let left_dist = left_dist.unwrap_or_default();
                        let right_dist = right_dist.unwrap_or_default();
                        if left_dist < right_dist {
                            left_pos
                        } else {
                            right_pos
                        }
                    }
                }
            }
        }
    }
}



// =========================
// === CallWidgetsConfig ===
// =========================

/// Configuration for widgets of arguments at function call Enso expression.
#[derive(Debug, Default, Clone)]
pub struct CallWidgetsConfig {
    /// The function call expression ID.
    pub call_id:     ast::Id,
    /// Definition of a widget for each function argument.
    pub definitions: Rc<Vec<ArgumentWidgetConfig>>,
}

/// A structure describing a widget update for specific argument of a function call.
#[derive(Debug)]
pub struct ArgumentWidgetConfig {
    /// The function argument name that this widget is for.
    pub argument_name: String,
    /// Widget configuration queried from the language server. When this is `None`, the widget
    /// configuration should be inferred automatically.
    pub config:        Option<node::input::widget::Configuration>,
}



// =============
// === Nodes ===
// =============

#[derive(Debug, Clone, CloneRef, Default, Deref)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct Nodes {
    #[deref]
    pub all:            SharedHashMap<NodeId, Node>,
    pub selected:       SharedVec<NodeId>,
    pub grid:           Rc<RefCell<Grid>>,
    pub inputs_updated: Rc<RefCell<Vec<NodeId>>>,
}

impl Nodes {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    fn take_nodes_with_updated_inputs(&self) -> Vec<NodeId> {
        self.inputs_updated.take()
    }

    /// Update node output connections for given edge source endpoint.
    pub fn update_source_endpoint(
        &self,
        edge: EdgeId,
        old_node: Option<NodeId>,
        new_node: Option<NodeId>,
    ) {
        if old_node != new_node {
            if let Some(node_id) = old_node {
                self.with(&node_id, |node| {
                    node.out_edges.remove(&edge);
                });
            }
            if let Some(node_id) = new_node {
                self.with(&node_id, |node| {
                    node.out_edges.insert(edge);
                });
            }
        }
    }

    /// Update node input connections for given edge target endpoint.
    pub fn update_target_endpoint(
        &self,
        edge: EdgeId,
        old_node: Option<NodeId>,
        new_node: Option<NodeId>,
    ) {
        if old_node != new_node {
            let mut inputs_updated = self.inputs_updated.borrow_mut();
            if let Some(node_id) = old_node {
                self.with(&node_id, |node| {
                    node.in_edges.remove(&edge);
                    inputs_updated.push(node_id);
                });
            }
            if let Some(node_id) = new_node {
                self.with(&node_id, |node| {
                    node.in_edges.insert(edge);
                    inputs_updated.push(node_id);
                });
            }
        }
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn insert(&self, node_id: NodeId, node: Node) {
        self.all.insert(node_id, node);
        self.recompute_grid(default());
    }

    /// Calculate a Magnet Alignment grid used for nodes alignment.
    ///
    /// A grid consists of:
    ///  - Horizontal lines through each node's Y coordinate.
    ///  - Vertical lines through each node's X coordinate.
    ///
    ///  `blacklist` nodes are excluded from the calculation.
    fn recompute_grid(&self, blacklist: HashSet<NodeId>) {
        let mut sorted_xs = Vec::new();
        let mut sorted_ys = Vec::new();
        for (id, node) in &*self.raw.borrow() {
            if !blacklist.contains(id) {
                let position = node.position();
                sorted_xs.push(position.x);
                sorted_ys.push(position.y);
            }
        }
        sorted_xs.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap());
        sorted_ys.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap());
        *self.grid.borrow_mut() = Grid { sorted_xs, sorted_ys };
    }

    /// Same as [`check_grid_magnet_with_threshold`], but with default threshold.
    pub fn check_grid_magnet(&self, position: Vector2<f32>) -> Vector2<Option<f32>> {
        self.check_grid_magnet_with_threshold(position, SNAP_DISTANCE_THRESHOLD)
    }

    /// Return the nearest point in a Magnet Alignment grid. Returns `None` if the nearest point's
    /// coordinate is further than a `threshold`.
    ///
    /// See [`recompute_grid`] docs for grid description.
    pub fn check_grid_magnet_with_threshold(
        &self,
        position: Vector2<f32>,
        threshold: f32,
    ) -> Vector2<Option<f32>> {
        self.grid.borrow().close_to(position, threshold)
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn set_quick_preview(&self, quick: bool) {
        self.raw.borrow().values().for_each(|node| node.view.quick_preview_vis.emit(quick))
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn show_quick_actions(&self, quick: bool) {
        self.all
            .raw
            .borrow()
            .values()
            .for_each(|node| node.view.show_quick_action_bar_on_hover.emit(quick))
    }
}


// === Node Selection ===

impl Nodes {
    /// Mark node as selected and send FRP event to node about its selection status.
    pub fn select(&self, node_id: impl Into<NodeId>) {
        let node_id = node_id.into();
        if let Some(node) = self.get_cloned_ref(&node_id) {
            // Remove previous instances and add new selection at end of the list, indicating that
            // this node was selected last, superseding the previous selection.
            while self.selected.contains(&node_id) {
                self.selected.remove_item(&node_id)
            }
            self.selected.push(node_id);
            node.select.emit(());
        }
    }

    /// Mark node as deselected and send FRP event to node about its selection status.
    pub fn deselect(&self, node_id: impl Into<NodeId>) {
        let node_id = node_id.into();
        if let Some(node) = self.get_cloned_ref(&node_id) {
            self.selected.remove_item(&node_id);
            node.deselect.emit(());
        }
    }

    /// Mark all node as selected and send FRP events to nodes.
    pub fn select_all(&self) {
        for id in self.keys() {
            self.select(id);
        }
    }

    /// Return all nodes marked as selected.
    pub fn all_selected(&self) -> Vec<NodeId> {
        self.selected.items()
    }

    /// Return the node that was marked as selected last.
    pub fn last_selected(&self) -> Option<NodeId> {
        self.selected.last_cloned()
    }

    /// Return whether the given node is marked as selected.
    pub fn is_selected(&self, node: NodeId) -> bool {
        self.selected.contains(&node)
    }

    /// Call `deselect` for all nodes marked as selected.
    pub fn deselect_all(&self) {
        let selected = self.selected.raw.as_ref().clone();
        selected.into_inner().into_iter().for_each(|node_id| self.deselect(node_id))
    }
}



// =============
// === Edges ===
// =============

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DetachedEdge {
    /// Edge connected to a source endpoint, detached from target. This edge is not represented in
    /// the connections list.
    OnlySource { id: Option<EdgeId>, source: EdgeEndpoint },
    /// Edge connected to a target endpoint, detached from source. This edge is represented in the
    /// connections list as connected, but the view is treating it as detached.
    OnlyTarget { id: Option<EdgeId>, target: EdgeEndpoint },
}

// The Default implementation for detached edge doesn't make sense, but it is required for certain
// FRP nodes to work. It should be avoidable once FRP doesn't require the default implementation in
// all cases.
impl Default for DetachedEdge {
    fn default() -> Self {
        Self::OnlySource { id: default(), source: default() }
    }
}

impl DetachedEdge {
    fn edge_id(&self) -> Option<EdgeId> {
        match self {
            Self::OnlySource { id, .. } => *id,
            Self::OnlyTarget { id, .. } => *id,
        }
    }

    fn node_id(&self) -> NodeId {
        match *self {
            Self::OnlySource { source, .. } => source.node_id,
            Self::OnlyTarget { target, .. } => target.node_id,
        }
    }

    fn source_edge_id(&self) -> Option<EdgeId> {
        match self {
            Self::OnlySource { id, .. } => *id,
            _ => None,
        }
    }

    fn target_edge_id(&self) -> Option<EdgeId> {
        match self {
            Self::OnlyTarget { id, .. } => *id,
            _ => None,
        }
    }

    fn source_endpoint(&self) -> Option<EdgeEndpoint> {
        match *self {
            Self::OnlySource { source, .. } => Some(source),
            _ => None,
        }
    }

    fn target_endpoint(&self) -> Option<EdgeEndpoint> {
        match *self {
            Self::OnlyTarget { target, .. } => Some(target),
            _ => None,
        }
    }

    fn assign_edge_id(&mut self, edge_id: EdgeId) {
        let id = match self {
            Self::OnlySource { id, .. } => id,
            Self::OnlyTarget { id, .. } => id,
        };
        *id = Some(edge_id);
    }

    fn new_target(target: EdgeEndpoint) -> Self {
        Self::OnlyTarget { id: None, target }
    }

    fn new_source(source: EdgeEndpoint) -> Self {
        Self::OnlySource { id: None, source }
    }

    fn connect_to_target(&self, target: EdgeEndpoint) -> Option<Connection> {
        match *self {
            Self::OnlySource { source, .. } => Some(Connection { source, target }),
            _ => None,
        }
    }

    fn connect_to_source(&self, source: EdgeEndpoint) -> Option<Connection> {
        match *self {
            Self::OnlyTarget { target, .. } => Some(Connection { source, target }),
            _ => None,
        }
    }
}
#[derive(Debug, Default, Deref, DerefMut)]
struct Edges {
    #[deref]
    #[deref_mut]
    all:      HashMap<EdgeId, Edge>,
    detached: Option<EdgeId>,
}

#[derive(Debug, Clone, CloneRef, Default)]
struct Visualizations {
    /// This keeps track of the currently selected visualization. There should only ever be one
    /// visualizations selected, however due to the way that the selection is determined, it can
    /// happen that while the FRP is resolved, temporarily, we have multiple visualization in this
    /// set. This happens because the selection status is determined bottom up from each
    /// visualization and the reported via FRP to the graph editor. That means if the status
    /// we might see the new selection status for a visualization getting set before we see the
    /// previously selected visualization report its deselection. If we ever have more than one
    /// visualization in this set after the status updates have been resolved, that is a bug.
    selected: SharedHashSet<NodeId>,
}

#[derive(Debug)]
struct TouchNetwork<T: frp::Data> {
    down:     frp::Source<T>,
    up:       frp::Stream<T>,
    is_down:  frp::Stream<bool>,
    selected: frp::Stream<T>,
}

impl<T: frp::Data> TouchNetwork<T> {
    fn new(network: &frp::Network, scene: &Scene) -> Self {
        let on_scene_up = scene.on_event::<mouse::Up>();
        let on_scene_down = scene.on_event_capturing::<mouse::Down>();
        frp::extend! { network
            pos_on_down <- on_scene_down.map(|e| e.client());
            on_up_primary <- on_scene_up.filter(mouse::is_primary);
            down          <- source::<T>();
            is_down       <- bool(&on_up_primary,&down);
            was_down      <- is_down.previous();
            mouse_up      <- on_up_primary.gate(&was_down);
            should_select <- mouse_up.map2(&pos_on_down,
                |end, start| {
                    let total_drag_sq = (start - end.client()).norm_squared();
                    let move_sq = end.movement().norm_squared();
                    total_drag_sq <= move_sq * 4.0
                }
            );
            up            <- down.sample(&mouse_up);
            selected      <- up.gate(&should_select);
        }
        Self { down, up, is_down, selected }
    }
}

#[derive(Debug)]
struct TouchState {
    nodes:       TouchNetwork<NodeId>,
    background:  TouchNetwork<()>,
    input_port:  TouchNetwork<EdgeEndpoint>,
    output_port: TouchNetwork<EdgeEndpoint>,
}

impl TouchState {
    fn new(network: &frp::Network, scene: &Scene) -> Self {
        let nodes = TouchNetwork::new(network, scene);
        let background = TouchNetwork::new(network, scene);
        let input_port = TouchNetwork::new(network, scene);
        let output_port = TouchNetwork::new(network, scene);
        Self { nodes, background, input_port, output_port }
    }
}

// === Node Creation ===

/// Describes the way used to request creation of a new node.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub enum WayOfCreatingNode {
    /// "add_node" FRP event was emitted.
    AddNodeEvent,
    /// "start_node_creation" FRP event was emitted.
    StartCreationEvent,
    /// "start_node_creation_from_port" FRP event was emitted.
    StartCreationFromPortEvent { endpoint: EdgeEndpoint },
    /// add_node_button was clicked.
    ClickingButton,
    /// The edge was dropped on the stage.
    DroppingEdge { endpoint: EdgeEndpoint },
}

impl Default for WayOfCreatingNode {
    fn default() -> Self {
        Self::AddNodeEvent
    }
}

/// Context data required to create a new node.
#[derive(Debug, Clone, CloneRef)]
struct NodeCreationContext {
    pointer_style: frp::Any<cursor::Style>,
    output_press:  frp::Source<EdgeEndpoint>,
    input_press:   frp::Source<EdgeEndpoint>,
}

impl GraphEditorModel {
    fn is_node_connected_at_input(&self, node_id: NodeId, port: PortId) -> bool {
        self.with_node(node_id, |node| {
            let in_edges = node.in_edges.raw.borrow();
            in_edges.iter().any(|edge_id| {
                self.with_edge(*edge_id, |edge| edge.target().map_or(false, |t| t.port == port))
                    .unwrap_or(false)
            })
        })
        .unwrap_or(false)
    }

    /// Return a position of the node with provided id.
    pub fn get_node_position(&self, node_id: NodeId) -> Option<Vector3<f32>> {
        self.nodes.get_cloned_ref(&node_id).map(|node| node.position())
    }

    /// Synchronize view edge state with current list of existing connections and detached edge
    /// state. For additional context, see "Edge maintenance" section in
    /// [`GraphEditor::frp_init_edge_state`].
    fn maintain_edges(
        &self,
        connections: &[Connection],
        mut detached: Option<DetachedEdge>,
        pointer_frp: &EdgePointerFrp,
    ) -> (Option<DetachedEdge>, Vec<EdgeId>) {
        let mut edges = self.edges.borrow_mut();
        let mut connections_set: HashSet<_> = connections.iter().collect();

        let mut dirty_edges = Vec::new();

        let detached_id = detached.and_then(|detached| detached.edge_id());

        edges.retain(|edge_id, edge| {
            let has_connection = edge.connection.map_or(false, |c| connections_set.remove(&c));
            if has_connection {
                // Edge still is present in connections, keep it.
                true
            } else if detached_id == Some(*edge_id) {
                // Edge is detached from target, but has no connection. Update and keep it.
                edge.connection = None;
                true
            } else {
                // Otherwise, remove this edge view and its connectivity data from nodes.
                edge.set_endpoints(None, None, &self.nodes);
                false
            }
        });

        // Connections remaining in connections_set are new, create new edges for them.
        dirty_edges.extend(connections_set.into_iter().map(|connection| {
            let mut edge = self.create_edge(pointer_frp);
            edge.connection = Some(*connection);
            edge.set_endpoints(Some(connection.source), Some(connection.target), &self.nodes);
            let edge_id = edge.id();
            edges.insert(edge_id, edge);
            edge_id
        }));

        // If a previously detached edge is no longer detached, set its endpoints.
        if edges.detached != detached_id && let Some(detached) = edges.detached.take() {
            if let Some(edge) = edges.get_mut(&detached) {
                // If an edge is no longer detached and had no connection, it has already been
                // removed as part of `drain_filter` above. Therefore `edge.connection` must always
                // be `Some` at this point.
                let conn = edge.connection.expect("Updating old detached edge without connection.");
                if edge.set_endpoints(Some(conn.source), Some(conn.target), &self.nodes) {
                    dirty_edges.push(edge.id());
                }
            }
        }

        // If we have a detached edge, update or create it.
        if let Some(some_detached) = &mut detached {
            let edge = match some_detached.edge_id() {
                Some(id) => edges.get_mut(&id),
                None => {
                    // Detached edge has no view assigned yet. Create a new one.
                    let edge = self.create_edge(pointer_frp);
                    let edge_id = edge.id();
                    some_detached.assign_edge_id(edge_id);
                    edges.insert(edge_id, edge);
                    edges.get_mut(&edge_id)
                }
            };

            // Configure the source and target of the detached edge's view.
            if let Some(edge) = edge {
                let (source, target) = match *some_detached {
                    DetachedEdge::OnlySource { source, .. } => (Some(source), None),
                    DetachedEdge::OnlyTarget { target, .. } => (None, Some(target)),
                };
                if edge.set_endpoints(source, target, &self.nodes) {
                    dirty_edges.push(edge.id());
                }
                edges.detached = Some(edge.id());
            } else {
                // The current detached edge state doesn't correspond to any valid or new edge.
                // Clear it.
                edges.detached = None;
                detached = None;
            }
        }

        (detached, dirty_edges)
    }

    fn create_edge(&self, pointer: &EdgePointerFrp) -> Edge {
        let edge = Edge::new(component::Edge::new(&self.app, &self.layers));
        self.add_child(&edge);
        let edge_id = edge.id();
        let network = edge.view.network();
        frp::extend! { network
            edge.view.set_hover_disabled <+ self.frp.output.has_detached_edge;
            pointer.source_click <+ edge.view.source_click.constant(edge_id);
            pointer.target_click <+ edge.view.target_click.constant(edge_id);
        }
        edge
    }

    #[profile(Objective)]
    fn create_node(
        &self,
        ctx: &NodeCreationContext,
        way: &WayOfCreatingNode,
        mouse_position: Vector2,
    ) -> (NodeId, Option<NodeSource>, bool) {
        let position = new_node_position::new_node_position(self, way, mouse_position);
        let node = self.new_node(ctx);
        node.set_xy(position);
        let should_edit = !matches!(way, WayOfCreatingNode::AddNodeEvent);
        if should_edit {
            node.view.set_expression(node::Expression::default());
        }
        let source = self.data_source_for_new_node(way);
        (node.id(), source, should_edit)
    }

    fn data_source_for_new_node(&self, way: &WayOfCreatingNode) -> Option<NodeSource> {
        use WayOfCreatingNode::*;
        let source_node = match way {
            AddNodeEvent => None,
            StartCreationEvent | ClickingButton => self.nodes.selected.first_cloned(),
            DroppingEdge { endpoint } => Some(endpoint.node_id),
            StartCreationFromPortEvent { endpoint } => Some(endpoint.node_id),
        };
        source_node.map(|node| NodeSource { node })
    }

    #[profile(Debug)]
    fn new_node(&self, ctx: &NodeCreationContext) -> Node {
        let view = component::Node::new(&self.app, &self.layers, self.vis_registry.clone_ref());
        let node = Node::new(view);
        let node_model = node.model();
        let network = node.frp().network();
        let node_id = node.id();
        self.add_child(&node);

        let out = &self.frp.output;
        let input_frp = &node_model.input.frp;
        let output_frp = &node_model.output.frp;

        let touch = &self.touch_state;
        let NodeCreationContext { pointer_style, output_press, input_press } = ctx;

        frp::extend! { network
            let node_down = &touch.nodes.down;
            eval_ node.background_press(node_down.emit(node_id));

            out.node_hovered <+ node.output.hover.map(move |t| Switch::new(node_id,*t));

            out.node_comment_set <+ node.comment.map(move |c| (node_id,c.clone()));
            node.set_output_expression_visibility <+ out.nodes_labels_visible;

            pointer_style <+ input_frp.pointer_style;
            eval output_frp.on_port_press ((p) output_press.emit(EdgeEndpoint::new(node_id,*p)));
            eval input_frp.on_port_press ((p) input_press.emit(EdgeEndpoint::new(node_id,*p)));

            let map_hover = move |t: &Switch<PortId>| Some(EdgeEndpoint::new(node_id,t.into_on()?));
            out.hover_node_input <+ input_frp.on_port_hover.map(map_hover);
            out.hover_node_output <+ output_frp.on_port_hover.map(map_hover);

            out.node_incoming_edge_updates <+ input_frp.input_edges_need_refresh.constant(node_id);
            out.node_outgoing_edge_updates <+ input_frp.width.constant(node_id);
            out.node_widget_tree_rebuilt <+ input_frp.widget_tree_rebuilt.constant(node_id);

            let is_editing = &input_frp.editing;
            expression_change_temporary <- node.on_expression_modified.gate(is_editing);
            expression_change_permanent <- node.on_expression_modified.gate_not(is_editing);

            temporary_expression <- expression_change_temporary.map2(
                &node_model.input.set_expression,
                move |(crumbs, code), expr| expr.code_with_replaced_span(crumbs, code)
            );
            out.node_expression_set <+ temporary_expression.map(
                move |code| (node_id, code.clone())
            );

            out.node_expression_span_set <+ expression_change_permanent.map(
                move |(crumbs, code)| (node_id, crumbs.clone(), code.clone())
            );

            out.widgets_requested <+ node.requested_widgets.map(
                move |(call, target)| (node_id, *call, *target)
            );

            let node_expression_edit = node.model().input.expression_edit.clone_ref();
            out.node_expression_edited <+ node_expression_edit.map(
                move |(expr, selection)| (node_id, expr.clone_ref(), selection.clone())
            );
            out.request_import <+ node.request_import;


            // === Actions ===

            out.node_action_context_switch <+ node.view.context_switch.map(move |s| (node_id, *s));
            out.node_action_freeze <+ node.view.freeze.map(move |is_frozen| (node_id, *is_frozen));
            out.node_action_skip <+ node.view.skip.map(move |is_skipped| (node_id, *is_skipped));
            node.set_disabled <+ node.view.skip;


            // === Visualizations ===

            let visualization = node.visualization();
            visualization_shown <- visualization.visible.on_true();
            visualization_hidden <- visualization.visible.on_false();

            let vis_is_selected = node_model.visualization.frp.is_selected.clone_ref();

            selected    <- vis_is_selected.on_true();
            deselected  <- vis_is_selected.on_false();
            out.on_visualization_select <+ selected.constant(Switch::On(node_id));
            out.on_visualization_select <+ deselected.constant(Switch::Off(node_id));

            preprocessor_changed <-
                node_model.visualization.frp.preprocessor.map(move |preprocessor| {
                    (node_id,preprocessor.clone())
                });
            out.visualization_preprocessor_changed <+ preprocessor_changed;


            metadata <- any(...);
            metadata <+ node_model.visualization.frp.preprocessor.map(visualization::Metadata::new);

            // Ensure the graph editor knows about internal changes to the visualization. If the
            // visualization changes that should indicate that the old one has been disabled and a
            // new one has been enabled.
            // TODO: Create a better API for updating the controller about visualization changes
            // (see #896)
            out.visualization_hidden <+ visualization_hidden.constant(node_id);
            out.visualization_shown  <+
                visualization_shown.map2(&metadata,move |_,metadata| (node_id,metadata.clone()));

            init <- source::<()>();
            enabled_visualization_path <- init.all_with3(
                &visualization.visible, &visualization.visualization_path,
                move |_init, is_enabled, path| (node_id, is_enabled.and_option(path.clone()))
            );
            out.enabled_visualization_path <+ enabled_visualization_path.on_change();


            // === Read-only mode ===

            node.set_read_only <+ self.frp.input.set_read_only;


            // === Execution Environment ===

            node.set_execution_environment <+ self.frp.output.execution_environment;
        }

        let initial_metadata = visualization::Metadata {
            preprocessor: node_model.visualization.frp.preprocessor.value(),
        };
        metadata.emit(initial_metadata);
        init.emit(());

        self.nodes.insert(node_id, node.clone_ref());
        node
    }
}



// ========================
// === GraphEditorModel ===
// ========================

#[derive(Debug, display::Object)]
#[allow(missing_docs)] // FIXME[everyone] Public-facing API should be documented.
pub struct GraphEditorModel {
    pub display_object:   display::object::Instance,
    // Required for dynamically creating nodes and edges.
    pub app:              Application,
    layers:               GraphLayers,
    pub nodes:            Nodes,
    edges:                RefCell<Edges>,
    pub vis_registry:     visualization::Registry,
    pub drop_manager:     ensogl_drop_manager::Manager,
    pub navigator:        Navigator,
    pub add_node_button:  Rc<component::add_node_button::AddNodeButton>,
    tooltip:              Tooltip,
    touch_state:          TouchState,
    visualizations:       Visualizations,
    frp:                  api::Private,
    frp_public:           api::Public,
    styles_frp:           StyleWatchFrp,
    selection_controller: selection::Controller,
}


// === Public ===

impl GraphEditorModel {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn new(app: &Application, frp: &Frp) -> Self {
        let network = frp.network();
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new_named("GraphEditor");
        let nodes = Nodes::new();
        let edges = RefCell::new(Edges::default());
        let vis_registry = visualization::Registry::with_default_visualizations();
        let visualizations = default();
        let touch_state = TouchState::new(network, scene);
        let app = app.clone_ref();
        let navigator = Navigator::new(scene, &scene.camera());
        let tooltip = Tooltip::new(&app);
        let add_node_button = Rc::new(component::add_node_button::AddNodeButton::new(&app));
        let drop_manager =
            ensogl_drop_manager::Manager::new(&scene.dom.root.clone_ref().into(), scene);
        let styles_frp = StyleWatchFrp::new(&scene.style_sheet);
        let selection_controller = selection::Controller::new(
            frp,
            &app.cursor,
            &scene.mouse.frp_deprecated,
            &touch_state,
            &nodes,
        );

        let layers = GraphLayers::new(&scene.layers);

        Self {
            display_object,
            app,
            layers,
            nodes,
            edges,
            vis_registry,
            drop_manager,
            tooltip,
            touch_state,
            visualizations,
            navigator,
            add_node_button,
            frp: frp.private.clone_ref(),
            frp_public: frp.public.clone_ref(),
            styles_frp,
            selection_controller,
        }
        .init()
    }

    fn init(self) -> Self {
        self.scene().add_child(&self.tooltip);
        self.add_child(&*self.add_node_button);
        self
    }

    fn scene(&self) -> &Scene {
        &self.app.display.default_scene
    }
}


// === Add node ===
impl GraphEditorModel {
    /// Create a new node and return a unique identifier.
    pub fn add_node(&self) -> NodeId {
        self.frp_public.input.add_node.emit(());
        let (node_id, _, _) = self.frp_public.output.node_added.value();
        node_id
    }

    /// Create a new node and place it at a free place below `above` node.
    pub fn add_node_below(&self, above: NodeId) -> NodeId {
        let pos = new_node_position::under(self, above);
        self.add_node_at(pos)
    }

    /// Create a new node and place it at `pos`.
    pub fn add_node_at(&self, pos: Vector2) -> NodeId {
        let node_id = self.add_node();
        self.frp_public.input.set_node_position.emit((node_id, pos));
        node_id
    }
}


// === Remove ===

impl GraphEditorModel {
    fn enable_visualization(&self, node_id: impl Into<NodeId>) {
        let node_id = node_id.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            node.enable_visualization();
        }
    }

    fn disable_visualization(&self, node_id: impl Into<NodeId>) {
        let node_id = node_id.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            node.disable_visualization();
        }
    }

    fn enable_visualization_fullscreen(&self, node_id: impl Into<NodeId>) -> bool {
        let node_id = node_id.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            node.frp().enable_fullscreen_visualization();
            node.visualization().fullscreen.value()
        } else {
            false
        }
    }

    fn disable_visualization_fullscreen(&self, node_id: impl Into<NodeId>) {
        let node_id = node_id.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            let new_state = visualization::ViewState::Enabled { has_error: false };
            node.model().visualization.frp.set_view_state(new_state);
        }
    }

    fn show_node_editing_preview(&self, node_id: impl Into<NodeId>) {
        let node_id = node_id.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            node.show_preview();
        }
    }

    /// Get the visualization on the node, if it is enabled.
    pub fn enabled_visualization(
        &self,
        node_id: impl Into<NodeId>,
    ) -> Option<visualization::Metadata> {
        let node = self.nodes.get_cloned_ref(&node_id.into())?;
        let frp = &node.model().visualization.frp;
        frp.visible.value().then(|| visualization::Metadata::new(&frp.preprocessor.value()))
    }

    /// Remove node and all edges connected to it.
    #[profile(Debug)]
    fn remove_node(&self, node_id: NodeId) {
        self.nodes.remove(&node_id);
        self.nodes.selected.remove_item(&node_id);
        self.frp.output.on_visualization_select.emit(Switch::Off(node_id));
    }

    fn node_in_edges(&self, node_id: impl Into<NodeId>) -> Vec<EdgeId> {
        self.with_node(node_id.into(), |node| node.in_edges()).unwrap_or_default()
    }

    fn node_out_edges(&self, node_id: impl Into<NodeId>) -> Vec<EdgeId> {
        self.with_node(node_id.into(), |node| node.out_edges()).unwrap_or_default()
    }

    fn node_in_and_out_edges(&self, node_id: impl Into<NodeId>) -> Vec<EdgeId> {
        self.with_node(node_id.into(), |node| node.all_edges()).unwrap_or_default()
    }

    #[profile(Detail)]
    fn set_node_expression(&self, node_id: impl Into<NodeId>, expr: impl Into<node::Expression>) {
        let node_id = node_id.into();
        let expr = expr.into();
        if let Some(node) = self.nodes.get_cloned_ref(&node_id) {
            node.set_expression.emit(expr);
        }
    }

    fn edit_node_expression(
        &self,
        node_id: NodeId,
        range: &text::Range<text::Byte>,
        inserted_str: &ImString,
    ) {
        self.with_node(node_id, |node| node.edit_expression(*range, inserted_str.clone()));
    }

    fn set_node_skip(&self, node_id: NodeId, skip: bool) {
        self.with_node(node_id, |node| node.set_skip_macro(skip));
    }

    fn set_node_freeze(&self, node_id: NodeId, freeze: bool) {
        self.with_node(node_id, |node| node.set_freeze_macro(freeze));
    }

    fn set_node_context_switch(&self, node_id: NodeId, context_switch: &Option<bool>) {
        self.with_node(node_id, |node| node.set_context_switch(*context_switch));
    }

    fn set_node_comment(&self, node_id: NodeId, comment: &ImString) {
        self.with_node(node_id, |node| node.set_comment(comment.clone()));
    }
}


// === Connect ===

impl GraphEditorModel {
    fn set_edge_freeze(&self, edge_id: EdgeId, is_frozen: bool) {
        self.with_edge(edge_id, |edge| edge.view.set_disabled.emit(is_frozen));
    }
}


// === Position ===

impl GraphEditorModel {
    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    #[profile(Debug)]
    pub fn set_node_position(&self, node_id: NodeId, position: Vector2) {
        self.with_node(node_id, |node| node.set_xy((position.x, position.y)));
        self.refresh_edge_positions(self.node_in_and_out_edges(node_id));
    }

    #[profile(Debug)]
    fn set_node_expression_usage_type(
        &self,
        node_id: NodeId,
        ast_id: ast::Id,
        maybe_type: Option<Type>,
    ) {
        self.with_node(node_id, |node| {
            let node_model = node.view.model();
            if node_model.output.whole_expr_id().contains(&ast_id) {
                let enso_type = maybe_type.as_ref().map(|tp| enso::Type::new(&tp.0));
                node_model.visualization.frp.set_vis_input_type(enso_type);
            }
            node.view.set_expression_usage_type.emit((ast_id, maybe_type));
        });
    }

    fn update_node_connections(&self, node_id: NodeId) {
        self.with_node(node_id, |node| {
            let entries = node.in_edges().into_iter().filter_map(|edge_id| {
                self.with_edge(edge_id, |edge| Some((edge.target?.port, edge.color)))?
            });
            node.view.set_connections.emit(entries.collect::<HashMap<_, _>>());
        });
    }

    fn update_node_widgets(&self, node_id: NodeId, updates: &CallWidgetsConfig) {
        self.try_with_node(node_id, |node| node.view.update_widgets.emit(updates.clone()));
    }

    fn disable_grid_snapping_for(&self, node_ids: &[NodeId]) {
        self.nodes.recompute_grid(node_ids.iter().cloned().collect());
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn node_position(&self, node_id: NodeId) -> Vector2<f32> {
        self.with_node(node_id, |node| node.position().xy()).unwrap_or_default()
    }

    /// Return the bounding box of the node identified by `node_id`, or a default bounding box if
    /// the node was not found.
    pub fn node_bounding_box(&self, node_id: NodeId) -> selection::BoundingBox {
        self.with_node(node_id, |node| node.bounding_box.value()).unwrap_or_default()
    }

    #[allow(missing_docs)] // FIXME[everyone] All pub functions should have docs.
    pub fn node_pos_mod(&self, node_id: NodeId, pos_diff: Vector2) -> (NodeId, Vector2) {
        let new_position =
            self.with_node(node_id, |n| n.position().xy() + pos_diff).unwrap_or_default();
        (node_id, new_position)
    }

    /// Recalculate colors for edges in specified list. Returns a set of edges that have changed
    /// their color.
    pub fn refresh_edge_colors(&self, edge_ids: impl IntoIterator<Item = EdgeId>) -> Vec<EdgeId> {
        let mut edges = self.edges.borrow_mut();
        edge_ids
            .into_iter()
            .filter(|edge_id| {
                let Some(edge) = edges.get_mut(edge_id) else { return false };
                let source_color = || match edge.target {
                    Some(target) => self.node_color(target.node_id),
                    None => self.hovered_input_color(),
                };
                let target_color = || match edge.source {
                    Some(source) => self.node_color(source.node_id),
                    None => self.hovered_output_color(),
                };
                let edge_color = source_color().or_else(target_color);
                let color = edge_color.unwrap_or_else(|| self.edge_fallback_color());
                edge.set_color(color)
            })
            .collect()
    }

    /// Refresh the source and target position of the edges identified by `edge_ids`.
    pub fn refresh_edge_positions(&self, edge_ids: impl IntoIterator<Item = EdgeId>) {
        let edges = self.edges.borrow();
        for edge_id in edge_ids.into_iter() {
            let Some(edge) = edges.get(&edge_id) else { continue };

            if let Some(edge_source) = edge.source() {
                self.with_node(edge_source.node_id, |node| {
                    let node_width = node.model().width();
                    let node_height = node.model().height();
                    let new_position = node.position().xy() + Vector2::new(node_width / 2.0, 0.0);
                    let prev_position = edge.position().xy();

                    if prev_position != new_position {
                        edge.set_xy(new_position);
                    }
                    edge.view.source_size.emit(Vector2(node_width, node_height));
                });
            }
            if let Some(edge_target) = edge.target() {
                self.with_node(edge_target.node_id, |node| {
                    let input = &node.model().input;
                    let port_offset = input.port_offset(edge_target.port);
                    let port_size = input.port_size(edge_target.port);
                    let node_position = node.position().xy();
                    edge.view.target_position.emit(node_position + port_offset);
                    edge.view.target_size.emit(port_size);
                });
            }
        }
    }

    /// Refresh the positions of all outgoing edges connected to the given node.
    pub fn refresh_outgoing_edge_positions(&self, node_ids: &[NodeId]) {
        for node_id in node_ids {
            self.refresh_edge_positions(self.node_out_edges(node_id));
        }
    }

    /// Refresh the positions of all incoming edges connected to the given node. This is useful when
    /// we know that the node ports has been updated, but we don't track which exact edges are
    /// affected.
    pub fn refresh_incoming_edge_positions(&self, node_ids: &[NodeId]) {
        for node_id in node_ids {
            self.refresh_edge_positions(self.node_in_edges(node_id));
        }
    }

    /// Perform an operation on a node with given ID, if it exists.
    pub fn try_with_node<T>(&self, id: NodeId, f: impl FnOnce(&Node) -> T) -> Option<T> {
        self.nodes.with(&id, f)
    }

    /// Perform an operation on a node with given ID. Reports an error if the node does not exist.
    pub fn with_node<T>(&self, id: NodeId, f: impl FnOnce(&Node) -> T) -> Option<T> {
        self.try_with_node(id, f).map_none(|| warn!("Trying to access nonexistent node '{id}'"))
    }

    fn with_edge<T>(&self, id: EdgeId, f: impl FnOnce(&Edge) -> T) -> Option<T> {
        let edges = self.edges.borrow();
        let edge = edges.get(&id).map_none(|| warn!("Trying to access nonexistent edge '{id}'"))?;
        Some(f(edge))
    }

    fn edge_connection(&self, id: EdgeId) -> Option<Connection> {
        self.with_edge(id, |edge| edge.connection).flatten()
    }

    fn edge_target(&self, id: EdgeId) -> Option<EdgeEndpoint> {
        self.with_edge(id, |edge| edge.target).flatten()
    }

    fn node_color(&self, id: NodeId) -> Option<color::Lcha> {
        self.with_node(id, |node| node.port_color.value())
    }

    fn hovered_input_color(&self) -> Option<color::Lcha> {
        let hover_target = self.frp_public.output.hover_node_input.value();
        hover_target.and_then(|tgt| self.node_color(tgt.node_id))
    }

    fn hovered_output_color(&self) -> Option<color::Lcha> {
        let hover_target = self.frp_public.output.hover_node_output.value();
        hover_target.and_then(|tgt| self.node_color(tgt.node_id))
    }

    /// Retrieve the color of the edge. Does not recomputes it, but returns the cached value.
    fn edge_color(&self, edge_id: EdgeId) -> color::Lcha {
        self.with_edge(edge_id, |edge| edge.color).unwrap_or_else(|| self.edge_fallback_color())
    }

    fn edge_fallback_color(&self) -> color::Lcha {
        self.styles_frp.get_color(theme::code::types::any::selection).value().into()
    }

    /// Pan the camera to fully fit the `target_bbox` (expressed in scene coordinates) into a
    /// rectangular viewport between `screen_min_xy` and `screen_max_xy` (in screen coordinates).
    /// If `target_bbox` does not fully fit in the viewport, prefer showing the top-left corner of
    /// `target_bbox` than the opposite one.
    fn pan_camera(
        &self,
        target_bbox: selection::BoundingBox,
        screen_min_xy: Vector2,
        screen_max_xy: Vector2,
    ) {
        use ensogl::display::navigation::navigator::PanEvent;
        let scene = &self.scene();
        let screen_to_scene_xy = |pos: Vector2| {
            let vec3 = Vector3(pos.x, pos.y, 0.0);
            scene.screen_to_scene_coordinates(vec3).xy()
        };
        let scene_min_xy = screen_to_scene_xy(screen_min_xy);
        let scene_max_xy = screen_to_scene_xy(screen_max_xy);
        let viewport = selection::BoundingBox::from_corners(scene_min_xy, scene_max_xy);
        let pan_left = some_if_negative(target_bbox.left() - viewport.left());
        let pan_right = some_if_positive(target_bbox.right() - viewport.right());
        let pan_up = some_if_positive(target_bbox.top() - viewport.top());
        let pan_down = some_if_negative(target_bbox.bottom() - viewport.bottom());
        let pan_x = pan_left.or(pan_right).unwrap_or_default();
        let pan_y = pan_up.or(pan_down).unwrap_or_default();
        let pan_xy = Vector2(pan_x, pan_y);
        self.navigator.emit_pan_event(PanEvent::new(-pan_xy * scene.camera().zoom()));
    }

    fn pan_camera_to_node(&self, node_id: NodeId) {
        use theme::graph_editor::screen_margin_when_panning_camera_to_node as pan_margin;
        let Some(node_bbox) = self.with_node(node_id, |n| n.bounding_box.value()) else { return };
        let camera = &self.scene().camera();
        let screen_size_halved = Vector2::from(camera.screen()) / 2.0;
        let styles = &self.styles_frp;
        let top_margin = styles.get_number(pan_margin::top).value();
        let bottom_margin = styles.get_number(pan_margin::bottom).value();
        let left_margin = styles.get_number(pan_margin::left).value();
        let right_margin = styles.get_number(pan_margin::right).value();
        let viewport_max_y = screen_size_halved.y - top_margin;
        let viewport_min_y = -screen_size_halved.y + bottom_margin;
        let viewport_min_x = -screen_size_halved.x + left_margin;
        let viewport_max_x = screen_size_halved.x - right_margin;
        let viewport_min_xy = Vector2(viewport_min_x, viewport_min_y);
        let viewport_max_xy = Vector2(viewport_max_x, viewport_max_y);
        self.pan_camera(node_bbox, viewport_min_xy, viewport_max_xy)
    }
}


// === Utilities ===

fn some_if_positive(x: f32) -> Option<f32> {
    (x > 0.0).as_some(x)
}

fn some_if_negative(x: f32) -> Option<f32> {
    (x < 0.0).as_some(x)
}



// ============================
// === FRP internal signals ===
// ============================

/// Set of internal FRP signals initialized in [`GraphEditor::frp_init_node_expression`].
struct NodeExpressionFrp {
    node_with_new_expression_type: frp::Stream<NodeId>,
}

/// Set of internal FRP signals initialized in [`GraphEditor::frp_init_background_interaction`].
struct BgInteractionFrp {
    /// A pointer was released over a background. This signal is emitted even if the pointer was
    /// not originally pressed over a background.
    pointer_up:                    frp::Stream,
    clicked_with_detached_edge:    frp::Stream,
    clicked_without_detached_edge: frp::Stream,
}

/// Set of internal FRP signals initialized in [`GraphEditor::frp_init_edge_pointer`].
#[derive(Debug, Clone, CloneRef)]
struct EdgePointerFrp {
    source_click: frp::Any<EdgeId>,
    target_click: frp::Any<EdgeId>,
}

/// Set of internal FRP signals initialized in [`GraphEditor::frp_init_edge_state`].
struct EdgeStateFrp {
    /// The edge currently being dragged. May or may not represent an existing connection.
    detached_edge:             frp::Stream<Option<DetachedEdge>>,
    /// Set of edges that were modified during maintenance process.
    /// See "Edge maintenance" section in [`GraphEditor::frp_init_edge_state`].
    maintained_edges_dirty:    frp::Stream<Vec<EdgeId>>,
    /// Set given edge as detached. If any other edge is already detached, it will be cleared
    /// first. When detached edge is set to invalid edge ID, the state will be cleared during
    /// maintain.
    set_detached_edge:         frp::Any<DetachedEdge>,
    /// Clear detached edge state. Does NOT remove the connection. If currently detached edge is
    /// still representing the connection, its view will be reconfigured to match that connection.
    /// In order to remove the connection, use `break_detached_connection` signal first. If there
    /// is no detached edge currently set, this signal is ignored.
    clear_detached_edge:       frp::Any_,
    /// Break the connection represented by the currently detached edge. Does NOT clear the
    /// detached edge state, nor does it remove the detached edge from view. In order to remove
    /// detached edge view, use `clear_detached_edge` signal. If there is no detached edge
    /// currently set, this signal is ignored.
    break_detached_connection: frp::Any_,
}

struct EdgeColorFrp {
    edges_with_updated_color: frp::Stream<Vec<EdgeId>>,
}

// Set of internal FRP signals initialized in [`GraphEditor::frp_init_edge_interaction`].
struct EdgeInteractionFrp {
    create_node_from_edge: frp::Stream<EdgeEndpoint>,
}

// ===================
// === GraphEditor ===
// ===================

/// The graph editor is the main view of the application once project has been opened. It displays
/// the graph and allows the user to interact with it. It manages the views of all nodes and edges
/// on the scene.
#[allow(missing_docs)]
#[derive(Debug, Clone, CloneRef, Deref, display::Object)]
pub struct GraphEditor {
    #[display_object]
    pub model: Rc<GraphEditorModel>,
    #[deref]
    pub frp:   Frp,
}

impl GraphEditor {
    fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Rc::new(GraphEditorModel::new(app, &frp));
        GraphEditor { model, frp }.init()
    }

    /// Initialize graph editor FRP network.
    fn init(self) -> Self {
        let node_expr = self.frp_init_node_expression();
        let bg_interaction = self.frp_init_background_interaction();
        let edge_pointer = self.frp_init_edge_pointer();
        let edge_state = self.frp_init_edge_state(edge_pointer.clone_ref());
        let edge_color = self.frp_init_edge_colors(&node_expr, &edge_state);
        self.frp_init_edge_positions(&edge_state);
        self.frp_init_node_connections(&edge_state, &edge_color);
        let create_node_from_edge =
            self.frp_init_edge_interaction(&edge_state, &edge_pointer, &bg_interaction);

        init_remaining_graph_editor_frp(
            &self,
            &edge_state,
            &bg_interaction,
            &create_node_from_edge,
            &edge_color,
        );
        self
    }

    fn frp_init_node_expression(&self) -> NodeExpressionFrp {
        let network = self.frp.network();
        let model = &self.model;
        let input = &self.frp.input;

        frp::extend! { network
            node_with_new_expression_type <- input.set_expression_usage_type
            .map(f!([model]((node_id,ast_id,maybe_type)) {
                model.set_node_expression_usage_type(*node_id,*ast_id,maybe_type.clone());
                *node_id
            })).batch_unique().iter();
            eval input.update_node_widgets(((id, widgets)) model.update_node_widgets(*id, widgets));
            eval input.set_node_expression(((id, expr)) model.set_node_expression(id, expr));
            eval input.edit_node_expression(
                ((id, range, ins)) model.edit_node_expression(*id, range, ins)
            );
        }
        NodeExpressionFrp { node_with_new_expression_type }
    }

    fn frp_init_node_connections(&self, edge_state: &EdgeStateFrp, edge_color: &EdgeColorFrp) {
        let network = self.frp.network();
        let model = &self.model;

        frp::extend! { network
            node_with_updated_inputs <- any(...);
            node_with_updated_inputs <+ edge_state.maintained_edges_dirty
                .map(f!((_) model.nodes.take_nodes_with_updated_inputs())).iter();
            node_with_updated_inputs <+ edge_color.edges_with_updated_color.iter()
                .filter_map(f!((id) model.edge_target(*id).map(|t| t.node_id)));
            nodes_to_update_connections <- node_with_updated_inputs.batch_unique();
            eval nodes_to_update_connections(
                [model] (node_ids) node_ids.iter().for_each(|id| model.update_node_connections(*id))
            );
        }
    }

    fn frp_init_background_interaction(&self) -> BgInteractionFrp {
        use display::scene::PointerTargetId;

        let network = self.frp.network();
        let out = &self.frp.private.output;
        let model = &self.model;
        let scene = model.scene();
        let touch = &model.touch_state;
        let mouse_target = &scene.mouse.target;
        let mouse = &scene.mouse.frp_deprecated;

        let bg_down = &touch.background.down;
        let bg_click = &touch.background.selected;

        frp::extend! { network
            up_target <- mouse.up_primary.map(f_!(mouse_target.get()));
            pointer_up <- up_target.filter(|t| t == &PointerTargetId::Background).constant(());
            eval_ scene.mouse.background.mouse_down_primary(bg_down.emit(()));

            // Remove focus from any element when background is clicked.
            eval_ touch.background.down(model.display_object.blur_tree());

            was_edge_detached_on_bg_click  <- out.has_detached_edge.sample(bg_click);
            clicked_with_detached_edge <- was_edge_detached_on_bg_click.on_true();
            clicked_without_detached_edge <- was_edge_detached_on_bg_click.on_false();
        }
        BgInteractionFrp { pointer_up, clicked_with_detached_edge, clicked_without_detached_edge }
    }

    fn frp_init_edge_pointer(&self) -> EdgePointerFrp {
        let network = self.frp.network();
        frp::extend! { network
            source_click <- any(...);
            target_click <- any(...);
        }
        EdgePointerFrp { source_click, target_click }
    }

    // Initialize the maintenance of edge state. Updates the edge views according to the set of
    // active connections and current detached edge state.
    fn frp_init_edge_state(&self, pointer: EdgePointerFrp) -> EdgeStateFrp {
        let network = self.frp.network();
        let input = &self.frp.private.input;
        let out = &self.frp.private.output;
        let model = &self.model;

        frp::extend! { network
            // Attempt to set detached edge state. See [`EdgeStateFrp.set_detached_edge`].
            set_detached_edge <- any_mut::<DetachedEdge>();

            // Break the connection of currently detached edge.
            // See [`EdgeStateFrp.break_detached_connection`].
            break_detached_connection <- any_(...);
            break_detached_connection <+ input.drop_dragged_edge;

            // Clear detached edge state. See [`EdgeStateFrp.clear_detached_edge`].
            clear_detached_edge <- any_(...);
            clear_detached_edge <+ input.drop_dragged_edge;
            clear_detached_edge <+ input.set_read_only.on_true();
            clear_detached_edge <+ out.connection_made;
            clear_detached_edge <+ out.node_added.debounce();
            clear_detached_edge <+ out.node_editing;
            clear_detached_edge <+ out.node_entered;
            clear_detached_edge <+ out.node_exited;

            // === Edge maintenance ===
            // Edge maintenance is a process of updating the view of all edges. It is triggered by
            // any connection change received through `set_connections` signal, or by any change to
            // the detached edge state. When triggered, it will update the view of all edges to
            // match that desired state. When the set detached edge is determined to be no longer
            // valid, it will be automatically cleared.

            // The state of detached edge that should be used during next maintain. Note that this
            // state forms a feedback loop, as it is influences by `maintain_result`. That way, the
            // detached edge state can be influenced by the maintain process itself.
            detached_edge_cell <- any(...);

            new_detached_edge <- any(...);
            new_detached_edge <+ set_detached_edge.some();
            new_detached_edge <+ clear_detached_edge.constant(None);
            detached_edge_changed <- new_detached_edge.map2(&detached_edge_cell, |a, b| a != b);
            detached_edge_cell <+ new_detached_edge;


            // The trigger signal when to perform the maintain. This separation of data and timing
            // is necessary, because the maintain process can effectively influence its own input
            // state (see `detached_edge_cell` above). By separating the timing, we can avoid
            // unnecessary looping.
            do_maintain_edges <- any_(...);
            do_maintain_edges <+ input.set_connections;
            do_maintain_edges <+ new_detached_edge.gate(&detached_edge_changed);
            maintain_result <- do_maintain_edges.map3(
                &input.set_connections, &detached_edge_cell,
                f!((_, conn, detached) model.maintain_edges(conn, *detached, &pointer))
            );
            detached_edge <- maintain_result._0();
            maintained_edges_dirty <- maintain_result._1();
            // Complete detached edge update feedback cycle - make sure the detached edge set during
            // maintain will also be used during maintain.
            detached_edge_cell <+ detached_edge;
            out.has_detached_edge <+ detached_edge.is_some().on_change();

            detached_to_break <- detached_edge.sample(&break_detached_connection);
            out.connection_broken <+ detached_to_break
                .filter_map(f!((detached) model.edge_connection(detached.as_ref()?.edge_id()?)));
        }

        EdgeStateFrp {
            detached_edge,
            maintained_edges_dirty,
            set_detached_edge,
            clear_detached_edge,
            break_detached_connection,
        }
    }

    /// Initialize drag and drop interactions for edges.
    fn frp_init_edge_interaction(
        &self,
        state: &EdgeStateFrp,
        pointer: &EdgePointerFrp,
        bg: &BgInteractionFrp,
    ) -> EdgeInteractionFrp {
        self.frp_init_edge_click(state, pointer);
        self.frp_init_edge_creation(state);
        self.frp_init_detached_edge_position(state);
        let create_node_from_edge = self.frp_init_edge_bg_drop(state, bg);
        EdgeInteractionFrp { create_node_from_edge }
    }

    fn frp_init_edge_click(&self, state: &EdgeStateFrp, pointer: &EdgePointerFrp) {
        let network = self.frp.network();
        let input = &self.frp.private.input;
        let out = &self.frp.private.output;
        let model = &self.model;

        frp::extend! { network
            cannot_interact <- input.set_read_only || out.has_detached_edge;
            source_click <- pointer.source_click.gate_not(&cannot_interact);
            target_click <- pointer.target_click.gate_not(&cannot_interact);
            detach_source <- source_click.filter_map(
                f!((id) model.with_edge(*id, Edge::as_detached_at_source).flatten())
            );
            detach_target <- target_click.filter_map(
                f!((id) model.with_edge(*id, Edge::as_detached_at_target).flatten())
            );
            state.set_detached_edge <+ detach_source;
            state.set_detached_edge <+ detach_target;
            // Only break the dragged connection when it is no longer connected to a target.
            // Otherwise keep the connection still active, so that the edge still has a valid target
            // AST to connect to.
            state.break_detached_connection <+ detach_source;
        }
    }

    fn frp_init_edge_creation(&self, state: &EdgeStateFrp) {
        let network = self.frp.network();
        let input = &self.frp.private.input;
        let out = &self.frp.private.output;
        let model = &self.model;
        let touch = &model.touch_state;
        let scene = model.scene();
        let mouse = &scene.mouse.frp_deprecated;

        frp::extend! { network
            init <- source_();

            // Before performing edge attach logic, check if creating new edge on click makes sense.
            cannot_interact <- input.set_read_only || out.has_detached_edge;
            cannot_interact <- all(cannot_interact, init)._0();
            can_create_new_edge <- cannot_interact.not();
            can_create_on_output <- can_create_new_edge.sample(&touch.output_port.down);
            can_create_on_input <- can_create_new_edge.sample(&touch.input_port.down);

            // Attach detached edge to node port when clicking or releasing on node port.
            port_output_mouse_up <- out.hover_node_output.sample(&mouse.up_primary).unwrap();
            port_input_mouse_up <- out.hover_node_input.sample(&mouse.up_primary).unwrap();
            attach_edge_output <- any(port_output_mouse_up, touch.output_port.selected);
            attach_edge_input <- any(port_input_mouse_up, touch.input_port.selected);
            out.connection_made <+ attach_edge_output.map2(&state.detached_edge,
                |source, detached| detached.as_ref()?.connect_to_source(*source)
            ).unwrap();
            out.connection_made <+ attach_edge_input.map2(&state.detached_edge,
                |target, detached| detached.as_ref()?.connect_to_target(*target)
            ).unwrap();

            // Create new detached edge when clicking on node port.
            state.set_detached_edge <+ touch.output_port.down.gate(&can_create_on_output)
                .map(|source| DetachedEdge::new_source(*source));
            state.set_detached_edge <+ touch.input_port.down.gate(&can_create_on_input)
                .filter(f!([model] (t) !model.is_node_connected_at_input(t.node_id,t.port)))
                .map(|target| DetachedEdge::new_target(*target));
        }
        init.emit(());
    }

    fn frp_init_edge_colors(
        &self,
        node_expr: &NodeExpressionFrp,
        edge_state: &EdgeStateFrp,
    ) -> EdgeColorFrp {
        let network = self.frp.network();
        let model = &self.model;
        let out = &self.frp.private.output;

        frp::extend! { network
            // === Update edge colors ===
            edge_to_refresh_color <- any(...);
            edge_to_refresh_color <+ node_expr.node_with_new_expression_type.map(
                f!((id) model.node_in_and_out_edges(*id))
            ).iter();
            edge_to_refresh_color <+ edge_state.maintained_edges_dirty.iter();
            port_hovered <- any_(out.hover_node_output, out.hover_node_input);
            edge_to_refresh_color <+ edge_state.detached_edge.sample(&port_hovered)
                .filter_map(|&d| d?.edge_id());
            edges_to_refresh_color_batch <- edge_to_refresh_color.batch_unique();
            edges_with_updated_color <- edges_to_refresh_color_batch.map(
                f!((edge_ids) model.refresh_edge_colors(edge_ids.iter().copied()))
            );
        }

        EdgeColorFrp { edges_with_updated_color }
    }

    fn frp_init_edge_positions(&self, state: &EdgeStateFrp) {
        let network = self.frp.network();
        let out = &self.frp.private.output;
        let model = &self.model;

        frp::extend! { network
            incoming_batch <- out.node_incoming_edge_updates.batch();
            outgoing_batch <- out.node_outgoing_edge_updates.batch();
            eval incoming_batch((n) model.refresh_incoming_edge_positions(n));
            eval outgoing_batch((n) model.refresh_outgoing_edge_positions(n));
            eval state.maintained_edges_dirty((e) model.refresh_edge_positions(e.iter().copied()));
        }
    }


    fn frp_init_detached_edge_position(&self, state: &EdgeStateFrp) {
        let network = self.frp.network();
        let model = &self.model;
        let cursor = &model.app.cursor.frp;
        let out = &self.frp.private.output;

        frp::extend! { network
            detached_source_edge <- state.detached_edge.and_then(|d| d.source_edge_id());
            detached_target_edge <- state.detached_edge.and_then(|d| d.target_edge_id());
            detached_source_endpoint <- state.detached_edge.and_then(|d| d.source_endpoint());
            detached_target_endpoint <- state.detached_edge.and_then(|d| d.target_endpoint());
            detached_target_node <- detached_target_endpoint.map_some(|e| e.node_id);
            detached_node <- state.detached_edge.map_some(|e| e.node_id());

            cursor_pos_on_update <- cursor.scene_position.sample(&state.detached_edge);
            refresh_cursor_pos <- any(cursor_pos_on_update, cursor.scene_position);
            refresh_cursor_data <- all(refresh_cursor_pos, cursor.box_size);

            // Only allow hovering output ports of different nodes than the current target node.
            is_hovering_valid_output <- out.hover_node_output.all_with(&detached_target_node,
                |&hover, &target| hover.zip(target).map_or(false, |(h, id)| h.node_id != id)
            );
            refresh_source <- refresh_cursor_pos.gate_not(&is_hovering_valid_output);
            snap_source_to_node <- out.hover_node_output.unwrap().gate(&is_hovering_valid_output);


            _eval <- refresh_cursor_data.map2(&detached_source_edge,
                f!(((position, cursor_size), &edge_id) model.with_edge(edge_id?, |edge| {
                    let top_of_cursor = Vector2(0.0, cursor_size.y() / 2.0 - CURSOR_EDGE_OVERLAP);
                    edge.view.target_position.emit(position.xy() + top_of_cursor);
                }))
            );
            _eval <- refresh_source.map2(&detached_target_edge,
                f!((position, &edge_id) model.with_edge(edge_id?, |edge| {
                    edge.set_xy(position.xy());
                    edge.view.source_size.emit(Vector2(0.0, 0.0));
                }))
            );
            _eval <- snap_source_to_node.map2(&detached_target_edge,
                f!([model](target, &edge_id) {
                    let (node_size, node_pos) = model.with_node(target.node_id, |node| {
                        let node_width  = node.view.model().width();
                        let node_height = node.view.model().height();
                        let node_pos    = node.position();
                        (Vector2(node_width, node_height), node_pos)
                    })?;
                    model.with_edge(edge_id?, |edge| {
                        edge.set_xy(Vector2(node_pos.x + node_size.x/2.0, node_pos.y));
                        edge.view.source_size.emit(node_size);
                    })
                })
            );

            // Whenever the detached edge is connected or hovered over a source, update the source's
            // node state, so it can be moved to a correct layer.
            has_detached_target <- detached_target_endpoint.is_some();
            active_source_endpoint <- any(...);
            active_source_endpoint <+ detached_source_endpoint;
            active_source_endpoint <+ out.hover_node_output.gate(&has_detached_target);

            // Whenever the detached edge is connected to a target, monitor for that target's port
            // existence. If it becomes invalid, the edge must be dropped.
            target_node_updated <- out.node_widget_tree_rebuilt.map2(&detached_target_node,
                |&rebuilt_id, &detached_id| Some(rebuilt_id) == detached_id
            );
            check_endpoint <- any_(&detached_target_endpoint, &target_node_updated).debounce();
            target_endpoint_to_check <- detached_target_endpoint.sample(&check_endpoint);
            detached_target_port_valid <- target_endpoint_to_check.map_some(
                f!((e) model
                    .try_with_node(e.node_id, |node| node.model().input.has_port(e.port))
                    .unwrap_or(false))
            ).unwrap();
            state.clear_detached_edge <+ detached_target_port_valid.on_false();

            // Whenever the node the detached edge is connected to is removed, drop the edge.
            state.clear_detached_edge <+ out.node_removed.map2(&detached_node,
                |&removed_id, &detached_id| Some(removed_id) == detached_id
            ).on_true();

        }
    }

    fn frp_init_edge_bg_drop(
        &self,
        state: &EdgeStateFrp,
        bg: &BgInteractionFrp,
    ) -> frp::Stream<EdgeEndpoint> {
        let network = self.frp.network();
        let touch = &self.model.touch_state;

        frp::extend! { network
            connect_drag_mode <- any(...);
            connect_drag_mode <+ touch.output_port.down.constant(true);
            connect_drag_mode <+ touch.input_port.down.constant(true);
            connect_drag_mode <+ touch.output_port.selected.constant(false);
            connect_drag_mode <+ touch.input_port.selected.constant(false);
            drop_onto_background <- bg.pointer_up.gate(&connect_drag_mode);
            perform_drop <- any_(drop_onto_background, bg.clicked_with_detached_edge);
            dropped_detached_edge <- state.detached_edge.sample(&perform_drop).unwrap();
            dropped_with_source <- dropped_detached_edge.filter_map(|d| d.source_endpoint());
            dropped_with_target <- dropped_detached_edge.filter_map(|d| d.target_endpoint());

            // Only consider creating a node from detached edge if it was still connected on the
            // source side.
            let edge_dropped_to_create_node = dropped_with_source;
            // Otherwise, remove the edge completely.
            state.break_detached_connection <+ dropped_with_target;
            state.clear_detached_edge <+ dropped_with_target;
        }
        edge_dropped_to_create_node
    }
}

impl application::View for GraphEditor {
    fn label() -> &'static str {
        "GraphEditor"
    }

    fn new(app: &Application) -> Self {
        GraphEditor::new(app)
    }

    fn global_shortcuts() -> Vec<application::shortcut::Shortcut> {
        use crate::shortcuts::SHORTCUTS;
        SHORTCUTS.iter().map(|(a, b, c, d)| Self::self_shortcut_when(*a, *c, *d, *b)).collect()
    }
}

/// Initialize remaining parts of graph editor FRP network. See [`GraphEditor::init`].
///
/// NOTE[PG]: This should eventually be refactored into smaller methods. I want to chip away at this
/// over time, especially once the new FRP system is in place. For the time being, it is kept in one
/// big mess left over from the old days. If you are in need to modify this method, consider
/// separating parts relevant to you into separate `frp_init` method.
fn init_remaining_graph_editor_frp(
    graph_editor: &GraphEditor,
    edge_state: &EdgeStateFrp,
    bg_interaction: &BgInteractionFrp,
    edge_interaction: &EdgeInteractionFrp,
    edge_color: &EdgeColorFrp,
) {
    let frp = &graph_editor.frp;
    let app = &graph_editor.model.app;
    let network = frp.network();
    let out = &frp.private.output;
    let inputs = &frp.private.input;
    let model = &graph_editor.model;
    let scene = model.scene();
    let mouse = &scene.mouse.frp_deprecated;
    let cursor = &model.app.cursor.frp;
    let touch = &model.touch_state;
    let vis_registry = &model.vis_registry;
    let selection_controller = &model.selection_controller;



    // ======================
    // === Read-only mode ===
    // ======================

    frp::extend! { network
        out.read_only <+ inputs.set_read_only;
    }

    // ========================
    // === Scene Navigation ===
    // ========================

    frp::extend! { network
        navigator_disabled <- out.some_visualization_selected.or(&inputs.set_navigator_disabled);
        model.navigator.frp.set_enabled <+ navigator_disabled.not();
        out.navigator_active <+ model.navigator.frp.enabled;
    }



    // =============================
    // === Node Level Navigation ===
    // =============================

    frp::extend! { network

        target_to_enter <- inputs.enter_hovered_node.map(f_!(scene.mouse.target.get()));

        // Go level up on background click.
        enter_on_background <= target_to_enter.map(|target| target.is_background().as_some(()));
        out.node_exited <+ enter_on_background;

        // Go level down on node double click.
        enter_on_node <= target_to_enter.map(|target| target.is_symbol().as_some(()));
        output_port_is_hovered <- frp.output.hover_node_output.map(Option::is_some);
        enter_node <- enter_on_node.gate_not(&output_port_is_hovered);
        node_switch_to_enter <- out.node_hovered.sample(&enter_node);
        node_to_enter <- node_switch_to_enter.filter_map(|switch| switch.into_on());
        out.node_entered <+ node_to_enter;
    }



    // =========================
    // === User Interactions ===
    // =========================

    // === Adding Node ===

    frp::extend! { network
        node_added_with_button <- model.add_node_button.clicked.gate_not(&inputs.set_read_only);
        start_node_creation_from_port <- out.hover_node_output.sample(
            &inputs.start_node_creation_from_port
        ).unwrap();

        input_add_node_way <- inputs.add_node.constant(WayOfCreatingNode::AddNodeEvent);
        input_start_creation_way <- inputs.start_node_creation.filter_map(f_!(
            // Only start node creation if nothing is focused. This is to prevent
            // creating nodes when we are editing texts and press enter.
            scene.focused_instance().is_none().then_some(WayOfCreatingNode::StartCreationEvent)
        ));
        start_creation_from_port_way <- start_node_creation_from_port.map(
            |&endpoint| WayOfCreatingNode::StartCreationFromPortEvent{ endpoint });
        add_with_button_way <- node_added_with_button.constant(WayOfCreatingNode::ClickingButton);
        add_with_edge_drop_way <- edge_interaction.create_node_from_edge.map(
            |&endpoint| WayOfCreatingNode::DroppingEdge { endpoint });

        add_node_way <- any(...);
        add_node_way <+ input_add_node_way;
        add_node_way <+ input_start_creation_way;
        add_node_way <+ start_creation_from_port_way;
        add_node_way <+ add_with_button_way;
        add_node_way <+ add_with_edge_drop_way;

        node_pointer_style <- any(...);
        let node_ctx = NodeCreationContext {
            pointer_style: node_pointer_style.clone_ref(),
            output_press:  touch.output_port.down.clone_ref(),
            input_press:   touch.input_port.down.clone_ref(),
        };
        new_node <- add_node_way.map2(&cursor.scene_position,
            f!((way, cursor_pos) model.create_node(&node_ctx, way, cursor_pos.xy()))
        );
        out.node_added <+ new_node;
        node_to_edit_after_adding <- new_node.filter_map(|&(id,_,do_edit)| do_edit.as_some(id));

        let on_before_rendering = ensogl::animation::on_before_rendering();
        node_to_pan <- new_node._0().debounce();
        time_to_pan <- on_before_rendering.sync_gate(&node_to_pan);
        pan_camera_to_node <- node_to_pan.sample(&time_to_pan);
        eval pan_camera_to_node((&node) model.pan_camera_to_node(node));
    }


    // === Node Editing ===

    frp::extend! { network
        node_in_edit_mode <- out.node_being_edited.map(|n| n.is_some());
        edit_mode <- bool(&inputs.edit_mode_off,&inputs.edit_mode_on);
        clicked_node <- touch.nodes.down.gate(&edit_mode);
        clicked_and_edited_nodes <- clicked_node.map2(&out.node_being_edited, |n, c| (*n, *c));
        let not_being_edited_already = |(clicked, edited): &(NodeId, Option<NodeId>)| {
            if let Some(edited) = edited { edited != clicked } else { true }
        };
        node_to_edit <- clicked_and_edited_nodes.filter(not_being_edited_already)._0();
        edit_node <- any(node_to_edit, node_to_edit_after_adding, inputs.edit_node);
        stop_edit_on_bg_click <- bg_interaction.clicked_without_detached_edge.gate(&node_in_edit_mode);
        stop_edit <- any(&stop_edit_on_bg_click,&inputs.stop_editing);
        edit_switch <- edit_node.gate(&node_in_edit_mode);
        node_being_edited <- out.node_being_edited.map(|n| n.unwrap_or_default());

        // The "finish" events must be emitted before "start", to properly cover the "switch" case.
        out.node_editing_finished <+ node_being_edited.sample(&stop_edit);
        out.node_editing_finished <+ node_being_edited.sample(&edit_switch);
        out.node_editing_started  <+ edit_node;

        out.node_being_edited <+ out.node_editing_started.map(|n| Some(*n));
        out.node_being_edited <+ out.node_editing_finished.constant(None);
        out.node_editing <+ out.node_being_edited.map(|t|t.is_some());

        out.node_edit_mode <+ edit_mode;
        out.nodes_labels_visible <+ out.node_edit_mode || node_in_edit_mode;

        eval out.node_editing_started ([model] (id) {
            let _profiler = profiler::start_debug!(profiler::APP_LIFETIME, "node_editing_started");
            if let Some(node) = model.nodes.get_cloned_ref(id) {
                node.model().input.set_editing(true);
            }
        });
        eval out.node_editing_finished ([model](id) {
            let _profiler = profiler::start_debug!(profiler::APP_LIFETIME, "node_editing_finished");
            if let Some(node) = model.nodes.get_cloned_ref(id) {
                node.model().input.set_editing(false);
            }
        });
    }


    // === Edited node growth/shrink animation ===

    component::node::growth_animation::initialize_edited_node_animator(model, frp, scene);


    // === Event Propagation ===

    // See the docs of `Node` to learn about how the graph - nodes event propagation works.
    frp::extend! { network
        _eval <- all_with(&out.node_hovered,&edit_mode,
            f!((tgt, e) model.try_with_node(tgt.value,|t| t.set_edit_ready_mode(*e && tgt.is_on())))
        );
        _eval <- all_with(&out.node_hovered,&edge_state.detached_edge,
            f!([model](hover_target,detached) {
                let node_id = hover_target.value;
                let detached_source = detached.and_then(|d| d.source_endpoint());
                let can_attach = detached_source.map_or(false, |e| e.node_id != node_id);
                let is_active = can_attach && hover_target.is_on();
                model.try_with_node(node_id,|t| t.model().input.set_ports_active(is_active));
            }
        ));
    }


    // === Node Actions ===

    frp::extend! { network
        eval out.node_action_freeze([model]((node_id,is_frozen)) {
            for edge_id in model.node_in_edges(node_id) {
                model.set_edge_freeze(edge_id,*is_frozen);
            }
        });
    }


    // === Remove Node ===

    frp::extend! { network
        all_nodes       <= inputs.remove_all_nodes.map(f_!(model.nodes.keys()));
        selected_nodes  <= inputs.remove_selected_nodes.map(f_!(model.nodes.all_selected()));
        nodes_to_remove <- any (all_nodes, selected_nodes);
        out.node_removed <+ nodes_to_remove;
    }


    // === Collapse Nodes ===
    frp::extend! { network
    // TODO [mwu] https://github.com/enso-org/ide/issues/760
    //   This is currently the provisional code to enable collapse nodes refactoring. While the APIs
    //   are as-intended, their behavior isn't. Please refer to the issue for details.
    let empty_id       = NodeId::default();
    let model_clone    = model.clone_ref();
    nodes_to_collapse <- inputs.collapse_selected_nodes . map(move |_|
        (model_clone.nodes.all_selected(),empty_id)
    );
    out.nodes_collapsed <+ nodes_to_collapse;
    }


    // === Set Node SKIP/FREEZE macros and context switch expression ===

    frp::extend! { network
        eval inputs.set_node_skip(((id, skip)) model.set_node_skip(*id, *skip));
        eval inputs.set_node_freeze(((id, freeze)) model.set_node_freeze(*id, *freeze));
        eval inputs.set_node_context_switch(((id, context_switch))
            model.set_node_context_switch(*id, context_switch)
        );
    }


    // === Copy-Paste ===

    frp::extend! { network
        out.node_copied <+ inputs.copy_selected_node.map(f_!(model.nodes.last_selected())).unwrap();
        cursor_pos_at_paste <- cursor.scene_position.sample(&inputs.paste_node).map(|v| v.xy());
        out.request_paste_node <+ cursor_pos_at_paste.map(
            f!([model](pos) new_node_position::at_mouse_aligned_to_close_nodes(&model, *pos))
        );
    }


    // === Set Node Comment ===
    frp::extend! { network

    eval inputs.set_node_comment(((id,comment)) model.set_node_comment(*id,comment));
    }

    // === Set Node Error ===
    frp::extend! { network

    eval inputs.set_node_error_status([model]((node_id, error)) {
        model.with_node(*node_id, |n| n.set_error.emit(error))
    });

    }

    // === Set Node Pending ===
    frp::extend! { network

    eval inputs.set_node_pending_status([model]((node_id, is_pending)) {
        model.with_node(*node_id, |n| n.set_pending.emit(is_pending))
    });

    }



    // ==================
    // === Move Nodes ===
    // ==================
    frp::extend! { network

    mouse_pos <- mouse.position.map(|p| Vector2(p.x,p.y));

    // === Discovering drag targets ===

    let node_down      = touch.nodes.down.clone_ref();
    node_in_edit_mode <- node_down.map2(&out.node_being_edited,|t,s| Some(*t) == *s);
    node_was_selected <- node_down.map(f!((id) model.nodes.selected.contains(id)));
    tgts_if_non_sel   <- node_down.map(|id|vec![*id]).gate_not(&node_was_selected);
    tgts_if_sel       <- node_down.map(f_!(model.nodes.selected.items())).gate(&node_was_selected);
    tgts_if_non_edit  <- any(tgts_if_non_sel,tgts_if_sel).gate_not(&node_in_edit_mode);
    tgts_if_edit      <- node_down.map(|_|default()).gate(&node_in_edit_mode);
    drag_tgts         <- any(tgts_if_non_edit,tgts_if_edit);
    any_drag_tgt      <- drag_tgts.map(|t|!t.is_empty());
    node_pos_on_down  <- node_down.map(f!((id) model.node_position(*id)));
    mouse_pos_on_down <- mouse_pos.sample(&node_down);
    mouse_pos_diff    <- mouse_pos.map2(&mouse_pos_on_down,|t,s|t-s).gate(&touch.nodes.is_down);
    node_pos_diff     <- mouse_pos_diff.map(f!([scene](t) t / scene.camera().zoom()));
    node_tgt_pos_rt   <- node_pos_diff.map2(&node_pos_on_down,|t,s|t+s);
    just_pressed      <- bool (&node_tgt_pos_rt,&node_pos_on_down);
    node_tgt_pos_rt   <- any  (&node_tgt_pos_rt,&node_pos_on_down);


    // === Snapping ===

    eval drag_tgts ((ids) model.disable_grid_snapping_for(ids));
    let node_tgt_pos_anim = DEPRECATED_Animation::<Vector2<f32>>::new(network);
    let x_snap_strength   = Easing::new(network);
    let y_snap_strength   = Easing::new(network);
    x_snap_strength.set_duration(300.0);
    y_snap_strength.set_duration(300.0);

    _eval <- node_tgt_pos_rt.map2(&just_pressed,
        f!([model,x_snap_strength,y_snap_strength,node_tgt_pos_anim](pos,just_pressed) {
            let snapped = model.nodes.check_grid_magnet(*pos);
            let x = snapped.x.unwrap_or(pos.x);
            let y = snapped.y.unwrap_or(pos.y);
            x_snap_strength.target(if snapped.x.is_none() { 0.0 } else { 1.0 });
            y_snap_strength.target(if snapped.y.is_none() { 0.0 } else { 1.0 });
            node_tgt_pos_anim.set_target_value(Vector2::new(x,y));
            if *just_pressed {
                node_tgt_pos_anim.set_target_value(*pos);
                x_snap_strength.skip();
                y_snap_strength.skip();
                node_tgt_pos_anim.skip();
            }
    }));

    node_tgt_pos <- all_with4
        ( &node_tgt_pos_rt
        , &node_tgt_pos_anim.value
        , &x_snap_strength.value
        , &y_snap_strength.value
        , |rt,snap,xw,yw| {
            let w     = Vector2(*xw,*yw);
            let w_inv = Vector2(1.0,1.0) - w;
            rt.component_mul(&w_inv) + snap.component_mul(&w)
        });


    // === Update All Target Nodes Positions ===

    main_tgt_pos_prev <- node_tgt_pos.previous();
    main_tgt_pos_diff <- node_tgt_pos.map2(&main_tgt_pos_prev,|t,s|t-s).gate_not(&just_pressed);
    drag_tgt          <= drag_tgts.sample(&main_tgt_pos_diff);
    tgt_new_pos       <- drag_tgt.map2(&main_tgt_pos_diff,f!((id,tx) model.node_pos_mod(*id,*tx)));
    out.node_position_set <+ tgt_new_pos;


    // === Batch Update ===

    after_drag             <- touch.nodes.up.gate_not(&just_pressed);
    tgt_after_drag         <= drag_tgts.sample(&after_drag);
    tgt_after_drag_new_pos <- tgt_after_drag.map(f!([model](id)(*id,model.node_position(*id))));
    out.node_position_set_batched <+ tgt_after_drag_new_pos;

    // === Set Node Position ===

    out.node_position_set         <+ inputs.set_node_position;
    out.node_position_set_batched <+ inputs.set_node_position;
    eval out.node_position_set (((id,pos)) model.set_node_position(*id,*pos));

    }

    // === Vis Set ===
    frp::extend! { network

    def _update_vis_data = inputs.set_visualization.map(f!([model,vis_registry]((node_id,path)) {
        model.with_node(*node_id, |node| {
            let definition = path.as_ref().and_then(|path| vis_registry.definition_from_path(path));
            node.model().visualization.frp.set_visualization.emit(definition);
        })
    }));
    }


    // === Vis Selection ===
    frp::extend! { network
        eval out.on_visualization_select ([model](switch) {
            if switch.is_on() {
                model.visualizations.selected.insert(switch.value);
            } else {
                model.visualizations.selected.remove(&switch.value);
            }
        });

        out.some_visualization_selected <+  out.on_visualization_select.map(f_!([model] {
            !model.visualizations.selected.is_empty()
        }));
    };


    // === Vis Update Data ===

    frp::extend! { network
    eval inputs.set_visualization_data (((node_id,data))
        model.with_node(*node_id, |node|  node.model().visualization.frp.set_data.emit(data));
    );

    eval inputs.set_error_visualization_data (((node_id,data))
        model.with_node(*node_id, |node|  node.model().error_visualization.send_data.emit(data))
    );

    nodes_to_cycle <= inputs.cycle_visualization_for_selected_node.map(f_!(model.nodes.all_selected()));
    node_to_cycle  <- any(nodes_to_cycle,inputs.cycle_visualization);
    eval node_to_cycle ((node_id)
        model.with_node(*node_id, |n| n.model().visualization.frp.cycle_visualization())
    );


    // === Visualization toggle ===
    //
    // Algorithm:
    //     - Press key. If all selected nodes have enabled vis, disable them.
    //     - If not, enable vis on missing nodes.
    //     - Release key. If the time passed from key press was short, do nothing.
    //     - If it was long, disable vis which were disabled (preview mode).

    let viz_press_ev = inputs.press_visualization_visibility.clone_ref();
    let viz_open_fs_ev = inputs.open_fullscreen_visualization.clone_ref();
    let viz_release_ev = inputs.release_visualization_visibility.clone_ref();
    viz_pressed <- bool(&viz_release_ev,&viz_press_ev);
    viz_was_pressed <- viz_pressed.previous();
    viz_press <- viz_press_ev.gate_not(&viz_was_pressed);
    viz_release <- viz_release_ev.gate(&viz_was_pressed);
    viz_press_time <- viz_press.map(|_| {
            let time = web::window.performance_or_panic().now() as f32;
            let frame_counter = Rc::new(web::FrameCounter::start_counting());
            (time, Some(frame_counter))
        });
    viz_release_time <- viz_release.map(|_| web::window.performance_or_panic().now() as f32);
    viz_preview_mode <- viz_release_time.map2(&viz_press_time,|t1,(t0,counter)| {
        let diff = t1-t0;
        // We check the time between key down and key up. If the time is less than the threshold
        // then it was a key press and we do not want to enter preview mode. If it is longer then
        // it was a key hold and we want to enter preview mode.
        let long_enough = diff > VIZ_PREVIEW_MODE_TOGGLE_TIME_MS;
        // We also check the number of passed frames, since the time measure can be misleading, if
        // there were dropped frames. The visualization might have just appeared while more than
        // the threshold time has passed.
        let enough_frames = if let Some(counter) = counter {
            let frames = counter.frames_since_start();
            frames > VIZ_PREVIEW_MODE_TOGGLE_FRAMES
        } else {
            false
        };
        long_enough && enough_frames
    });
    viz_preview_mode_end <- viz_release.gate(&viz_preview_mode).gate_not(&out.is_fs_visualization_displayed);
    viz_tgt_nodes <- viz_press.gate_not(&out.is_fs_visualization_displayed).map(f_!(model.nodes.all_selected()));
    viz_tgt_nodes_off <- viz_tgt_nodes.map(f!([model](node_ids) {
        node_ids.iter().cloned().filter(|node_id| {
            model.with_node(*node_id, |n| !n.visualization().visible.value()).unwrap_or_default()
        }).collect_vec()
    }));

    viz_tgt_nodes_all_on <- viz_tgt_nodes_off.map(|t| t.is_empty());
    viz_enable_by_press <= viz_tgt_nodes.gate_not(&viz_tgt_nodes_all_on);
    viz_enable <- any(viz_enable_by_press,inputs.enable_visualization);
    viz_disable_by_press <= viz_tgt_nodes.gate(&viz_tgt_nodes_all_on);
    viz_update_failed <- inputs.visualization_update_failed._0();
    viz_disable <- any(viz_disable_by_press, inputs.disable_visualization, viz_update_failed);
    viz_preview_disable <= viz_tgt_nodes_off.sample(&viz_preview_mode_end);
    viz_fullscreen_on <= viz_open_fs_ev.map(f_!(model.nodes.last_selected()));

    eval viz_enable          ((id) model.enable_visualization(id));
    eval viz_disable         ((id) model.disable_visualization(id));
    eval viz_preview_disable ((id) model.disable_visualization(id));
    fullscreen_vis_was_enabled <- viz_fullscreen_on.filter_map(f!((id)
        model.enable_visualization_fullscreen(id).then(|| *id))
    );

    viz_fs_to_close <- out.visualization_fullscreen.sample(&inputs.close_fullscreen_visualization);
    eval viz_fs_to_close ([model](vis) {
        if let Some(vis) = vis {
            model.disable_visualization_fullscreen(vis);
            model.enable_visualization(vis);
        }
    });

    out.visualization_fullscreen <+ fullscreen_vis_was_enabled.map(|id| Some(*id));
    out.visualization_fullscreen <+ inputs.close_fullscreen_visualization.constant(None);

    out.is_fs_visualization_displayed <+ out.visualization_fullscreen.map(Option::is_some);

    out.visualization_update_error <+ inputs.visualization_update_failed;

    eval inputs.show_node_editing_preview ((id) model.show_node_editing_preview(id));


    // === Register Visualization ===

    eval inputs.register_visualization ([vis_registry](handle) {
        if let Some(handle) = handle {
            vis_registry.add(handle);
        }
    });
    eval inputs.reset_visualization_registry ([vis_registry](()) {
        vis_registry.remove_all_visualizations();
        vis_registry.add_default_visualizations();
    });
    out.visualization_registry_reload_requested <+ inputs.reload_visualization_registry;


    // === Entering and Exiting Nodes ===

    node_to_enter           <= inputs.enter_selected_node.map(f_!(model.nodes.last_selected()));
    out.node_entered <+ node_to_enter;
    out.node_exited  <+ inputs.exit_node;

    // ================
    // === Node VCS ===
    // ================

    eval inputs.set_node_vcs_status(((node_id,status))
        model.with_node(*node_id, |node| node.set_vcs_status.emit(status))
    );



    // ===================
    // === Other Binds ===
    // ===================

    eval out.node_selected   ((id) model.nodes.select(id));
    eval out.node_deselected ((id) model.nodes.deselect(id));
    eval out.node_removed    ((id) model.remove_node(*id));
    out.on_visualization_select <+ out.node_removed.map(|&id| Switch::Off(id));

    // === Remove implementation ===
    out.node_removed <+ inputs.remove_node;
    }


    // =====================
    // === Pointer Style ===
    // =====================

    frp::extend! { network

    cursor_on_drag_down <- touch.nodes.down.gate(&any_drag_tgt).constant(
        cursor::Style::new_with_all_fields_default().press()
    );
    cursor_on_drag_up   <- touch.nodes.up.constant(cursor::Style::default());
    pointer_on_drag     <- any (&cursor_on_drag_down,&cursor_on_drag_up);

    detached_edge_style <- edge_state.detached_edge.all_with(&edge_color.edges_with_updated_color,
        f!([model](d, _) {
            let color = d.and_then(|d| d.edge_id()).map(|id| model.edge_color(id));
            color.map_or_default(|c| cursor::Style::new_color(c).press())
        })
    );

    let selection_style  = selection_controller.cursor_style.clone_ref();

    cursor.set_style <+ all
        [ pointer_on_drag
        , selection_style
        , node_pointer_style
        , detached_edge_style
        ].fold();
    }

    // ==============================
    // === Component Interactions ===
    // ==============================

    // === Nodes + Selection ===

    // Do not show quick actions on hover while doing an area selection.
    frp::extend! { network
        eval selection_controller.area_selection ((area_selection) model.nodes.show_quick_actions(!area_selection));
    }

    // === Visualization + Selection ===

    // Do not allow area selection while we show a fullscreen visualization.
    frp::extend! { network
        allow_area_selection <- out.is_fs_visualization_displayed.not();
        eval allow_area_selection ((area_selection)
            selection_controller.enable_area_selection.emit(area_selection)
        );
    }

    // ===============
    // === Tooltip ===
    // ===============

    frp::extend! { network

        model.tooltip.frp.set_style <+ app.frp.tooltip;

        quick_visualization_preview <- bool(&frp.disable_quick_visualization_preview,
                                            &frp.enable_quick_visualization_preview);
        eval quick_visualization_preview((value) model.nodes.set_quick_preview(*value));
    }



    // =====================
    // === Dropped Files ===
    // =====================

    use theme::graph_editor::default_y_gap_between_nodes as gap_path;
    let default_gap = model.styles_frp.get_number_or(gap_path, 0.0);
    let files_received = model.drop_manager.files_received().clone_ref();
    frp::extend! { network
        files_with_positions <- files_received.map2(&default_gap,
            move |drop_event_data,default_gap| {
                let files = &drop_event_data.files;
                let drop_posititon = drop_event_data.position;
                let single_offset = default_gap + node::HEIGHT;
                files.iter().enumerate().map(|(index,file)| {
                    let offset = Vector2(0.0, single_offset * index as f32);
                    (file.clone_ref(), drop_posititon + offset)
                }).collect_vec()
            }
        );
        file_dropped            <= files_with_positions;
        out.file_dropped <+ file_dropped;
    }



    // =========================
    // === Gap Between Nodes ===
    // =========================

    let style_sheet = &scene.style_sheet;
    let styles = StyleWatchFrp::new(style_sheet);
    let default_x_gap_path = ensogl_hardcoded_theme::graph_editor::default_x_gap_between_nodes;
    let default_y_gap_path = ensogl_hardcoded_theme::graph_editor::default_y_gap_between_nodes;
    let min_x_spacing_path = ensogl_hardcoded_theme::graph_editor::minimal_x_spacing_for_new_nodes;
    let default_x_gap = styles.get_number_or(default_x_gap_path, 0.0);
    let default_y_gap = styles.get_number_or(default_y_gap_path, 0.0);
    let min_x_spacing = styles.get_number_or(min_x_spacing_path, 0.0);
    frp::extend! { network
        frp.private.output.default_x_gap_between_nodes <+ default_x_gap;
        frp.private.output.default_y_gap_between_nodes <+ default_y_gap;
        frp.private.output.min_x_spacing_for_new_nodes <+ min_x_spacing;
    }
    frp.private.output.default_x_gap_between_nodes.emit(default_x_gap.value());
    frp.private.output.default_y_gap_between_nodes.emit(default_y_gap.value());
    frp.private.output.min_x_spacing_for_new_nodes.emit(min_x_spacing.value());



    // ==================
    // === Debug Mode ===
    // ==================

    frp::extend! { network
        out.debug_mode <+ frp.set_debug_mode;

        limit_max_zoom <- frp.set_debug_mode.on_false();
        unlimit_max_zoom <- frp.set_debug_mode.on_true();
        eval_ limit_max_zoom (model.navigator.set_max_zoom(Some(MAX_ZOOM)));
        eval_ unlimit_max_zoom (model.navigator.set_max_zoom(None));
    }

    // Init defaults
    frp.edit_mode_off.emit(());
    frp.set_debug_mode.emit(false);
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use application::test_utils::ApplicationExt;
    use ensogl::animation::test_utils::next_frame;
    use ensogl::display::scene::test_utils::MouseExt;
    use node::test_utils::NodeModelExt;

    #[test]
    fn test_adding_node_by_internal_api() {
        let (_, graph_editor) = init();
        assert_eq!(graph_editor.num_nodes(), 0);
        graph_editor.add_node();
        assert_eq!(graph_editor.num_nodes(), 1);
        graph_editor.assert(Case { node_source: None, should_edit: false });
    }

    #[test]
    fn test_adding_node_by_shortcut() {
        test_adding_node(press_add_node_shortcut);
    }

    fn press_add_node_shortcut(editor: &GraphEditor) {
        editor.start_node_creation();
    }

    #[test]
    fn test_adding_node_by_adding_node_button() {
        test_adding_node(click_add_node_button);
    }

    fn click_add_node_button(editor: &GraphEditor) {
        let adding_node_button = &editor.model.add_node_button;
        adding_node_button.click();
    }

    fn test_adding_node(add_node: impl Fn(&GraphEditor)) {
        let (app, graph_editor) = init();
        assert_eq!(graph_editor.num_nodes(), 0);

        // Adding first node.
        let (node_1_id, node_1) = graph_editor.add_node_by(&add_node);
        graph_editor.assert(Case { node_source: None, should_edit: true });
        graph_editor.stop_editing();
        assert_eq!(graph_editor.num_nodes(), 1);
        next_frame();

        // First node is created in the center of the screen.
        let node_1_pos = node_1.position();
        let screen_center = app.display.default_scene.screen_to_scene_coordinates(Vector3::zeros());
        assert_eq!(node_1_pos.xy(), screen_center.xy());

        // Adding second node with the first node selected.
        graph_editor.model.nodes.select(node_1_id);
        let (_, node_2) = graph_editor.add_node_by(&add_node);
        graph_editor.assert(Case { node_source: Some(node_1_id), should_edit: true });
        assert_eq!(graph_editor.num_nodes(), 2);

        // Second node is below the first and left-aligned to it.
        let node_2_pos = node_2.position();
        assert!(node_2_pos.y < node_1_pos.y);
        assert_eq!(node_2_pos.x, node_1_pos.x);
    }

    #[test]
    fn test_adding_node_by_dropping_edge() {
        let (app, graph_editor) = init();
        let mouse = &app.display.default_scene.mouse;

        assert_eq!(graph_editor.num_nodes(), 0);
        // Adding a new node.
        let (node_1_id, node_1) = graph_editor.add_node_by_api();
        graph_editor.stop_editing();
        next_frame();
        // Creating edge.
        let port = node_1.model().output_port_hover_shape().expect("No output port.");
        mouse.click_on(&port, Vector2::zero());
        assert_eq!(graph_editor.num_edges(), 1);
        // Dropping edge.
        let click_pos = Vector2(300.0, 300.0);
        let click_on_background = |_: &GraphEditor| mouse.click_on_background(click_pos);
        let (_, node_2) = graph_editor.add_node_by(&click_on_background);
        graph_editor.assert(Case { node_source: Some(node_1_id), should_edit: true });
        let node_pos = node_2.position();
        assert_eq!(node_pos.xy(), click_pos);
    }

    #[test]
    fn test_connecting_two_nodes() {
        let (app, ref graph_editor) = init();
        assert_eq!(graph_editor.num_nodes(), 0);
        assert_eq!(graph_editor.num_edges(), 0);
        let mouse = &app.display.default_scene.mouse;
        // Adding two nodes.
        let (node_id_1, node_1) = graph_editor.add_node_by_api();
        graph_editor.stop_editing();
        let (node_id_2, node_2) = graph_editor.add_node_by_api();
        graph_editor.stop_editing();
        next_frame();
        // Creating edge.
        let port = node_1.model().output_port_hover_shape().expect("No output port.");
        mouse.click_on(&*port, Vector2::zero());
        assert_eq!(graph_editor.num_edges(), 1);
        let edge_id = *graph_editor.model.edges.borrow().keys().next().expect("Edge was not added");
        graph_editor.model.with_edge(edge_id, |edge| {
            assert_eq!(edge.source().map(|e| e.node_id), Some(node_id_1));
            assert_eq!(edge.target(), None);
        });

        // Connecting edge.
        // We need to enable ports. Normally it is done by hovering the node.
        node_2.model().input.frp.set_ports_active(true);
        let port_hover = node_2.model().input_port_hover_shape().expect("No input port.");
        next_frame();

        mouse.click_on(&*port_hover, Vector2::zero());
        graph_editor.model.with_edge(edge_id, |edge| {
            assert_eq!(edge.source().map(|e| e.node_id), Some(node_id_1));
            assert_eq!(edge.target().map(|e| e.node_id), Some(node_id_2));
        });
    }

    #[test]
    // The alignment is disabled for mouse-oriented node placement. See [`new_node_position`] docs.
    #[ignore]
    fn test_magnet_alignment_when_adding_node_by_shortcut() {
        test_magnet_alignment_when_adding_node(move_mouse_and_add_node_by_shortcut);
    }

    fn move_mouse_and_add_node_by_shortcut(
        scene: &Scene,
        editor: &GraphEditor,
        mouse_pos: Vector2,
    ) {
        scene.mouse.hover_background(mouse_pos);
        press_add_node_shortcut(editor);
    }

    #[test]
    fn test_magnet_alignment_when_adding_node_by_add_node_button() {
        test_magnet_alignment_when_adding_node(move_camera_and_click_add_node_button);
    }

    fn move_camera_and_click_add_node_button(
        scene: &Scene,
        editor: &GraphEditor,
        camera_pos: Vector2,
    ) {
        let camera = &scene.camera();
        camera.set_xy(camera_pos);
        camera.update(scene);
        click_add_node_button(editor);
    }

    fn test_magnet_alignment_when_adding_node(add_node_at: impl Fn(&Scene, &GraphEditor, Vector2)) {
        let (app, graph_editor) = init();
        let scene = &app.display.default_scene;
        let add_node_at = |editor: &GraphEditor, pos: Vector2| add_node_at(scene, editor, pos);

        // Create two nodes, with the 2nd one positioned below and far to the right from the 1st
        // one.
        let (_, node_1) = graph_editor.add_node_by_api_at_pos(Vector2(0.0, 0.0));
        let (_, node_2) = graph_editor.add_node_by_api_at_pos(Vector2(800.0, -100.0));

        // Create third node, placing it roughly below the 1st one and to the left of the 2nd one,
        // but slightly displaced from a position aligned to them both. Verify that the magnet
        // algorithm repositions the node such that it is aligned with existing nodes.
        let aligned_pos = Vector2(node_1.position().x, node_2.position().y);
        let displacement = Vector2(7.0, 8.0);
        test_node_added_with_displacement_gets_aligned(
            &graph_editor,
            aligned_pos,
            displacement,
            add_node_at,
        );

        // Create fourth node, placing it roughly to the right of the 1st node and above the 2nd
        // one, but slightly displaced from a position aligned to them both. Verify that the magnet
        // algorithm repositions the node such that it is aligned with existing nodes.
        let aligned_pos = Vector2(node_2.position().x, node_1.position().y);
        test_node_added_with_displacement_gets_aligned(
            &graph_editor,
            aligned_pos,
            displacement,
            add_node_at,
        );
    }

    fn test_node_added_with_displacement_gets_aligned(
        graph_editor: &GraphEditor,
        aligned_pos: Vector2,
        displacement: Vector2,
        add_node_at: impl Fn(&GraphEditor, Vector2),
    ) {
        enso_frp::microtasks::flush_microtasks();
        let unaligned_pos = aligned_pos + displacement;
        let add_node_unaligned = |editor: &GraphEditor| add_node_at(editor, unaligned_pos);
        let (_, node) = graph_editor.add_node_by(&add_node_unaligned);
        graph_editor.stop_editing();
        assert_eq!(node.position().xy(), aligned_pos);
    }

    #[test]
    fn test_magnet_alignment_when_no_space_for_node_added_with_add_node_button() {
        let (app, graph_editor) = init();
        let scene = &app.display.default_scene;

        // Create 1st node.
        let (node_1_id, node_1) = graph_editor.add_node_by_api();
        graph_editor.stop_editing();

        // Create 2nd node below the 1st one and move it slightly to the right.
        graph_editor.model.nodes.select(node_1_id);
        let (node_2_id, node_2) = graph_editor.add_node_by(&press_add_node_shortcut);
        node_2.update_x(|x| x + 16.0);

        // Create 3rd node below the 2nd one and move it slightly down and far to the right.
        graph_editor.model.nodes.select(node_2_id);
        let (_, node_3) = graph_editor.add_node_by(&press_add_node_shortcut);
        node_2.update_xy(|pos| pos + Vector2(800.0, -7.0));

        // Create 4th node by clicking (+) button when camera is roughly centered at the 1st node.
        let small_displacement = Vector2(8.0, 9.0);
        let pos_near_node_1 = node_1.position().xy() + small_displacement;
        let add_node = |editor: &GraphEditor| {
            move_camera_and_click_add_node_button(scene, editor, pos_near_node_1)
        };
        let (_, node_4) = graph_editor.add_node_by(&add_node);
        let aligned_pos = Vector2(node_1.position().x, node_3.position().y);
        assert_eq!(node_4.position().xy(), aligned_pos);
    }


    // === Test utilities ===

    /// An assertion case used when adding new nodes. See [`GraphEditor::assert`] below.
    struct Case {
        /// A source node of the added node.
        node_source: Option<NodeId>,
        /// Should we start the node editing immediately after adding it?
        should_edit: bool,
    }

    impl GraphEditor {
        fn num_edges(&self) -> usize {
            self.model.edges.borrow().len()
        }

        /// Get the number of nodes currently present in the graph.
        pub fn num_nodes(&self) -> usize {
            self.model.nodes.len()
        }

        fn add_node_by<F: Fn(&GraphEditor)>(&self, add_node: &F) -> (NodeId, Node) {
            let (old_node_id, ..) = self.node_added.value();
            add_node(self);
            let (node_id, ..) = self.node_added.value();
            assert_ne!(node_id, old_node_id, "Node was not added.");
            let node = self.model.nodes.get_cloned_ref(&node_id).expect("Node was not added.");
            node.set_expression(node::Expression::new_plain("some_not_empty_expression"));
            (node_id, node)
        }

        fn add_node_by_api(&self) -> (NodeId, Node) {
            let add_node = |editor: &GraphEditor| editor.add_node();
            self.add_node_by(&add_node)
        }

        fn add_node_by_api_at_pos(&self, position: Vector2) -> (NodeId, Node) {
            let (node_id, node) = self.add_node_by_api();
            self.stop_editing();
            node.set_xy(position);
            (node_id, node)
        }

        fn assert(&self, case: Case) {
            let (added_node, node_source, should_edit) = self.node_added.value();
            let node_being_edited = self.node_being_edited.value();
            assert_eq!(
                should_edit, case.should_edit,
                "Node editing state does not match expected."
            );
            assert_eq!(should_edit, node_being_edited.is_some());
            if let Some(node_being_edited) = node_being_edited {
                assert_eq!(node_being_edited, added_node, "Edited node does not match added one.");
            }
            let node_source = node_source.map(|source| source.node);
            assert_eq!(node_source, case.node_source, "Source node does not match expected.");
        }
    }

    fn init() -> (Application, GraphEditor) {
        let app = Application::new("root");
        app.set_screen_size_for_tests();
        let graph_editor = GraphEditor::new(&app);
        app.display.add_child(&graph_editor);
        next_frame();
        (app, graph_editor)
    }
}
