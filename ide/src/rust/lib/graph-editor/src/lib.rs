#![allow(missing_docs)]

//! NOTE
//! This file is under a heavy development. It contains commented lines of code and some code may
//! be of poor quality. Expect drastic changes.

#![feature(associated_type_defaults)]
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
use crate::component::node::Node;
use crate::component::node::WeakNode;
use enso_frp as frp;
use enso_frp::io::keyboard;
use enso_frp::Position;
use ensogl::display::object::Id;
use ensogl::display::world::*;
use ensogl::display;
use ensogl::system::web::StyleSetter;
use ensogl::system::web;
use nalgebra::Vector2;




#[derive(Clone,CloneRef,Debug,Default)]
pub struct NodeSet {
    data : Rc<RefCell<HashMap<Id,Node>>>
}

impl NodeSet {
    pub fn borrow(&self) -> Ref<HashMap<Id,Node>> {
        self.data.borrow()
    }

    pub fn take(&self) -> HashMap<Id,Node> {
        mem::take(&mut *self.data.borrow_mut())
    }

    pub fn insert(&self, node:Node) {
        self.data.borrow_mut().insert(node.id(),node);
    }

    pub fn remove(&self, node:&Node) {
        self.data.borrow_mut().remove(&node.id());
    }

    pub fn contains(&self, node:&Node) -> bool {
        self.get(node.id()).is_some()
    }

    pub fn get(&self, id:Id) -> Option<Node> {
        self.data.borrow().get(&id).map(|t| t.clone_ref())
    }

    pub fn clear(&self) {
        self.data.borrow_mut().clear();
    }
}



#[derive(Clone,CloneRef,Debug,Default)]
pub struct WeakNodeSet {
    data : Rc<RefCell<HashMap<Id,WeakNode>>>
}

impl WeakNodeSet {
    pub fn borrow(&self) -> Ref<HashMap<Id,WeakNode>> {
        self.data.borrow()
    }

    pub fn take(&self) -> HashMap<Id,WeakNode> {
        mem::take(&mut *self.data.borrow_mut())
    }

    pub fn for_each_taken<F:Fn(Node)>(&self,f:F) {
        self.take().into_iter().for_each(|(_,node)| { node.upgrade().for_each(|n| f(n)) })
    }

    pub fn for_each<F:Fn(Node)>(&self,f:F) {
        self.data.borrow().iter().for_each(|(_,node)| { node.upgrade().for_each(|n| f(n)) })
    }

    pub fn insert(&self, node:&Node) {
        self.data.borrow_mut().insert(node.id(),node.downgrade());
    }

    pub fn contains(&self, node:&Node) -> bool {
        self.get(node.id()).is_some()
    }

    pub fn get(&self, id:Id) -> Option<Node> {
        self.data.borrow().get(&id).and_then(|t| t.upgrade())
    }
}


#[derive(Clone,CloneRef,Debug,Default,Shrinkwrap)]
pub struct WeakNodeSelectionSet {
    data : WeakNodeSet
}

impl WeakNodeSelectionSet {
    pub fn clear(&self) {
        self.for_each_taken(|node| node.events.deselect.emit(()));
    }
}

// ==================
// === Connection ===
// ==================

/// Endpoint of connection. This struct is used for connection identification.
#[derive(Clone,Debug)]
pub struct Endpoint {
    //Note[ao] actually I would be happy if I could put here something like "node id" instead of
    //handle. The handle is problematic when it comes to comparing and storing structures in
    //HashSet.
    pub node : WeakNode,
    pub port : Vec<span_tree::node::Crumb>,
}

/// Some connection identification
#[derive(Clone,Debug)]
pub struct Connection {
    pub source      : Endpoint,
    pub destination : Endpoint,
}


#[derive(Debug,Clone,CloneRef)]
pub struct GraphEditorFrp {
    pub network : frp::Network,
    pub inputs  : FrpInputs,
    pub status  : FrpStatus,

    pub node_release                   : frp::Stream<Option<WeakNode>>,
    //Note[ao] Here "by_command" means: by keyboard action or mouse event.
    pub connections_removed_by_command : frp::Stream<Vec<Connection>>,
    pub connections_added_by_command   : frp::Stream<Vec<Connection>>,
    //Note[ao] in theory I can just attach to the events in `command` structure, but I need to get
    // the list of removed nodes, and I'm not quaranteed, that the nodes will still exist
    // when I receive my event (perhaps the actual removing will be processed first).
    //
    // Now it is "hacky" because I directly bind to the Js keyboard input events (I wrote it
    // before the Keyboard management get the current shape).
    pub nodes_removed_by_command       : frp::Stream<Vec<WeakNode>>,
}

impl Deref for GraphEditorFrp {
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
    /// Add a new node and place it at the mouse cursor position.
    add_node_at_cursor,
    /// Remove all selected nodes from the graph.
    remove_selected_nodes,
    /// Remove all nodes from the graph.
    remove_all_nodes,
}


impl Commands {
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            def add_node_at_cursor    = source();
            def remove_selected_nodes = source();
            def remove_all_nodes      = source();
        }
        Self {add_node_at_cursor,remove_selected_nodes,remove_all_nodes}
    }
}

#[derive(Debug,Clone,CloneRef,Shrinkwrap)]
pub struct FrpInputs {
    #[shrinkwrap(main_field)]
    commands                     : Commands,
    register_node                : frp::Source<Node>,
    // Node[ao]: Remember, that NodeEditor currently does not use this endpoint, because it needs to
    // get the node reference after adding (it keeps mapping between node ids from double rep to the
    // WeakNode.
    pub add_node_at              : frp::Source<Position>,
    pub select_node              : frp::Source<Option<WeakNode>>,
    pub translate_selected_nodes : frp::Source<Position>,

    // Node[ao]: options are here, because FRP requires Default.
    pub add_connection           : frp::Source<Option<Connection>>,
    pub remove_connection        : frp::Source<Option<Connection>>,
    // Note[ao]: as a reminder: pattern is a left side of assignment, expression here is the right
    // side (or the whole line if there is no assignment). You can pick better names if you want.
    pub set_expression_span_tree : frp::Source<Option<(WeakNode, span_tree::SpanTree)>>,
    // Note[ao]: Here I can send `None` sometimes, because node can lose its pattern.
    pub set_pattern_span_tree    : frp::Source<Option<(WeakNode, span_tree::SpanTree)>>,
}

impl FrpInputs {
    pub fn new(network:&frp::Network) -> Self {
        let commands = Commands::new(network);
        frp::extend! { network
            def register_node            = source();
            def add_node_at              = source();
            def select_node              = source();
            def translate_selected_nodes = source();
            def add_connection           = source();
            def remove_connection        = source();
            def set_expression_span_tree = source();
            def set_pattern_span_tree    = source();
        }
        Self {commands,register_node,add_node_at,select_node,translate_selected_nodes,
            add_connection,remove_connection,set_expression_span_tree,set_pattern_span_tree}
    }

    fn register_node<T: AsRef<Node>>(&self, arg: T) {
        self.register_node.emit(arg.as_ref());
    }
    pub fn add_node_at<T: AsRef<Position>>(&self, arg: T) {
        self.add_node_at.emit(arg.as_ref());
    }
    pub fn add_node_at_cursor(&self) {
        self.add_node_at_cursor.emit(());
    }
    pub fn select_node<T: AsRef<Option<WeakNode>>>(&self, arg: T) {
        self.select_node.emit(arg.as_ref());
    }
    pub fn translate_selected_nodes<T: AsRef<Position>>(&self, arg: T) {
        self.translate_selected_nodes.emit(arg.as_ref());
    }
    pub fn remove_selected_nodes(&self) {
        self.remove_selected_nodes.emit(());
    }
    pub fn remove_all_nodes(&self) {
        self.remove_all_nodes.emit(());
    }
}

impl application::command::FrpNetworkProvider for GraphEditor {
    fn network(&self) -> &frp::Network {
        &self.frp.network
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




#[derive(Debug,Clone,CloneRef,Default)]
pub struct NodeState {
    pub set      : NodeSet,
    pub selected : WeakNodeSelectionSet,
}

#[derive(Debug,Clone,CloneRef)]
pub struct GraphEditor {
    pub logger         : Logger,
    pub display_object : display::object::Instance,
    pub nodes          : NodeState,
    pub frp            : GraphEditorFrp,
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
    pub nodes : TouchNetwork::<Option<WeakNode>>,
    pub bg    : TouchNetwork::<()>,
}

impl TouchState {
    pub fn new(network:&frp::Network, mouse:&frp::io::Mouse) -> Self {
        let nodes      = TouchNetwork::<Option<WeakNode>>::new(&network,mouse);
        let bg = TouchNetwork::<()>::new(&network,mouse);
        Self {nodes,bg}
    }
}


impl GraphEditor {

    pub fn add_node(&self) -> WeakNode {
        let node = Node::new();
        self.frp.inputs.register_node(&node);
        node.downgrade()
    }

    pub fn remove_node(&self, node:WeakNode) {
        if let Some(node) = node.upgrade() {
            self.nodes.set.remove(&node);
        }
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
        ]
    }
}

impl application::View for GraphEditor {

    fn new(world:&World) -> Self {
        let logger = Logger::new("GraphEditor");
        let scene  = world.scene();
        let cursor = Cursor::new();
        web::body().set_style_or_panic("cursor","none");
        world.add_child(&cursor);


        let display_object = display::object::Instance::new(logger.clone());
        let mouse          = &scene.mouse.frp;
        let network        = frp::Network::new();
        let inputs         = FrpInputs::new(&network);
        let nodes          = NodeState::default();
        let touch          = TouchState::new(&network,mouse);


        frp::extend! { network

        // === Cursor ===

        def mouse_on_down_position = mouse.position.sample(&mouse.press);
        def selection_zero         = source::<Position>();
        def selection_size_down    = mouse.position.map2(&mouse_on_down_position,|m,n|{m-n});
        def selection_size_if_down = selection_size_down.gate(&touch.bg.is_down);
        def selection_size_on_down = selection_zero.sample(&mouse.press);
        def selection_size         = selection_size_if_down.merge(&selection_size_on_down);

        def _cursor_size = selection_size.map(f!((cursor)(p) {
            cursor.set_selection_size(Vector2::new(p.x,p.y));
        }));

        def _cursor_press = mouse.press.map(f!((cursor)(_) {
            cursor.events.press.emit(());
        }));

        def _cursor_release = mouse.release.map(f!((cursor)(_) {
            cursor.events.release.emit(());
        }));

        def _cursor_position = mouse.position.map(f!((cursor)(p) {
            cursor.set_position(Vector2::new(p.x,p.y));
        }));


        // === Generic Selection ===

        def mouse_down_target  = mouse.press.map(f_!((scene) scene.mouse.target.get()));
        def _mouse_down_target = mouse_down_target.map(f!((touch,scene)(target) {
            match target {
                display::scene::Target::Background => {
                    touch.bg.down.emit(());
                }
                display::scene::Target::Symbol {symbol_id,instance_id} => {
                    scene.shapes.get_mouse_target(*symbol_id as i32, *instance_id as usize).for_each(|target| {
                        target.mouse_down().for_each(|t| t.emit(()));
                    })
                }
            }
        }));


        // === Selection ===

        def _deselect_all_on_bg_press = touch.bg.selected.map(f_!((nodes) nodes.selected.clear()));
        def select_unified            = inputs.select_node.merge(&touch.nodes.selected);
        def _select_pressed           = select_unified.map(f!((nodes)(opt_node) {
            opt_node.for_each_ref(|weak_node| {
                weak_node.upgrade().map(|node| {
                    nodes.selected.clear();
                    node.events.select.emit(());
                    nodes.selected.insert(&node);
                })
            })
        }));


        // === Add Node ===

        def add_node_at_cursor_pos = inputs.add_node_at_cursor.map2(&mouse.position,|_,p|{*p});
        def add_node               = inputs.add_node_at.merge(&add_node_at_cursor_pos);
        def _add_new_node          = add_node.map(f!((inputs)(pos) {
            let node = Node::new();
            inputs.register_node(&node);
            node.mod_position(|t| {
                t.x += pos.x;
                t.y += pos.y;
            });
        }));

        def _new_node = inputs.register_node.map(f!((network,nodes,touch,display_object)(node) {
            let weak_node = node.downgrade();
            frp::new_bridge_network! { [network,node.view.events.network]
                def _node_on_down_tagged = node.view.events.mouse_down.map(f_!((touch) {
                    touch.nodes.down.emit(Some(weak_node.clone_ref()))
                }));
            }
            display_object.add_child(node);
            nodes.set.insert(node.clone_ref());
        }));


        // === Remove Node ===

        def _remove_all      = inputs.remove_all_nodes.map(f!((nodes)(()) nodes.set.clear()));
        def _remove_selected = inputs.remove_selected_nodes.map(f!((nodes,nodes)(_) {
            nodes.selected.for_each_taken(|node| nodes.set.remove(&node))
        }));


        // === Move Nodes ===

        def mouse_tx_if_node_pressed = mouse.translation.gate(&touch.nodes.is_down);
        def _move_node_with_mouse    = mouse_tx_if_node_pressed.map2(&touch.nodes.down,|tx,node| {
            node.mod_position(|p| { p.x += tx.x; p.y += tx.y; })
        });

        def _move_selected_nodes = inputs.translate_selected_nodes.map(f!((nodes)(t) {
            nodes.selected.for_each(|node| {
                node.mod_position(|p| {
                    p.x += t.x;
                    p.y += t.y;
                })
            })
        }));


        // === Status ===

        def is_active_src = source::<bool>();
        def is_empty_src  = source::<bool>();
        def is_active = is_active_src.sampler();
        def is_empty  = is_empty_src.sampler();

        def connections_removed_by_command = source::<Vec<Connection>>().into();
        def connections_added_by_command   = source::<Vec<Connection>>().into();
        def nodes_removed_by_command       = source::<Vec<WeakNode>>().into();
        }

        // FIXME This is a temporary solution. Should be replaced by a real thing once layout
        //       management is implemented.
        is_active_src.emit(true);

        let status = FrpStatus {is_active,is_empty};

        let node_release = touch.nodes.up;
        let frp = GraphEditorFrp {network,inputs,status,node_release,connections_removed_by_command,
            connections_added_by_command,nodes_removed_by_command};

        Self {logger,frp,nodes,display_object}
    }


}

impl display::Object for GraphEditor {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
