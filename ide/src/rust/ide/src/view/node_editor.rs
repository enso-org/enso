#![allow(missing_docs)] // FIXME

use crate::prelude::*;

use ensogl::display::traits::*;
use ensogl::display;
use ensogl::display::world::World;
use enso_frp::*;

use graph_editor::GraphEditor;
use crate::notification;
use utils::channel::process_stream_with_handle;



// ====================
// === Graph Editor ===
// ====================

#[derive(Clone,Debug)]
pub struct NodeEditor {
    display_object : display::object::Node,
    graph          : Rc<GraphEditor>,
    controller     : controller::graph::Handle,
    logger         : Logger,
}

impl NodeEditor {
    pub fn new(logger:&Logger, world:&World, controller:controller::graph::Handle) -> Self {
        let logger         = logger.sub("GraphEditor");
        let display_object = display::object::Node::new(&logger);
        let graph          = Rc::new(graph_editor::GraphEditor::new(world));
        display_object.add_child(graph.deref());
        let editor         = NodeEditor {display_object,graph,controller,logger};
        editor.initialize()
    }

    fn initialize(self) -> Self {
        Self::update_graph(self.graph.deref(),&self.controller);
        self.setup_controller_notifications();
        self
    }

    fn setup_controller_notifications(&self) {
        let subscribe  = self.controller.subscribe();
        let weak_graph = Rc::downgrade(&self.graph);
        let controller = self.controller.clone();
        executor::global::spawn(process_stream_with_handle(subscribe,weak_graph, move |notification,graph| {
            match notification {
                notification::Graph::Invalidate => {
                    Self::update_graph(graph.deref(),&controller)
                }
            }
            futures::future::ready(())
        }));
    }

    fn update_graph(graph:&GraphEditor, controller:&controller::Graph) {
        graph.events.clear_graph.event.emit(());
        if let Ok(nodes_info) = controller.nodes() {
            let nodes_with_index = nodes_info.iter().enumerate();
            let nodes_positions  = nodes_with_index.map(|(i,n)| n.metadata.and_then(|m| m.position).map(|p| enso_frp::Position::new(p.vector.x as i32, p.vector.y as i32)).unwrap_or_else(|| enso_frp::Position::new(i as i32 * 100,0)));
            for pos in nodes_positions { graph.events.add_node_at.event.emit(pos) }
        }
    }
}

impl<'t> From<&'t NodeEditor> for &'t display::object::Node {
    fn from(graph_editor:&'t NodeEditor) -> Self {
        &graph_editor.display_object
    }
}
