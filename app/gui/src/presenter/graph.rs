//! The module with the [`Graph`] presenter. See [`crate::presenter`] documentation to know more
//! about presenters in general.

mod state;

use crate::prelude::*;

use crate::executor::global::spawn_stream_handler;

use enso_frp as frp;
use ide_view as view;
use ide_view::graph_editor::component::node as node_view;
use ide_view::graph_editor::EdgeEndpoint;



// ===============
// === Aliases ===
// ===============

type ViewNodeId = view::graph_editor::NodeId;
type AstNodeId = ast::Id;
type ViewConnection = view::graph_editor::EdgeId;
type AstConnection = controller::graph::Connection;



// =============
// === Model ===
// =============

#[derive(Clone, Debug)]
struct Model {
    logger:     Logger,
    controller: controller::ExecutedGraph,
    view:       view::graph_editor::GraphEditor,
    state:      Rc<state::State>,
}

impl Model {
    pub fn new(
        controller: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
    ) -> Self {
        let logger = Logger::new("presenter::Graph");
        let state = default();
        Self { logger, controller, view, state }
    }

    /// Node position was changed in view.
    fn node_position_changed(&self, id: ViewNodeId, position: Vector2) {
        self.update_ast(
            || {
                let ast_id = self.state.update_from_view().set_node_position(id, position)?;
                Some(self.controller.graph().set_node_position(ast_id, position))
            },
            "update node position",
        );
    }

    /// Node was removed in view.
    fn node_removed(&self, id: ViewNodeId) {
        self.update_ast(
            || {
                let ast_id = self.state.update_from_view().remove_node(id)?;
                Some(self.controller.graph().remove_node(ast_id))
            },
            "remove node",
        )
    }

    /// Connection was created in view.
    fn new_connection_created(&self, id: ViewConnection) {
        self.update_ast(
            || {
                let connection = self.view.model.edges.get_cloned_ref(&id)?;
                let ast_to_create = self.state.update_from_view().create_connection(connection)?;
                Some(self.controller.connect(&ast_to_create))
            },
            "create connection",
        );
    }

    /// Connection was removed in view.
    fn connection_removed(&self, id: ViewConnection) {
        self.update_ast(
            || {
                let ast_to_remove = self.state.update_from_view().remove_connection(id)?;
                Some(self.controller.disconnect(&ast_to_remove))
            },
            "delete connection",
        );
    }

    fn update_ast<F>(&self, f: F, action: &str)
    where F: FnOnce() -> Option<FallibleResult> {
        if let Some(Err(err)) = f() {
            error!(self.logger, "Failed to {action} in AST: {err}");
        }
    }

    /// Extract all types for subexpressions in node expressions, update the state,
    /// and return the events for graph editor FRP input setting all of those types.
    ///
    /// The result includes the types not changed according to the state. That's because this
    /// function is used after node expression change, and we need to reset all the types in view.
    fn all_types_of_node(
        &self,
        node: ViewNodeId,
    ) -> Vec<(ViewNodeId, ast::Id, Option<view::graph_editor::Type>)> {
        let subexpressions = self.state.expressions_of_node(node);
        subexpressions
            .iter()
            .map(|id| {
                let a_type = self.expression_type(*id);
                self.state.update_from_controller().set_expression_type(*id, a_type.clone());
                (node, *id, a_type)
            })
            .collect()
    }

    /// Extract all method pointers for subexpressions, update the state, and return events updating
    /// view for expressions where method pointer actually changed.
    fn all_method_pointers_of_node(
        &self,
        node: ViewNodeId,
    ) -> Vec<(ast::Id, Option<view::graph_editor::MethodPointer>)> {
        let subexpressions = self.state.expressions_of_node(node);
        subexpressions.iter().filter_map(|id| self.refresh_expression_method_pointer(*id)).collect()
    }

    /// Refresh type of the given expression.
    ///
    /// If the view update is required, the GraphEditor's FRP input event is returned.
    fn refresh_expression_type(
        &self,
        id: ast::Id,
    ) -> Option<(ViewNodeId, ast::Id, Option<view::graph_editor::Type>)> {
        let a_type = self.expression_type(id);
        let node_view =
            self.state.update_from_controller().set_expression_type(id, a_type.clone())?;
        Some((node_view, id, a_type))
    }

    /// Refresh method pointer of the given expression.
    ///
    /// If the view update is required, the GraphEditor's FRP input event is returned.
    fn refresh_expression_method_pointer(
        &self,
        id: ast::Id,
    ) -> Option<(ast::Id, Option<view::graph_editor::MethodPointer>)> {
        let method_pointer = self.expression_method(id);
        self.state
            .update_from_controller()
            .set_expression_method_pointer(id, method_pointer.clone())?;
        Some((id, method_pointer))
    }

    /// Extract the expression's current type from controllers.
    fn expression_type(&self, id: ast::Id) -> Option<view::graph_editor::Type> {
        let registry = self.controller.computed_value_info_registry();
        let info = registry.get(&id)?;
        Some(view::graph_editor::Type(info.typename.as_ref()?.clone_ref()))
    }

    /// Extract the expression's current method pointer from controllers.
    fn expression_method(&self, id: ast::Id) -> Option<view::graph_editor::MethodPointer> {
        let registry = self.controller.computed_value_info_registry();
        let method_id = registry.get(&id)?.method_call?;
        let suggestion_db = self.controller.graph().suggestion_db.clone_ref();
        let method = suggestion_db.lookup_method_ptr(method_id).ok()?;
        Some(view::graph_editor::MethodPointer(Rc::new(method)))
    }
}



// ==================
// === ViewUpdate ===
// ==================

/// Structure handling view update after graph invalidation.
///
/// Because updating various graph elements (nodes, connections, types) bases on the same data
/// extracted from controllers, the data are cached in this structure.
#[derive(Clone, Debug, Default)]
struct ViewUpdate {
    state:       Rc<state::State>,
    nodes:       Vec<controller::graph::Node>,
    trees:       HashMap<AstNodeId, controller::graph::NodeTrees>,
    connections: HashSet<AstConnection>,
}

impl ViewUpdate {
    /// Create ViewUpdate information from Graph Presenter's model.
    fn new(model: &Model) -> FallibleResult<Self> {
        let displayed = model.state.clone_ref();
        let nodes = model.controller.graph().nodes()?;
        let connections_and_trees = model.controller.connections()?;
        let connections = connections_and_trees.connections.into_iter().collect();
        let trees = connections_and_trees.trees;
        Ok(Self { state: displayed, nodes, trees, connections })
    }

    /// Remove nodes from the state and return node views to be removed.
    fn remove_nodes(&self) -> Vec<ViewNodeId> {
        self.state.update_from_controller().retain_nodes(&self.node_ids().collect())
    }

    /// Returns number of nodes view should create.
    fn count_nodes_to_add(&self) -> usize {
        self.node_ids().filter(|n| self.state.view_id_of_ast_node(*n).is_none()).count()
    }

    /// Set the nodes expressions in state, and return the events to be passed to Graph Editor FRP
    /// input for nodes where expression changed.
    ///
    /// The nodes not having views are also updated in the state.
    fn set_node_expressions(&self) -> Vec<(ViewNodeId, node_view::Expression)> {
        self.nodes
            .iter()
            .filter_map(|node| {
                let id = node.main_line.id();
                let trees = self.trees.get(&id).cloned().unwrap_or_default();
                self.state.update_from_controller().set_node_expression(node, trees)
            })
            .collect()
    }

    /// Set the nodes position in state, and return the events to be passed to GraphEditor FRP
    /// input for nodes where position changed.
    ///
    /// The nodes not having views are also updated in the state.
    fn set_node_positions(&self) -> Vec<(ViewNodeId, Vector2)> {
        self.nodes
            .iter()
            .filter_map(|node| {
                let id = node.main_line.id();
                let position = node.position()?.vector;
                let view_id =
                    self.state.update_from_controller().set_node_position(id, position)?;
                Some((view_id, position))
            })
            .collect()
    }

    /// Remove connections from the state and return views to be removed.
    fn remove_connections(&self) -> Vec<ViewConnection> {
        self.state.update_from_controller().retain_connections(&self.connections)
    }

    /// Add connections to the state and return endpoints of connections to be created in views.
    fn add_connections(&self) -> Vec<(EdgeEndpoint, EdgeEndpoint)> {
        let ast_conns = self.connections.iter();
        ast_conns
            .filter_map(|connection| {
                self.state.update_from_controller().set_connection(connection.clone())
            })
            .collect()
    }

    fn node_ids(&self) -> impl Iterator<Item = AstNodeId> + '_ {
        self.nodes.iter().map(controller::graph::Node::id)
    }
}



// =============
// === Graph ===
// =============

/// The Graph Presenter, synchronizing graph state between graph controller and view.
///
/// This presenter focuses on the graph structure: nodes, their expressions and types, and
/// connections between them. It does not integrate Searcher nor Breadcrumbs - integration of
/// these is still to-be-delivered.
#[derive(Debug)]
pub struct Graph {
    network: frp::Network,
    model:   Rc<Model>,
}

impl Graph {
    /// Create graph presenter. The returned structure is working and does not require any
    /// initialization.
    pub fn new(
        controller: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
    ) -> Self {
        let network = frp::Network::new("presenter::Graph");
        let model = Rc::new(Model::new(controller, view));
        Self { network, model }.init()
    }

    fn init(self) -> Self {
        let logger = &self.model.logger;
        let network = &self.network;
        let model = &self.model;
        let view = &self.model.view.frp;
        frp::extend! { network
            update_view <- source::<()>();
            update_data <- update_view.map(
                f_!([logger,model] match ViewUpdate::new(&*model) {
                    Ok(update) => Rc::new(update),
                    Err(err) => {
                        error!(logger,"Failed to update view: {err:?}");
                        Rc::new(default())
                    }
                })
            );


            // === Refreshing Nodes ===

            remove_node <= update_data.map(|update| update.remove_nodes());
            update_node_expression <= update_data.map(|update| update.set_node_expressions());
            set_node_position <= update_data.map(|update| update.set_node_positions());
            view.remove_node <+ remove_node;
            view.set_node_expression <+ update_node_expression;
            view.set_node_position <+ set_node_position;

            view.add_node <+ update_data.map(|update| update.count_nodes_to_add()).repeat();
            added_node_update <- view.node_added.filter_map(f!((view_id)
                model.state.assign_node_view(*view_id)
            ));
            init_node_expression <- added_node_update.filter_map(|update| Some((update.view_id?, update.expression.clone())));
            view.set_node_expression <+ init_node_expression;
            view.set_node_position <+ added_node_update.filter_map(|update| Some((update.view_id?, update.position)));


            // === Refreshing Connections ===

            remove_connection <= update_data.map(|update| update.remove_connections());
            add_connection <= update_data.map(|update| update.add_connections());
            view.remove_edge <+ remove_connection;
            view.connect_nodes <+ add_connection;


            // === Refreshing Expressions ===

            reset_node_types <- any(update_node_expression, init_node_expression)._0();
            set_expression_type <= reset_node_types.map(f!((view_id) model.all_types_of_node(*view_id)));
            set_method_pointer <= reset_node_types.map(f!((view_id) model.all_method_pointers_of_node(*view_id)));
            view.set_expression_usage_type <+ set_expression_type;
            view.set_method_pointer <+ set_method_pointer;

            update_expressions <- source::<Vec<ast::Id>>();
            update_expression <= update_expressions;
            view.set_expression_usage_type <+ update_expression.filter_map(f!((id) model.refresh_expression_type(*id)));
            view.set_method_pointer <+ update_expression.filter_map(f!((id) model.refresh_expression_method_pointer(*id)));


            // === Changes from the View ===

            eval view.node_position_set_batched(((node_id, position)) model.node_position_changed(*node_id, *position));
            eval view.node_removed((node_id) model.node_removed(*node_id));
            eval view.on_edge_endpoints_set((edge_id) model.new_connection_created(*edge_id));
            eval view.on_edge_endpoint_unset(((edge_id,_)) model.connection_removed(*edge_id));
        }

        update_view.emit(());
        self.setup_controller_notification_handlers(update_view, update_expressions);

        self
    }

    fn setup_controller_notification_handlers(
        &self,
        update_view: frp::Source<()>,
        update_expressions: frp::Source<Vec<ast::Id>>,
    ) {
        use crate::controller::graph::executed;
        use crate::controller::graph::Notification;
        let graph_notifications = self.model.controller.subscribe();
        self.spawn_sync_stream_handler(graph_notifications, move |notification, model| {
            info!(model.logger, "Received controller notification {notification:?}");
            match notification {
                executed::Notification::Graph(graph) => match graph {
                    Notification::Invalidate => update_view.emit(()),
                    Notification::PortsUpdate => update_view.emit(()),
                },
                executed::Notification::ComputedValueInfo(expressions) =>
                    update_expressions.emit(expressions),
                executed::Notification::EnteredNode(_) => {}
                executed::Notification::SteppedOutOfNode(_) => {}
            }
        })
    }

    fn spawn_sync_stream_handler<Stream, Function>(&self, stream: Stream, handler: Function)
    where
        Stream: StreamExt + Unpin + 'static,
        Function: Fn(Stream::Item, Rc<Model>) + 'static, {
        let model = Rc::downgrade(&self.model);
        spawn_stream_handler(model, stream, move |item, model| {
            handler(item, model);
            futures::future::ready(())
        })
    }
}
