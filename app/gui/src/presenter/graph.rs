use crate::prelude::*;

use crate::executor::global::spawn_stream_handler;

use bimap::BiMap;
use bimap::Overwritten;
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



// ======================
// === DisplayedState ===
// ======================

// === DisplayedNodes ===

#[derive(Clone, Debug, Default)]
struct DisplayedNode {
    view_id:    Option<ViewNodeId>,
    position:   Vector2,
    expression: node_view::Expression,
}

#[derive(Clone, Debug, Default)]
struct DisplayedNodes {
    displayed_nodes:     HashMap<AstNodeId, DisplayedNode>,
    nodes_without_view:  Vec<AstNodeId>,
    ast_node_by_view_id: HashMap<ViewNodeId, AstNodeId>,
}

impl DisplayedNodes {
    fn get(&self, id: AstNodeId) -> Option<&DisplayedNode> {
        self.displayed_nodes.get(&id)
    }

    fn get_mut(&mut self, id: AstNodeId) -> Option<&mut DisplayedNode> {
        self.displayed_nodes.get_mut(&id)
    }

    fn get_mut_or_create(&mut self, id: AstNodeId) -> &mut DisplayedNode {
        let nodes_without_view = &mut self.nodes_without_view;
        self.displayed_nodes.entry(id).or_insert_with(|| {
            nodes_without_view.push(id);
            default()
        })
    }

    fn ast_id_of_view(&self, view_id: ViewNodeId) -> Option<AstNodeId> {
        self.ast_node_by_view_id.get(&view_id).copied()
    }

    fn assign_newly_created_node(&mut self, view_id: ViewNodeId) -> Option<&mut DisplayedNode> {
        let ast_node = self.nodes_without_view.pop()?;
        let mut opt_displayed = self.displayed_nodes.get_mut(&ast_node);
        if let Some(displayed) = &mut opt_displayed {
            displayed.view_id = Some(view_id);
            self.ast_node_by_view_id.insert(view_id, ast_node);
        }
        opt_displayed
    }

    fn retain_nodes(&mut self, nodes: &HashSet<AstNodeId>) -> Vec<ViewNodeId> {
        self.nodes_without_view.drain_filter(|id| !nodes.contains(id));
        let removed = self.displayed_nodes.drain_filter(|id, _| !nodes.contains(id));
        let removed_views = removed.filter_map(|(_, data)| data.view_id).collect();
        for view_id in &removed_views {
            self.ast_node_by_view_id.remove(view_id);
        }
        removed_views
    }

    fn remove_node(&mut self, node: ViewNodeId) -> Option<AstNodeId> {
        let ast_id = self.ast_node_by_view_id.remove(&node)?;
        self.displayed_nodes.remove(&ast_id);
        Some(ast_id)
    }
}


// === Displayed Connections ===

#[derive(Clone, Debug, Default)]
struct DisplayedConnections {
    connections:              BiMap<AstConnection, ViewConnection>,
    connections_without_view: HashSet<AstConnection>,
}

impl DisplayedConnections {
    fn view_of_ast_connection(&self, connection: &AstConnection) -> Option<ViewConnection> {
        self.connections.get_by_left(connection).copied()
    }

    fn retain_connections(&mut self, connections: &HashSet<AstConnection>) -> Vec<ViewConnection> {
        self.connections_without_view.retain(|x| connections.contains(x));
        let to_remove = self.connections.iter().filter(|(con, _)| !connections.contains(con));
        let to_remove_vec = to_remove.map(|(_, edge_id)| *edge_id).collect_vec();
        self.connections.retain(|con, _| connections.contains(con));
        to_remove_vec
    }

    /// Returns true if the controller needs refreshing.
    fn assign_connection_view(&mut self, connection: AstConnection, view: ViewConnection) -> bool {
        let exited_without_view = self.connections_without_view.remove(&connection);
        match self.connections.insert(connection, view) {
            Overwritten::Neither => !exited_without_view,
            Overwritten::Left(_, _) => false,
            Overwritten::Right(previous, _) => {
                self.connections_without_view.insert(previous);
                !exited_without_view
            }
            Overwritten::Pair(_, _) => false,
            Overwritten::Both(_, (previous, _)) => {
                self.connections_without_view.insert(previous);
                false
            }
        }
    }

    fn remove_connection(&mut self, connection: ViewConnection) -> Option<AstConnection> {
        let (ast_connection, _) = self.connections.remove_by_right(&connection)?;
        Some(ast_connection)
    }
}


// === DisplayedExpressionss ===

#[derive(Clone, Debug, Default)]
struct DisplayedExpression {
    node:            AstNodeId,
    expression_type: Option<view::graph_editor::Type>,
    method_pointer:  Option<view::graph_editor::MethodPointer>,
}

#[derive(Clone, Debug, Default)]
struct DisplayedExpressions {
    expressions:         HashMap<ast::Id, DisplayedExpression>,
    expressions_of_node: HashMap<AstNodeId, Vec<ast::Id>>,
}

impl DisplayedExpressions {
    fn retain_expression_of_nodes(&mut self, nodes: &HashSet<AstNodeId>) {
        let nodes_to_remove =
            self.expressions_of_node.drain_filter(|node_id, _| !nodes.contains(node_id));
        let expr_to_remove = nodes_to_remove.map(|(_, exprs)| exprs).flatten();
        for expression_id in expr_to_remove {
            self.expressions.remove(&expression_id);
        }
    }

    fn node_expression_changed(&mut self, node: AstNodeId, expressions: Vec<ast::Id>) {
        let new_set: HashSet<ast::Id> = expressions.iter().copied().collect();
        let old_set = self.expressions_of_node.insert(node, expressions).unwrap_or_default();
        for old_expression in old_set {
            if !new_set.contains(&old_expression) {
                self.expressions.remove(&old_expression);
            }
        }
    }

    fn get_mut(&mut self, id: ast::Id) -> Option<&mut DisplayedExpression> {
        self.expressions.get_mut(&id)
    }

    fn subexpressions_of_node(&self, id: ast::Id) -> &[ast::Id] {
        self.expressions_of_node.get(&id).map_or(&[], |v| v.as_slice())
    }
}


// === DisplayedState ===

#[derive(Clone, Debug, Default)]
struct DisplayedState {
    nodes:       RefCell<DisplayedNodes>,
    connections: RefCell<DisplayedConnections>,
    expressions: RefCell<DisplayedExpressions>,
}

impl DisplayedState {
    fn assign_newly_created_node(&self, view_id: ViewNodeId) -> Option<DisplayedNode> {
        self.nodes.borrow_mut().assign_newly_created_node(view_id).cloned()
    }

    fn assign_connection_view(
        &self,
        connection: view::graph_editor::Edge,
    ) -> Option<AstConnection> {
        let view_source = connection.source()?;
        let view_target = connection.target()?;
        let ast_connection =
            self.ast_connection_from_view_edge_targets(view_source, view_target)?;
        let mut connections = self.connections.borrow_mut();
        let should_update_controllers =
            connections.assign_connection_view(ast_connection.clone(), connection.id());
        should_update_controllers.then_some(ast_connection)
    }

    fn view_id_of_ast_node(&self, node: AstNodeId) -> Option<ViewNodeId> {
        self.nodes.borrow().get(node).and_then(|n| n.view_id)
    }

    fn ast_id_of_view_node(&self, node: ViewNodeId) -> Option<AstNodeId> {
        self.nodes.borrow().ast_id_of_view(node)
    }

    fn view_of_ast_connection(&self, connection: &AstConnection) -> Option<ViewConnection> {
        self.connections.borrow().view_of_ast_connection(connection)
    }

    fn view_edge_targets_of_ast_connection(
        &self,
        connection: AstConnection,
    ) -> Option<(EdgeEndpoint, EdgeEndpoint)> {
        let nodes = self.nodes.borrow();
        let src_node = nodes.get(connection.source.node)?.view_id?;
        let dst_node = nodes.get(connection.destination.node)?.view_id?;
        let src = EdgeEndpoint::new(src_node, connection.source.port);
        let data = EdgeEndpoint::new(dst_node, connection.destination.port);
        Some((src, data))
    }

    fn ast_connection_from_view_edge_targets(
        &self,
        source: EdgeEndpoint,
        target: EdgeEndpoint,
    ) -> Option<controller::graph::Connection> {
        let nodes = self.nodes.borrow();
        let src_node = nodes.ast_id_of_view(source.node_id)?;
        let dst_node = nodes.ast_id_of_view(target.node_id)?;
        Some(controller::graph::Connection {
            source:      controller::graph::Endpoint::new(src_node, source.port),
            destination: controller::graph::Endpoint::new(dst_node, target.port),
        })
    }

    fn subexpressions_of_node(&self, node: ViewNodeId) -> Vec<ast::Id> {
        let ast_node = self.nodes.borrow().ast_id_of_view(node);
        ast_node
            .map_or_default(|id| self.expressions.borrow().subexpressions_of_node(id).to_owned())
    }

    fn retain_nodes(&self, nodes: &HashSet<AstNodeId>) -> Vec<ViewNodeId> {
        self.expressions.borrow_mut().retain_expression_of_nodes(nodes);
        self.nodes.borrow_mut().retain_nodes(nodes)
    }

    fn retain_connections(&self, connections: &HashSet<AstConnection>) -> Vec<ViewConnection> {
        self.connections.borrow_mut().retain_connections(connections)
    }

    fn refresh_node_position(&self, node: AstNodeId, position: Vector2) -> Option<ViewNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let mut displayed = nodes.get_mut_or_create(node);
        if displayed.position != position {
            displayed.position = position;
            displayed.view_id
        } else {
            None
        }
    }

    fn refresh_node_expression(
        &self,
        node: &controller::graph::Node,
        trees: controller::graph::NodeTrees,
    ) -> Option<(ViewNodeId, node_view::Expression)> {
        let ast_id = node.main_line.id();
        let new_displayed_expr = node_view::Expression {
            pattern:             node.info.pattern().map(|t| t.repr()),
            code:                node.info.expression().repr(),
            whole_expression_id: node.info.expression().id,
            input_span_tree:     trees.inputs,
            output_span_tree:    trees.outputs.unwrap_or_else(default),
        };
        let mut nodes = self.nodes.borrow_mut();
        let displayed = nodes.get_mut_or_create(ast_id);
        if &displayed.expression != &new_displayed_expr {
            displayed.expression = new_displayed_expr.clone();
            let new_expressions =
                node.info.ast().iter_recursive().filter_map(|ast| ast.id).collect();
            self.expressions.borrow_mut().node_expression_changed(ast_id, new_expressions);
            Some((displayed.view_id?, new_displayed_expr))
        } else {
            None
        }
    }

    fn refresh_expression_type(
        &self,
        id: ast::Id,
        new_type: Option<view::graph_editor::Type>,
    ) -> Option<ViewNodeId> {
        let mut expressions = self.expressions.borrow_mut();
        let to_update = expressions.get_mut(id).filter(|d| d.expression_type != new_type);
        if let Some(displayed) = to_update {
            displayed.expression_type = new_type;
            self.nodes.borrow().get(displayed.node).and_then(|node| node.view_id)
        } else {
            None
        }
    }

    fn refresh_expression_method_pointer(
        &self,
        id: ast::Id,
        method_ptr: Option<view::graph_editor::MethodPointer>,
    ) -> Option<ViewNodeId> {
        let mut expressions = self.expressions.borrow_mut();
        let to_update = expressions.get_mut(id).filter(|d| d.method_pointer != method_ptr);
        if let Some(displayed) = to_update {
            displayed.method_pointer = method_ptr;
            self.nodes.borrow().get(displayed.node).and_then(|node| node.view_id)
        } else {
            None
        }
    }

    fn node_position_changed(&self, id: ViewNodeId, new_position: Vector2) -> Option<AstNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let ast_id = nodes.ast_id_of_view(id)?;
        let displayed = nodes.get_mut(ast_id)?;
        if displayed.position != new_position {
            displayed.position = new_position;
            Some(ast_id)
        } else {
            None
        }
    }

    fn connection_removed(&self, id: ViewConnection) -> Option<AstConnection> {
        self.connections.borrow_mut().remove_connection(id)
    }

    fn node_removed(&self, id: ViewNodeId) -> Option<AstNodeId> {
        self.nodes.borrow_mut().remove_node(id)
    }
}


#[derive(Clone, Debug)]
struct Model {
    logger:     Logger,
    controller: controller::ExecutedGraph,
    view:       view::graph_editor::GraphEditor,
    displayed:  Rc<DisplayedState>,
}

impl Model {
    pub fn new(
        controller: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
    ) -> Self {
        let logger = Logger::new("presenter::Graph");
        let displayed = default();
        Self { logger, controller, view, displayed }
    }

    fn node_position_changed(&self, id: ViewNodeId, position: Vector2) {
        self.update_ast(
            || {
                let ast_id = self.displayed.node_position_changed(id, position)?;
                Some(self.controller.graph().set_node_position(ast_id, position))
            },
            "update node position",
        );
    }

    fn node_removed(&self, id: ViewNodeId) {
        self.update_ast(
            || {
                let ast_id = self.displayed.node_removed(id)?;
                Some(self.controller.graph().remove_node(ast_id))
            },
            "remove node",
        )
    }

    fn new_connection_created(&self, id: ViewConnection) {
        self.update_ast(
            || {
                let connection = self.view.model.edges.get_cloned_ref(&id)?;
                let ast_to_create = self.displayed.assign_connection_view(connection)?;
                Some(self.controller.connect(&ast_to_create))
            },
            "create connection",
        );
    }

    fn connection_removed(&self, id: ViewConnection) {
        self.update_ast(
            || {
                let ast_to_remove = self.displayed.connection_removed(id)?;
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

    fn all_types_of_node(
        &self,
        node: ViewNodeId,
    ) -> Vec<(ViewNodeId, ast::Id, Option<view::graph_editor::Type>)> {
        let subexpressions = self.displayed.subexpressions_of_node(node);
        subexpressions
            .iter()
            .map(|id| {
                let a_type = self.expression_type(*id);
                self.displayed.refresh_expression_type(*id, a_type.clone());
                (node, *id, a_type)
            })
            .collect()
    }

    fn all_method_pointers_of_node(
        &self,
        node: ViewNodeId,
    ) -> Vec<(ast::Id, Option<view::graph_editor::MethodPointer>)> {
        let subexpressions = self.displayed.subexpressions_of_node(node);
        subexpressions.iter().filter_map(|id| self.refresh_expression_method_pointer(*id)).collect()
    }

    fn refresh_expression_type(
        &self,
        id: ast::Id,
    ) -> Option<(ViewNodeId, ast::Id, Option<view::graph_editor::Type>)> {
        let a_type = self.expression_type(id);
        let node_view = self.displayed.refresh_expression_type(id, a_type.clone())?;
        Some((node_view, id, a_type))
    }

    fn refresh_expression_method_pointer(
        &self,
        id: ast::Id,
    ) -> Option<(ast::Id, Option<view::graph_editor::MethodPointer>)> {
        let method_pointer = self.expression_method(id);
        self.displayed.refresh_expression_method_pointer(id, method_pointer.clone())?;
        Some((id, method_pointer))
    }

    fn expression_type(&self, id: ast::Id) -> Option<view::graph_editor::Type> {
        let registry = self.controller.computed_value_info_registry();
        let info = registry.get(&id)?;
        Some(view::graph_editor::Type(info.typename.as_ref()?.clone_ref()))
    }

    fn expression_method(&self, id: ast::Id) -> Option<view::graph_editor::MethodPointer> {
        let registry = self.controller.computed_value_info_registry();
        let method_id = registry.get(&id)?.method_call?;
        let suggestion_db = self.controller.graph().suggestion_db.clone_ref();
        let method = suggestion_db.lookup_method_ptr(method_id).ok()?;
        Some(view::graph_editor::MethodPointer(Rc::new(method)))
    }
}


#[derive(Clone, Debug, Default)]
struct ViewUpdate {
    displayed:   Rc<DisplayedState>,
    nodes:       Vec<controller::graph::Node>,
    trees:       HashMap<AstNodeId, controller::graph::NodeTrees>,
    connections: HashSet<AstConnection>,
}

impl ViewUpdate {
    fn new(model: &Model) -> FallibleResult<Self> {
        let displayed = model.displayed.clone_ref();
        let nodes = model.controller.graph().nodes()?;
        let connections_and_trees = model.controller.connections()?;
        let connections = connections_and_trees.connections.into_iter().collect();
        let trees = connections_and_trees.trees;
        Ok(Self { displayed, nodes, trees, connections })
    }

    fn nodes_to_remove(&self) -> Vec<ViewNodeId> {
        self.displayed.retain_nodes(&self.node_ids().collect())
    }

    fn nodes_to_add(&self) -> usize {
        self.node_ids().filter(|n| self.displayed.view_id_of_ast_node(*n).is_none()).count()
    }

    fn expressions_to_set(&self) -> Vec<(ViewNodeId, node_view::Expression)> {
        self.nodes
            .iter()
            .filter_map(|node| {
                let id = node.main_line.id();
                let trees = self.trees.get(&id).cloned().unwrap_or_default();
                self.displayed.refresh_node_expression(node, trees)
            })
            .collect()
    }

    fn positions_to_set(&self) -> Vec<(ViewNodeId, Vector2)> {
        self.nodes
            .iter()
            .filter_map(|node| {
                let id = node.main_line.id();
                let position = node.position()?.vector;
                let view_id = self.displayed.refresh_node_position(id, position)?;
                Some((view_id, position))
            })
            .collect()
    }

    fn connections_to_remove(&self) -> Vec<ViewConnection> {
        self.displayed.retain_connections(&self.connections)
    }

    fn connections_to_add(&self) -> Vec<(EdgeEndpoint, EdgeEndpoint)> {
        let ast_conns = self.connections.iter();
        let ast_conns_to_add = ast_conns
            .filter(|connection| self.displayed.view_of_ast_connection(connection).is_none());
        ast_conns_to_add
            .filter_map(|connection| {
                self.displayed.view_edge_targets_of_ast_connection(connection.clone())
            })
            .collect()
    }

    fn node_ids<'a>(&'a self) -> impl Iterator<Item = AstNodeId> + 'a {
        self.nodes.iter().map(controller::graph::Node::id)
    }
}

#[derive(Debug)]
pub struct Graph {
    network: frp::Network,
    model:   Rc<Model>,
}

impl Graph {
    pub fn new(
        controller: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
    ) -> Self {
        let network = frp::Network::new("presenter::Graph");
        let model = Rc::new(Model::new(controller, view));
        Self { network, model }.init()
    }

    pub fn init(self) -> Self {
        let logger = &self.model.logger;
        let network = &self.network;
        let model = &self.model;
        let displayed = &model.displayed;
        let controller = &model.controller;
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

            remove_node <= update_data.map(|update| update.nodes_to_remove());
            update_node_expression <= update_data.map(|update| update.expressions_to_set());
            set_node_position <= update_data.map(|update| update.positions_to_set());
            remove_connection <= update_data.map(|update| update.connections_to_remove());
            add_connection <= update_data.map(|update| update.connections_to_add());
            view.remove_node <+ remove_node;
            view.set_node_expression <+ update_node_expression;
            view.set_node_position <+ set_node_position;
            view.remove_edge <+ remove_connection;
            view.connect_nodes <+ add_connection;

            view.add_node <+ update_data.map(|update| update.nodes_to_add()).repeat();
            added_node_update <- view.node_added.filter_map(f!((view_id)
                model.displayed.assign_newly_created_node(*view_id)
            ));
            init_node_expression <- added_node_update.filter_map(|update| Some((update.view_id?, update.expression.clone())));
            view.set_node_expression <+ init_node_expression;
            view.set_node_position <+ added_node_update.filter_map(|update| Some((update.view_id?, update.position)));

            reset_node_types <- any(update_node_expression, init_node_expression)._0();
            set_expression_type <= reset_node_types.map(f!((view_id) model.all_types_of_node(*view_id)));
            set_method_pointer <= reset_node_types.map(f!((view_id) model.all_method_pointers_of_node(*view_id)));
            view.set_expression_usage_type <+ set_expression_type;
            view.set_method_pointer <+ set_method_pointer;

            eval view.node_position_set_batched(((node_id, position)) model.node_position_changed(*node_id, *position));
            eval view.node_removed((node_id) model.node_removed(*node_id));
            eval view.on_edge_endpoints_set((edge_id) model.new_connection_created(*edge_id));
            eval view.on_edge_endpoint_unset(((edge_id,_)) model.connection_removed(*edge_id));

            update_expressions <- source::<Vec<ast::Id>>();
            update_expression <= update_expressions;
            view.set_expression_usage_type <+ update_expression.filter_map(f!((id) model.refresh_expression_type(*id)));
            view.set_method_pointer <+ update_expression.filter_map(f!((id) model.refresh_expression_method_pointer(*id)));
        }

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
