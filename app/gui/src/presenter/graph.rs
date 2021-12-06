use crate::prelude::*;
use bimap::BiMap;

use enso_frp as frp;
use ide_view as view;
use ide_view::graph_editor::component::node as node_view;



// ======================
// === DisplayedState ===
// ======================

// === DisplayedNodes ===

type ViewNodeId = view::graph_editor::NodeId;
type AstNodeId = ast::Id;
type ViewConnection = view::graph_editor::EdgeId;
type AstConnection = controller::graph::Connection;

#[derive(Clone, Debug, Default)]
struct DisplayedNode {
    view_id:    Option<ViewNodeId>,
    position:   Vector2,
    expression: node_view::Expression,
}

impl DisplayedNode {
    fn new(view_id: ViewNodeId) -> Self {
        DisplayedNode { view_id: Some(view_id), position: default(), expression: default() }
    }
}

#[derive(Clone, Debug, Default)]
struct DisplayedNodes {
    displayed_nodes:     HashMap<AstNodeId, DisplayedNode>,
    nodes_without_view:  Vec<AstNodeId>,
    ast_node_by_view_id: HashMap<ViewNodeId, AstNodeId>,
}

impl DisplayedNodes {
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

    fn assign_newly_created_node(&mut self, view_id: ViewNodeId) -> Option<DisplayedNode> {
        let ast_node = self.nodes_without_view.pop()?;
        let mut opt_displayed = self.displayed_nodes.get_mut(&ast_node);
        if let Some(displayed) = &mut opt_displayed {
            displayed.view_id = Some(view_id);
            self.ast_node_by_view_id.insert(view_id, ast_node);
        }
        opt_displayed.cloned()
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
}

#[derive(Clone, Debug, Default)]
struct DisplayedExpression {
    node:            ViewNodeId,
    expression_type: Option<view::graph_editor::Type>,
}


// === DisplayedState ===

#[derive(Clone, Debug, Default)]
struct DisplayedState {
    nodes:       RefCell<DisplayedNodes>,
    connections: RefCell<BiMap<AstConnection, ViewConnection>>,
    expressions: RefCell<HashMap<ast::Id, DisplayedExpression>>,
}

impl DisplayedState {
    fn assign_newly_created_node(&self, view_id: ViewNodeId) -> Option<DisplayedNode> {
        self.nodes.borrow_mut().assign_newly_created_node(view_id)
    }

    fn view_id_of_ast_node(&self, node: AstNodeId) -> Option<ViewNodeId> {
        self.nodes.borrow().displayed_nodes.get(&node).and_then(|node| node.view_id)
    }

    fn retain_nodes(&self, nodes: &HashSet<AstNodeId>) -> Vec<ViewNodeId> {
        self.nodes.borrow_mut().retain_nodes(nodes)
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
            Some((displayed.view_id?, new_displayed_expr))
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
}


#[derive(Clone, Debug)]
struct Model {
    logger:     Logger,
    controller: controller::ExecutedGraph,
    view:       view::graph_editor::GraphEditor,
    displayed:  DisplayedState,
}

impl Model {
    pub fn new(
        controller: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
    ) -> Self {
        let logger = Logger::new("presenter::Graph");
        let mappings = default();
        Self { logger, controller, view, displayed: mappings }
    }

    fn node_position_changed(&self, id: ViewNodeId, position: Vector2) {
        if let Some(ast_id) = self.displayed.node_position_changed(id, position) {
            if let Err(err) = self.controller.graph().set_node_position(ast_id, position) {
                error!(self.logger, "Failed to set node position in AST: {err}");
            }
        }
    }
}


#[derive(Clone, Debug, Default)]
struct ViewUpdate {
    nodes_to_remove:    Vec<ViewNodeId>,
    nodes_to_add:       usize,
    expressions_to_set: Vec<(ViewNodeId, node_view::Expression)>,
    positions_to_set:   Vec<(ViewNodeId, Vector2)>,
}

impl ViewUpdate {
    fn new(model: Rc<Model>) -> FallibleResult<Self> {
        let nodes = model.controller.graph().nodes()?;
        let connections = model.controller.connections()?;
        let mut trees = connections.trees;
        let connections = connections.connections;
        let id_of_node = |n: &controller::graph::Node| n.info.main_line.id();
        let node_ids = nodes.iter().map(id_of_node);
        let nodes_to_remove = model.displayed.retain_nodes(&node_ids.clone().collect());
        let nodes_to_add =
            node_ids.filter(|node| model.displayed.view_id_of_ast_node(*node).is_none());
        let expressions_to_set = nodes.iter().filter_map(|node| {
            let id = node.main_line.id();
            let trees = trees.remove(&id).unwrap_or_default();
            model.displayed.refresh_node_expression(node, trees)
        });
        let positions_to_set = nodes.iter().filter_map(|node| {
            let id = node.main_line.id();
            let position = node.position()?.vector;
            let view_id = model.displayed.refresh_node_position(id, position)?;
            Some((view_id, position))
        });
        Ok(Self {
            nodes_to_remove,
            expressions_to_set: expressions_to_set.collect(),
            positions_to_set: positions_to_set.collect(),
            nodes_to_add: nodes_to_add.count(),
        })
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
        let view = &self.model.view.frp;
        frp::extend! { network
            update_view <- source::<()>();
            update_data <- update_view.map(
                f_!([logger,model] match ViewUpdate::new(model.clone_ref()) {
                    Ok(update) => Rc::new(update),
                    Err(err) => {
                        error!(logger,"Failed to update view: {err:?}");
                        Rc::new(default())
                    }
                })
            );

            remove_node <= update_data.map(|update| update.nodes_to_remove.clone());
            set_node_expression <= update_data.map(|update| update.expressions_to_set.clone());
            set_node_position <= update_data.map(|update| update.positions_to_set.clone());
            view.remove_node <+ remove_node;
            view.set_node_expression <+ set_node_expression;
            view.set_node_position <+ set_node_position;

            view.add_node <+ update_data.map(|update| update.nodes_to_add).repeat();
            added_node_update <- view.node_added.filter_map(f!((view_id)
                model.displayed.assign_newly_created_node(*view_id)
            ));
            view.set_node_expression <+ added_node_update.filter_map(|update| Some((update.view_id?, update.expression.clone())));
            view.set_node_position <+ added_node_update.filter_map(|update| Some((update.view_id?, update.position)));
        }
        self
    }
}
