//! The module containing the Graph Presenter [`State`]

use crate::prelude::*;

use crate::presenter::graph::AstConnection;
use crate::presenter::graph::AstEndpoint;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewConnection;
use crate::presenter::graph::ViewNodeId;

use double_representation::context_switch::ContextSwitch;
use double_representation::context_switch::ContextSwitchExpression;
use engine_protocol::language_server::ExpressionUpdatePayload;
use engine_protocol::language_server::SuggestionId;
use ide_view as view;
use ide_view::graph_editor::component::node as node_view;
use ide_view::graph_editor::component::visualization as visualization_view;
use ide_view::graph_editor::EdgeEndpoint;



// =============
// === Nodes ===
// =============

/// A single node data.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct Node {
    pub view_id: Option<ViewNodeId>,
    pub position: Vector2,
    pub expression: node_view::Expression,
    pub is_skipped: bool,
    pub is_frozen: bool,
    pub context_switch: Option<ContextSwitchExpression>,
    pub error: Option<node_view::Error>,
    pub is_pending: bool,
    pub visualization: Option<visualization_view::Path>,
    /// Indicate whether this node view is updated automatically by changes from the controller
    /// or view, or will be explicitly updated.
    disable_expression_auto_update: bool,
}


// === Prepare view updates from the node state ===

#[allow(missing_docs)]
impl Node {
    pub fn position_update(&self) -> Option<(ViewNodeId, Vector2)> {
        Some((self.view_id?, self.position))
    }

    pub fn expression_update(&self) -> Option<(ViewNodeId, node_view::Expression)> {
        Some((self.view_id?, self.expression.clone()))
    }

    pub fn skip_update(&self) -> Option<(ViewNodeId, bool)> {
        Some((self.view_id?, self.is_skipped))
    }

    pub fn freeze_update(&self) -> Option<(ViewNodeId, bool)> {
        Some((self.view_id?, self.is_frozen))
    }

    pub fn output_context_update(&self) -> Option<(ViewNodeId, Option<bool>)> {
        let switch = self.context_switch.as_ref().map(|expr| expr.switch == ContextSwitch::Enable);
        Some((self.view_id?, switch))
    }

    pub fn error_update(&self) -> Option<(ViewNodeId, Option<node_view::Error>)> {
        Some((self.view_id?, self.error.clone()))
    }

    pub fn pending_update(&self) -> Option<(ViewNodeId, bool)> {
        Some((self.view_id?, self.is_pending))
    }

    pub fn visualization_update(&self) -> Option<(ViewNodeId, Option<visualization_view::Path>)> {
        Some((self.view_id?, self.visualization.clone()))
    }
}

/// The set of node states.
///
/// This structure allows to access data of any node by Ast ID, or view id. It also keeps list
/// of the AST nodes with no view assigned, and allows to assign View Id to the next one.
#[derive(Clone, Debug, Default)]
pub struct Nodes {
    // Each operation in this structure should keep the following constraints:
    // * Each `nodes_without_view` entry has an entry in `nodes` with `view_id` being `None`.
    // * All values in `ast_node_by_view_id` has corresponding element in `nodes` with `view_id`
    //   being equal to key of the value.
    nodes:               HashMap<AstNodeId, Node>,
    nodes_without_view:  Vec<AstNodeId>,
    ast_node_by_view_id: HashMap<ViewNodeId, AstNodeId>,
}

impl Nodes {
    /// Get the state of the node by Ast ID.
    pub fn get(&self, id: AstNodeId) -> Option<&Node> {
        self.nodes.get(&id)
    }

    /// Get mutable reference of the node's state by Ast ID.
    pub fn get_mut(&mut self, id: AstNodeId) -> Option<&mut Node> {
        self.nodes.get_mut(&id)
    }

    /// Get id of AST corresponding with the node represented by given view.
    pub fn ast_id_by_view(&self, id: ViewNodeId) -> Option<AstNodeId> {
        self.ast_node_by_view_id.get(&id).copied()
    }

    /// Get the mutable reference, creating an default entry without view if it's missing.
    ///
    /// The entry will be also present on the "nodes without view" list and may have view assigned
    /// using [`assign_newly_created_node`] method.
    pub fn get_mut_or_create(&mut self, id: AstNodeId) -> &mut Node {
        Self::get_mut_or_create_static(&mut self.nodes, &mut self.nodes_without_view, id)
    }

    fn get_mut_or_create_static<'a>(
        nodes: &'a mut HashMap<AstNodeId, Node>,
        nodes_without_view: &mut Vec<AstNodeId>,
        id: AstNodeId,
    ) -> &'a mut Node {
        nodes.entry(id).or_insert_with(|| {
            nodes_without_view.push(id);
            default()
        })
    }

    /// Get the AST ID of the node represented by given view. Returns None, if the node view does
    /// not represent any AST node.
    pub fn ast_id_of_view(&self, view_id: ViewNodeId) -> Option<AstNodeId> {
        self.ast_node_by_view_id.get(&view_id).copied()
    }

    /// Assign a node view to the one of AST nodes without view. If there is any of such nodes,
    /// `None` is returned. Otherwise, returns the node state - the newly created view must be
    /// refreshed with the data from the state.
    pub fn assign_newly_created_node(&mut self, view_id: ViewNodeId) -> Option<&mut Node> {
        let ast_node = self.nodes_without_view.pop()?;
        let mut opt_displayed = self.nodes.get_mut(&ast_node);
        if let Some(displayed) = &mut opt_displayed {
            displayed.view_id = Some(view_id);
            self.ast_node_by_view_id.insert(view_id, ast_node);
        }
        opt_displayed
    }

    /// Assign a node view to a concrete AST node. Returns the node state: the view must be
    /// refreshed with the data from the state.
    pub fn assign_node_view_explicitly(
        &mut self,
        view_id: ViewNodeId,
        ast_id: AstNodeId,
    ) -> &mut Node {
        let mut displayed = self.nodes.entry(ast_id).or_default();
        let old_ast_to_remove = if let Some(old_view) = displayed.view_id {
            self.ast_node_by_view_id.remove(&old_view)
        } else {
            self.nodes_without_view.remove_item(&ast_id);
            None
        };
        displayed.view_id = Some(view_id);
        self.ast_node_by_view_id.insert(view_id, ast_id);
        // That is quite unfortunate, but we have to temporarily drop the acquired borrow over
        // `nodes` to remove the old ast. It is needed to ensure we never have two displayed nodes
        // updated from a single ast node.
        if let Some(old_ast) = old_ast_to_remove {
            self.nodes.remove(&old_ast);
        }
        // Unwrap is safe here, because we just inserted the node to the map.
        self.nodes.get_mut(&ast_id).unwrap()
    }

    /// Update the state retaining given set of nodes. Returns the list of removed nodes' views.
    pub fn retain_nodes(&mut self, nodes: &HashSet<AstNodeId>) -> Vec<ViewNodeId> {
        self.nodes_without_view.drain_filter(|id| !nodes.contains(id));
        let removed = self.nodes.drain_filter(|id, _| !nodes.contains(id));
        let removed_views = removed.filter_map(|(_, data)| data.view_id).collect();
        for view_id in &removed_views {
            self.ast_node_by_view_id.remove(view_id);
        }
        removed_views
    }

    /// Remove node represented by given view (if any) and return its AST ID.
    pub fn remove_node(&mut self, node: ViewNodeId) -> Option<AstNodeId> {
        let ast_id = self.ast_node_by_view_id.remove(&node)?;
        self.nodes.remove(&ast_id);
        Some(ast_id)
    }
}


// ===================
// === Expressions ===
// ===================

/// A single expression data.
#[derive(Clone, Debug, Default)]
pub struct Expression {
    /// A node whose line contains this expression.
    pub node:            AstNodeId,
    /// The known type of the expression.
    pub expression_type: Option<view::graph_editor::Type>,
    /// Suggestion database ID the method called by this expression.
    pub suggestion_id:   Option<SuggestionId>,
    /// A AST ID of `self` argument associated with a method call represented by this expression.
    pub target_id:       Option<ast::Id>,
}

/// The data of node's expressions.
///
/// The expressions are all AST nodes of the line representing the node in the code.
#[derive(Clone, Debug, Default)]
pub struct Expressions {
    expressions:         HashMap<ast::Id, Expression>,
    expressions_of_node: HashMap<AstNodeId, Vec<ast::Id>>,
}

impl Expressions {
    /// Remove all expressions not belonging to the any of the `nodes`.
    pub fn retain_expression_of_nodes(&mut self, nodes: &HashSet<AstNodeId>) {
        let nodes_to_remove =
            self.expressions_of_node.drain_filter(|node_id, _| !nodes.contains(node_id));
        let expr_to_remove = nodes_to_remove.flat_map(|(_, exprs)| exprs);
        for expression_id in expr_to_remove {
            self.expressions.remove(&expression_id);
        }
    }

    /// Update information about node expressions.
    ///
    /// New node's expressions are added, and those which stopped to be part of the node are
    /// removed.
    pub fn update_node_expressions(&mut self, node: AstNodeId, expressions: Vec<ast::Id>) {
        let new_set: HashSet<ast::Id> = expressions.iter().copied().collect();
        let old_set = self.expressions_of_node.insert(node, expressions).unwrap_or_default();
        for old_expression in &old_set {
            if !new_set.contains(old_expression) {
                self.expressions.remove(old_expression);
            }
        }
        for new_expression in new_set {
            if !old_set.contains(&new_expression) {
                self.expressions.insert(new_expression, Expression { node, ..default() });
            }
        }
    }

    /// Get mutable reference to given expression data.
    pub fn get_mut(&mut self, id: ast::Id) -> Option<&mut Expression> {
        self.expressions.get_mut(&id)
    }

    /// Get the list of all expressions of the given node.
    pub fn expressions_of_node(&self, id: AstNodeId) -> &[ast::Id] {
        self.expressions_of_node.get(&id).map_or(&[], |v| v.as_slice())
    }
}



// =============
// === State ===
// =============

/// The Graph Presenter State.
///
/// This structure keeps the information how the particular graph elements received from controllers
/// are represented in the view. It also handles updates from the controllers and
/// the view in `update_from_controller` and `update_from_view` respectively.
#[derive(Clone, Debug, Default)]
pub struct State {
    nodes:       RefCell<Nodes>,
    expressions: RefCell<Expressions>,
}

impl State {
    /// Get the node state by the AST ID.
    pub fn get_node(&self, node: AstNodeId) -> Option<Node> {
        self.nodes.borrow().get(node).cloned()
    }

    /// Get node's view id by the AST ID.
    pub fn view_id_of_ast_node(&self, node: AstNodeId) -> Option<ViewNodeId> {
        self.nodes.borrow().get(node).and_then(|n| n.view_id)
    }

    /// Get node's AST ID by the view id.
    pub fn ast_node_id_of_view(&self, node: ViewNodeId) -> Option<AstNodeId> {
        self.nodes.borrow().ast_id_of_view(node)
    }

    /// Get id of all node's expressions (ids of the all corresponding line AST nodes).
    pub fn expressions_of_node(&self, node: ViewNodeId) -> Vec<ast::Id> {
        let ast_node = self.nodes.borrow().ast_id_of_view(node);
        ast_node.map_or_default(|id| self.expressions.borrow().expressions_of_node(id).to_owned())
    }

    /// Apply the update from controller.
    pub fn update_from_controller(&self) -> ControllerChange {
        ControllerChange { state: self }
    }

    /// Apply the update from the view.
    pub fn update_from_view(&self) -> ViewChange {
        ViewChange { state: self }
    }

    /// Assign a node view to the one of AST nodes without view. If there is any of such nodes,
    /// `None` is returned. Otherwise, returns the node state - the newly created view must be
    /// refreshed with the data from the state.
    pub fn assign_node_view(&self, view_id: ViewNodeId) -> Option<Node> {
        self.nodes.borrow_mut().assign_newly_created_node(view_id).cloned()
    }

    /// Assign a node view to a concrete AST node. Returns the node state: the view must be
    /// refreshed with the data from the state.
    pub fn assign_node_view_explicitly(&self, view_id: ViewNodeId, ast_id: AstNodeId) -> Node {
        self.nodes.borrow_mut().assign_node_view_explicitly(view_id, ast_id).clone()
    }

    /// Checks if the node should be synced with its AST automatically.
    pub fn should_receive_expression_auto_updates(&self, node: ast::Id) -> bool {
        // When node is in process of being created, it is not yet present in the state. In that
        // case the initial expression update needs to be processed. Otherwise the node would be
        // created without any expression.
        let auto_update_disabled = self
            .nodes
            .borrow()
            .get(node)
            .map_or_default(|node| node.disable_expression_auto_update);
        !auto_update_disabled
    }

    /// Set the flag that indicates if the node should be synced with its AST automatically.
    pub fn allow_expression_auto_updates(&self, node: ast::Id, allow: bool) {
        if let Some(node) = self.nodes.borrow_mut().get_mut(node) {
            node.disable_expression_auto_update = !allow;
        }
    }
}

// ========================
// === ControllerChange ===
// ========================

/// The wrapper for [`State`] reference providing the API to be called when presenter is notified
/// by controllers about graph change.
///
/// All of its operations updates the [`State`] to synchronize it with the graph in AST, and returns
/// the information how to update yje view, to have the view synchronized with the state.
///
/// In the particular case, when the graph was changed due to user interations with the view, these
/// method should  discover that no change in state is needed (because it was updated already by
/// [`ViewChange`]), and so the view's. This way we avoid an infinite synchronization cycle.
#[derive(Deref, DerefMut, Debug)]
pub struct ControllerChange<'a> {
    state: &'a State,
}


// === Nodes ===

impl<'a> ControllerChange<'a> {
    /// Remove all nodes not belonging to the given set. Returns the list of to-be-removed views.
    pub fn retain_nodes(&self, nodes: &HashSet<AstNodeId>) -> Vec<ViewNodeId> {
        self.expressions.borrow_mut().retain_expression_of_nodes(nodes);
        self.nodes.borrow_mut().retain_nodes(nodes)
    }

    /// Set the new node position. If the node position actually changed, the to-be-updated view
    /// is returned.
    pub fn set_node_position(&self, node: AstNodeId, position: Vector2) -> Option<ViewNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let mut displayed = nodes.get_mut_or_create(node);
        if displayed.position != position {
            displayed.position = position;
            displayed.view_id
        } else {
            None
        }
    }

    /// Set the new node expression. If the expression actually changed, the to-be-updated view
    /// is returned with the new expression to set.
    pub fn set_node_expression(
        &self,
        node: &controller::graph::Node,
        trees: controller::graph::NodeTrees,
    ) -> Option<(ViewNodeId, node_view::Expression)> {
        let ast_id = node.main_line.id();
        let new_displayed_expr = node_view::Expression {
            pattern:             node.info.pattern().map(|t| t.repr()),
            code:                node.info.expression().repr().into(),
            whole_expression_id: Some(node.info.id()),
            input_span_tree:     trees.inputs,
            output_span_tree:    trees.outputs.unwrap_or_else(default),
        };
        let mut nodes = self.nodes.borrow_mut();
        let displayed = nodes.get_mut_or_create(ast_id);

        let displayed_updated = displayed.expression != new_displayed_expr;
        let context_switch_updated = displayed.context_switch != node.info.ast_info.context_switch;
        let skip_updated = displayed.is_skipped != node.info.macros_info().skip;
        let freeze_updated = displayed.is_frozen != node.info.macros_info().freeze;

        if displayed_updated || context_switch_updated || skip_updated || freeze_updated {
            debug!(
                "Setting node expression from controller: {} -> {}",
                displayed.expression, new_displayed_expr
            );
            displayed.expression = new_displayed_expr.clone();
            let new_expressions =
                node.info.ast().iter_recursive().filter_map(|ast| ast.id).collect();
            self.expressions.borrow_mut().update_node_expressions(ast_id, new_expressions);
            Some((displayed.view_id?, new_displayed_expr))
        } else {
            None
        }
    }

    /// Check if `SKIP` macro is present in the expression and return the updated state (whether the
    /// macro is present). Returns `None` if no changes to the state are needed.
    pub fn set_node_skip(&self, node: &controller::graph::Node) -> Option<bool> {
        let ast_id = node.main_line.id();
        let mut nodes = self.nodes.borrow_mut();
        let displayed = nodes.get_mut_or_create(ast_id);
        let skip = node.info.macros_info().skip;
        if displayed.is_skipped != skip {
            displayed.is_skipped = skip;
            Some(skip)
        } else {
            None
        }
    }

    /// Check if `FREEZE` macro is present in the expression and return the updated state (whether
    /// the macro is present). Returns `None` if no changes to the state are needed.
    pub fn set_node_freeze(&self, node: &controller::graph::Node) -> Option<bool> {
        let ast_id = node.main_line.id();
        let mut nodes = self.nodes.borrow_mut();
        let displayed = nodes.get_mut_or_create(ast_id);
        let freeze = node.info.macros_info().freeze;
        if displayed.is_frozen != freeze {
            displayed.is_frozen = freeze;
            Some(freeze)
        } else {
            None
        }
    }

    /// Check if context switch expression is present in the expression and return it.
    /// Returns a nested option:
    /// - `None` if no changes to the state are needed.
    /// - `Some(None)` if the expression was removed.
    /// - `Some(Some(_))` if the expression was added.
    pub fn set_node_context_switch(
        &self,
        node: &controller::graph::Node,
    ) -> Option<Option<ContextSwitchExpression>> {
        let ast_id = node.main_line.id();
        let mut nodes = self.nodes.borrow_mut();
        let displayed = nodes.get_mut_or_create(ast_id);
        let expr = node.info.ast_info.context_switch.clone();
        if displayed.context_switch != expr {
            displayed.context_switch = expr.clone();
            Some(expr)
        } else {
            None
        }
    }

    /// Set the node error basing of the given expression's payload. If the error is actually
    /// changed, the to-be-updated node view is returned with the proper error description. If the
    /// expression is not a whole expression of any node, nothing is updated and `None` is returned.
    pub fn set_node_error_from_payload(
        &self,
        expression: ast::Id,
        payload: Option<ExpressionUpdatePayload>,
    ) -> Option<(ViewNodeId, Option<node_view::Error>)> {
        let node_id = self.state.nodes.borrow().get(expression).is_some().as_some(expression)?;
        let new_error = self.convert_payload_to_error(node_id, payload);
        let mut nodes = self.nodes.borrow_mut();
        let displayed = nodes.get_mut(node_id)?;
        if displayed.error != new_error {
            displayed.error = new_error.clone();
            Some((displayed.view_id?, new_error))
        } else {
            None
        }
    }

    /// Set whether this node is currently awaiting completion of execution.
    pub fn set_node_pending(
        &self,
        node_id: ast::Id,
        is_pending: bool,
    ) -> Option<(ViewNodeId, bool)> {
        let mut nodes = self.nodes.borrow_mut();
        let displayed = nodes.get_mut(node_id)?;
        if displayed.is_pending != is_pending {
            displayed.is_pending = is_pending;
            Some((displayed.view_id?, is_pending))
        } else {
            None
        }
    }

    fn convert_payload_to_error(
        &self,
        node_id: AstNodeId,
        payload: Option<ExpressionUpdatePayload>,
    ) -> Option<node_view::error::Error> {
        use node_view::error::Kind;
        use ExpressionUpdatePayload::*;
        let is_propagated = |trace: Vec<AstNodeId>| {
            let nodes = self.nodes.borrow();
            let root_cause = trace.iter().find(|id| nodes.get(**id).is_some());
            !root_cause.contains(&&node_id)
        };
        let (kind, message, propagated) = match payload {
            Some(Value { warnings: Some(warnings) }) if warnings.count > 0 => {
                // We return `None` as message, even though we have a warning text available. We
                // don't want to replace the visualization of the value with a warning text though.
                Some((Kind::Warning, None, false))
            }
            Some(DataflowError { trace }) => Some((Kind::Dataflow, None, is_propagated(trace))),
            Some(Panic { message, trace }) => {
                let message = Some(message);
                let is_propagated = is_propagated(trace);
                Some((Kind::Panic, message, is_propagated))
            }
            _ => None,
        }?;

        let kind = Immutable(kind);
        let message = Rc::new(message);
        let propagated = Immutable(propagated);
        Some(node_view::error::Error { kind, message, propagated })
    }

    /// Set the node's attached visualization. The `visualization_data` should be the content of
    /// `visualization` field in node's metadata. If the visualization actually changes, the
    /// to-be-updated node view is returned with the deserialized visualization path.
    pub fn set_node_visualization(
        &self,
        node_id: AstNodeId,
        visualization_data: Option<serde_json::Value>,
    ) -> Option<(ViewNodeId, Option<visualization_view::Path>)> {
        let controller_path = visualization_data.and_then(|data| {
            // It is perfectly fine to ignore deserialization errors here. This is metadata, that
            // might not even be initialized.
            serde_json::from_value(data).ok()
        });

        let mut nodes = self.state.nodes.borrow_mut();
        let displayed = nodes.get_mut_or_create(node_id);
        if displayed.visualization != controller_path {
            displayed.visualization = controller_path.clone();
            Some((displayed.view_id?, controller_path))
        } else {
            None
        }
    }
}


// === Connections ===

impl<'a> ControllerChange<'a> {
    /// Map controller connections to view connections. Only creates connections where nodes on both
    /// endpoints are currently represented in the view.
    pub fn map_connections(&self, connections: &[AstConnection]) -> Vec<ViewConnection> {
        let nodes = self.nodes.borrow();
        connections
            .iter()
            .filter_map(|connection| {
                let src_node = nodes.get(connection.source.node)?.view_id?;
                let dst_node = nodes.get(connection.target.node)?.view_id?;
                let source = EdgeEndpoint::new(src_node, connection.source.port);
                let target = EdgeEndpoint::new(dst_node, connection.target.port);
                Some(ViewConnection { source, target })
            })
            .collect()
    }
}


// === Expressions ===

impl<'a> ControllerChange<'a> {
    /// Set the new type of expression. If the type actually changes, the to-be-updated view is
    /// returned.
    pub fn set_expression_type(
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

    /// Set the new expression's suggestion ID. If the suggestion ID actually changes, the
    /// to-be-updated view and target (`self` argument) AST ID is returned.
    pub fn set_expression_suggestion(
        &self,
        id: ast::Id,
        suggestion: Option<SuggestionId>,
    ) -> Option<(ViewNodeId, Option<ast::Id>)> {
        let mut expressions = self.expressions.borrow_mut();
        let displayed = expressions.get_mut(id).filter(|d| d.suggestion_id != suggestion)?;
        displayed.suggestion_id = suggestion;
        let nodes = self.nodes.borrow();
        let node = nodes.get(displayed.node)?;
        Some((node.view_id?, displayed.target_id))
    }
}



// ==================
// === ViewChange ===
// ==================

/// The wrapper for [`State`] reference providing the API to be called when presenter is notified
/// about view change.
///
/// All of its operations updates the [`State`] to synchronize it with the graph view, and returns
/// the information how to update the AST graph, to have the AST synchronized with the state.
///
/// In particular case, when the view was changed due to change in controller, these method should
/// discover that no change in state is needed (because it was updated already by
/// [`ControllerChange`]), and so the AST graph's. This way we avoid an infinite synchronization
/// cycle.
#[derive(Deref, DerefMut, Debug)]
pub struct ViewChange<'a> {
    state: &'a State,
}


// === Nodes ===

impl<'a> ViewChange<'a> {
    /// Set the new node position. If the node position actually changed, the AST node to-be-updated
    /// ID is returned.
    pub fn set_node_position(&self, id: ViewNodeId, new_position: Vector2) -> Option<AstNodeId> {
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

    /// Remove the node, and returns its AST ID.
    pub fn remove_node(&self, id: ViewNodeId) -> Option<AstNodeId> {
        self.nodes.borrow_mut().remove_node(id)
    }

    /// Set the new node visualization. If the visualization actually changes, the AST ID of the
    /// affected node is returned.
    pub fn set_node_visualization(
        &self,
        id: ViewNodeId,
        new_path: Option<visualization_view::Path>,
    ) -> Option<AstNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let ast_id = nodes.ast_id_of_view(id)?;
        let displayed = nodes.get_mut(ast_id)?;
        if displayed.visualization != new_path {
            displayed.visualization = new_path;
            Some(ast_id)
        } else {
            None
        }
    }

    /// Mark the node as skipped and return its AST ID. Returns `None` if no changes to the
    /// expression are needed.
    pub fn set_node_skip(&self, id: ViewNodeId, skip: bool) -> Option<AstNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let ast_id = nodes.ast_id_of_view(id)?;
        let displayed = nodes.get_mut(ast_id)?;
        if displayed.is_skipped != skip {
            displayed.is_skipped = skip;
            Some(ast_id)
        } else {
            None
        }
    }

    /// Mark the node as frozen and return its AST ID. Returns `None` if no changes to the
    /// expression are needed.
    pub fn set_node_freeze(&self, id: ViewNodeId, freeze: bool) -> Option<AstNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let ast_id = nodes.ast_id_of_view(id)?;
        let displayed = nodes.get_mut(ast_id)?;
        if displayed.is_frozen != freeze {
            displayed.is_frozen = freeze;
            Some(ast_id)
        } else {
            None
        }
    }

    /// Set the node context switch. Returns `None` if no changes to the expression are needed.
    pub fn set_node_context_switch(
        &self,
        id: ViewNodeId,
        expr: Option<ContextSwitchExpression>,
    ) -> Option<AstNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let ast_id = nodes.ast_id_of_view(id)?;
        let displayed = nodes.get_mut(ast_id)?;
        if displayed.context_switch != expr {
            displayed.context_switch = expr;
            Some(ast_id)
        } else {
            None
        }
    }

    /// Set the node expression.
    pub fn set_node_expression(&self, id: ViewNodeId, expression: ImString) -> Option<AstNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let ast_id = nodes.ast_id_of_view(id)?;
        let displayed = nodes.get_mut(ast_id)?;

        let expression_has_changed = displayed.expression.code != expression;
        if expression_has_changed {
            let expression = node_view::Expression::new_plain(expression);
            debug!("Setting node expression from view: {} -> {}", displayed.expression, expression);
            displayed.expression = expression;
            Some(ast_id)
        } else {
            None
        }
    }

    /// Set AST ID of target argument (`self`) associated with a given call expression. Returns
    /// affected node id when the expression was found, even when the target id is not modified.
    pub fn set_call_expression_target_id(
        &self,
        expression: ast::Id,
        target_id: Option<ast::Id>,
    ) -> Option<AstNodeId> {
        let mut expressions = self.expressions.borrow_mut();
        let displayed = expressions.get_mut(expression)?;
        displayed.target_id = target_id;
        Some(displayed.node)
    }

    /// Determine if an expression span change is valid and has any effect. Returns node AST ID.
    /// Returns `None` if no changes to the expression are needed or when the span doesn't exist.
    pub fn check_node_expression_span_update(
        &self,
        id: ViewNodeId,
        crumbs: &span_tree::Crumbs,
        new_span_expression: &str,
    ) -> Option<AstNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let ast_id = nodes.ast_id_of_view(id)?;
        let displayed = nodes.get_mut(ast_id)?;
        let code = displayed.expression.code.as_str();

        let port_ref = displayed.expression.input_span_tree.get_node(crumbs).ok()?;
        let span = port_ref.span();
        let span_as_range = enso_text::Range::new(span.start, span.end);
        let span_expression = &code[span_as_range];
        debug!("Checking expression span update: {} -> {}", span_expression, new_span_expression);
        let expression_has_changed = span_expression != new_span_expression;
        expression_has_changed.then_some(ast_id)
    }

    /// Map a connection on view side to a connection on controller side. Returns `None` if view
    /// node on either connection endpoint is not represented in the controller.
    pub fn view_to_ast_connection(&self, connection: &ViewConnection) -> Option<AstConnection> {
        let source_node = self.state.ast_node_id_of_view(connection.source.node_id)?;
        let target_node = self.state.ast_node_id_of_view(connection.target.node_id)?;
        Some(AstConnection {
            source: AstEndpoint::new(source_node, connection.source.port),
            target: AstEndpoint::new(target_node, connection.target.port),
        })
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Parser;

    fn create_test_node(expression: &str) -> controller::graph::Node {
        let parser = Parser::new();
        let ast = parser.parse_line_ast(expression).unwrap();
        let main_line = double_representation::node::MainLine::from_ast(&ast).unwrap();
        controller::graph::Node {
            info:     double_representation::node::NodeInfo { documentation: None, main_line },
            metadata: None,
        }
    }

    fn node_trees_of(node: &controller::graph::Node) -> controller::graph::NodeTrees {
        controller::graph::NodeTrees::new(&node.info, &span_tree::generate::context::Empty).unwrap()
    }

    struct TestNode {
        node: controller::graph::Node,
        view: ViewNodeId,
    }

    struct Fixture {
        state: State,
        nodes: Vec<TestNode>,
    }

    impl Fixture {
        fn setup_nodes(expressions: impl IntoIterator<Item: AsRef<str>>) -> Self {
            let nodes = expressions.into_iter().map(|expr| create_test_node(expr.as_ref()));
            let state = State::default();
            let displayed_nodes = nodes
                .enumerate()
                .map(|(i, node)| {
                    let view = ensogl::display::object::Id::from(i).into();
                    state.update_from_controller().set_node_expression(&node, node_trees_of(&node));
                    state.assign_node_view(view);
                    TestNode { node, view }
                })
                .collect();
            Fixture { state, nodes: displayed_nodes }
        }
    }

    #[wasm_bindgen_test]
    fn adding_and_removing_nodes() {
        let state = State::default();
        let node1 = create_test_node("node1 = 2 + 2");
        let node2 = create_test_node("node2 = node1 + 2");
        let node_view_1 = ensogl::display::object::Id::from(1).into();
        let node_view_2 = ensogl::display::object::Id::from(2).into();
        let from_controller = state.update_from_controller();
        let from_view = state.update_from_view();

        assert_eq!(from_controller.set_node_expression(&node1, node_trees_of(&node1)), None);
        assert_eq!(from_controller.set_node_expression(&node2, node_trees_of(&node2)), None);

        assert_eq!(state.view_id_of_ast_node(node1.id()), None);
        assert_eq!(state.view_id_of_ast_node(node2.id()), None);

        let assigned = state.assign_node_view(node_view_2);
        assert_eq!(assigned.as_ref().map(|node| node.expression.code.as_str()), Some("node1 + 2"));
        let assigned = state.assign_node_view(node_view_1);
        assert_eq!(assigned.as_ref().map(|node| node.expression.code.as_str()), Some("2 + 2"));

        assert_eq!(state.view_id_of_ast_node(node1.id()), Some(node_view_1));
        assert_eq!(state.view_id_of_ast_node(node2.id()), Some(node_view_2));

        let node1_exprs =
            node1.info.main_line.ast().iter_recursive().filter_map(|a| a.id).collect_vec();
        assert_eq!(state.expressions_of_node(node_view_1), node1_exprs);
        let node2_exprs =
            node2.info.main_line.ast().iter_recursive().filter_map(|a| a.id).collect_vec();
        assert_eq!(state.expressions_of_node(node_view_2), node2_exprs);

        let views_to_remove = from_controller.retain_nodes(&[node1.id()].iter().copied().collect());
        assert_eq!(views_to_remove, vec![node_view_2]);

        assert_eq!(state.view_id_of_ast_node(node1.id()), Some(node_view_1));
        assert_eq!(state.view_id_of_ast_node(node2.id()), None);

        assert_eq!(from_view.remove_node(node_view_1), Some(node1.id()));
        assert_eq!(state.view_id_of_ast_node(node1.id()), None)
    }

    #[wasm_bindgen_test]
    fn refreshing_node_expression() {
        let Fixture { state, nodes } = Fixture::setup_nodes(&["foo bar"]);
        let node_id = nodes[0].node.id();
        let new_ast = Parser::new().parse_line_ast("foo baz").unwrap().with_id(node_id);
        let main_line = double_representation::node::MainLine::from_ast(&new_ast).unwrap();
        let new_node = controller::graph::Node {
            info:     double_representation::node::NodeInfo { documentation: None, main_line },
            metadata: None,
        };
        let new_subexpressions = new_ast.iter_recursive().filter_map(|ast| ast.id).collect_vec();
        let new_trees = node_trees_of(&new_node);
        let view = nodes[0].view;
        let expected_new_expression = view::graph_editor::component::node::Expression {
            pattern:             None,
            code:                "foo baz".into(),
            whole_expression_id: Some(node_id),
            input_span_tree:     new_trees.inputs.clone(),
            output_span_tree:    default(),
        };
        let updater = state.update_from_controller();
        assert_eq!(
            updater.set_node_expression(&new_node, new_trees.clone()),
            Some((view, expected_new_expression))
        );
        assert_eq!(updater.set_node_expression(&new_node, new_trees), None);
        assert_eq!(state.expressions_of_node(view), new_subexpressions);
    }

    #[wasm_bindgen_test]
    fn updating_node_position() {
        let Fixture { state, nodes } = Fixture::setup_nodes(&["foo"]);
        let node_id = nodes[0].node.id();
        let view_id = nodes[0].view;
        let position_from_ast = Vector2(1.0, 2.0);
        let position_from_view = Vector2(3.0, 4.0);
        let from_controller = state.update_from_controller();
        let from_view = state.update_from_view();

        assert_eq!(from_controller.set_node_position(node_id, position_from_ast), Some(view_id));
        assert_eq!(from_view.set_node_position(view_id, position_from_ast), None);
        assert_eq!(from_view.set_node_position(view_id, position_from_view), Some(node_id));
        assert_eq!(from_controller.set_node_position(node_id, position_from_view), None);
    }

    #[wasm_bindgen_test]
    fn refreshing_expression_types() {
        use ast::crumbs::InfixCrumb;
        let Fixture { state, nodes } = Fixture::setup_nodes(&["2 + 3"]);
        let view = nodes[0].view;
        let node_ast = nodes[0].node.expression();
        let left_operand = node_ast.get(&InfixCrumb::LeftOperand.into()).unwrap().id.unwrap();
        let right_operand = node_ast.get(&InfixCrumb::RightOperand.into()).unwrap().id.unwrap();
        let updater = state.update_from_controller();

        let number_type = Some(view::graph_editor::Type::from("Number".to_owned()));
        assert_eq!(updater.set_expression_type(left_operand, number_type.clone()), Some(view));
        assert_eq!(updater.set_expression_type(right_operand, number_type.clone()), Some(view));

        assert_eq!(updater.set_expression_type(left_operand, number_type.clone()), None);
        assert_eq!(updater.set_expression_type(right_operand, number_type), None);

        assert_eq!(updater.set_expression_type(left_operand, None), Some(view));
        assert_eq!(updater.set_expression_type(right_operand, None), Some(view));
    }

    #[wasm_bindgen_test]
    fn refreshing_expression_method_pointers() {
        let Fixture { state, nodes } = Fixture::setup_nodes(&["foo bar"]);
        let view = nodes[0].view;
        let expr = nodes[0].node.id();
        let updater = state.update_from_controller();

        assert_eq!(updater.set_expression_suggestion(expr, Some(5)), Some((view, None)));
        assert_eq!(updater.set_expression_suggestion(expr, Some(5)), None);
        assert_eq!(updater.set_expression_suggestion(expr, None), Some((view, None)));
    }
}
