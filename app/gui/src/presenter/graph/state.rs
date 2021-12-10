use crate::prelude::*;

use crate::presenter::graph::AstConnection;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewConnection;
use crate::presenter::graph::ViewNodeId;

use bimap::BiMap;
use bimap::Overwritten;
use ide_view as view;
use ide_view::graph_editor::component::node as node_view;
use ide_view::graph_editor::EdgeEndpoint;



// =============
// === Nodes ===
// =============

#[derive(Clone, Debug, Default)]
pub struct Node {
    pub view_id:    Option<ViewNodeId>,
    pub position:   Vector2,
    pub expression: node_view::Expression,
}

#[derive(Clone, Debug, Default)]
pub struct Nodes {
    displayed_nodes:     HashMap<AstNodeId, Node>,
    nodes_without_view:  Vec<AstNodeId>,
    ast_node_by_view_id: HashMap<ViewNodeId, AstNodeId>,
}

impl Nodes {
    pub fn get(&self, id: AstNodeId) -> Option<&Node> {
        self.displayed_nodes.get(&id)
    }

    pub fn get_mut(&mut self, id: AstNodeId) -> Option<&mut Node> {
        self.displayed_nodes.get_mut(&id)
    }

    pub fn get_mut_or_create(&mut self, id: AstNodeId) -> &mut Node {
        let nodes_without_view = &mut self.nodes_without_view;
        self.displayed_nodes.entry(id).or_insert_with(|| {
            nodes_without_view.push(id);
            default()
        })
    }

    pub fn ast_id_of_view(&self, view_id: ViewNodeId) -> Option<AstNodeId> {
        self.ast_node_by_view_id.get(&view_id).copied()
    }

    pub fn assign_newly_created_node(&mut self, view_id: ViewNodeId) -> Option<&mut Node> {
        let ast_node = self.nodes_without_view.pop()?;
        let mut opt_displayed = self.displayed_nodes.get_mut(&ast_node);
        if let Some(displayed) = &mut opt_displayed {
            displayed.view_id = Some(view_id);
            self.ast_node_by_view_id.insert(view_id, ast_node);
        }
        opt_displayed
    }

    pub fn retain_nodes(&mut self, nodes: &HashSet<AstNodeId>) -> Vec<ViewNodeId> {
        self.nodes_without_view.drain_filter(|id| !nodes.contains(id));
        let removed = self.displayed_nodes.drain_filter(|id, _| !nodes.contains(id));
        let removed_views = removed.filter_map(|(_, data)| data.view_id).collect();
        for view_id in &removed_views {
            self.ast_node_by_view_id.remove(view_id);
        }
        removed_views
    }

    pub fn remove_node(&mut self, node: ViewNodeId) -> Option<AstNodeId> {
        let ast_id = self.ast_node_by_view_id.remove(&node)?;
        self.displayed_nodes.remove(&ast_id);
        Some(ast_id)
    }
}



// ===================
// === Connections ===
// ===================

#[derive(Clone, Debug, Default)]
pub struct Connections {
    connections:              BiMap<AstConnection, ViewConnection>,
    connections_without_view: HashSet<AstConnection>,
}

impl Connections {
    pub fn view_of_ast_connection(&self, connection: &AstConnection) -> Option<ViewConnection> {
        self.connections.get_by_left(connection).copied()
    }

    pub fn retain_connections(
        &mut self,
        connections: &HashSet<AstConnection>,
    ) -> Vec<ViewConnection> {
        self.connections_without_view.retain(|x| connections.contains(x));
        let to_remove = self.connections.iter().filter(|(con, _)| !connections.contains(con));
        let to_remove_vec = to_remove.map(|(_, edge_id)| *edge_id).collect_vec();
        self.connections.retain(|con, _| connections.contains(con));
        to_remove_vec
    }

    pub fn add_ast_connection(&mut self, connection: AstConnection) -> bool {
        if !self.connections.contains_left(&connection) {
            self.connections_without_view.insert(connection)
        } else {
            false
        }
    }

    /// Returns true if the controller needs refreshing.
    pub fn assign_connection_view(
        &mut self,
        connection: AstConnection,
        view: ViewConnection,
    ) -> bool {
        let existed_without_view = self.connections_without_view.remove(&connection);
        match self.connections.insert(connection, view) {
            Overwritten::Neither => !existed_without_view,
            Overwritten::Left(_, _) => false,
            Overwritten::Right(previous, _) => {
                self.connections_without_view.insert(previous);
                !existed_without_view
            }
            Overwritten::Pair(_, _) => false,
            Overwritten::Both(_, (previous, _)) => {
                self.connections_without_view.insert(previous);
                false
            }
        }
    }

    pub fn remove_connection(&mut self, connection: ViewConnection) -> Option<AstConnection> {
        let (ast_connection, _) = self.connections.remove_by_right(&connection)?;
        Some(ast_connection)
    }
}



// ===================
// === Expressions ===
// ===================

#[derive(Clone, Debug, Default)]
pub struct Expression {
    pub node:            AstNodeId,
    pub expression_type: Option<view::graph_editor::Type>,
    pub method_pointer:  Option<view::graph_editor::MethodPointer>,
}

#[derive(Clone, Debug, Default)]
pub struct Expressions {
    expressions:         HashMap<ast::Id, Expression>,
    expressions_of_node: HashMap<AstNodeId, Vec<ast::Id>>,
}

impl Expressions {
    pub fn retain_expression_of_nodes(&mut self, nodes: &HashSet<AstNodeId>) {
        let nodes_to_remove =
            self.expressions_of_node.drain_filter(|node_id, _| !nodes.contains(node_id));
        let expr_to_remove = nodes_to_remove.map(|(_, exprs)| exprs).flatten();
        for expression_id in expr_to_remove {
            self.expressions.remove(&expression_id);
        }
    }

    pub fn node_expression_changed(&mut self, node: AstNodeId, expressions: Vec<ast::Id>) {
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

    pub fn get_mut(&mut self, id: ast::Id) -> Option<&mut Expression> {
        self.expressions.get_mut(&id)
    }

    pub fn subexpressions_of_node(&self, id: ast::Id) -> &[ast::Id] {
        self.expressions_of_node.get(&id).map_or(&[], |v| v.as_slice())
    }
}



// =============
// === State ===
// =============

#[derive(Clone, Debug, Default)]
pub struct State {
    nodes:       RefCell<Nodes>,
    connections: RefCell<Connections>,
    expressions: RefCell<Expressions>,
}


// === Getters ===

impl State {
    pub fn view_id_of_ast_node(&self, node: AstNodeId) -> Option<ViewNodeId> {
        self.nodes.borrow().get(node).and_then(|n| n.view_id)
    }

    pub fn ast_id_of_view_node(&self, node: ViewNodeId) -> Option<AstNodeId> {
        self.nodes.borrow().ast_id_of_view(node)
    }

    pub fn view_of_ast_connection(&self, connection: &AstConnection) -> Option<ViewConnection> {
        self.connections.borrow().view_of_ast_connection(connection)
    }

    pub fn view_edge_targets_of_ast_connection(
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

    pub fn ast_connection_from_view_edge_targets(
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

    pub fn subexpressions_of_node(&self, node: ViewNodeId) -> Vec<ast::Id> {
        let ast_node = self.nodes.borrow().ast_id_of_view(node);
        ast_node
            .map_or_default(|id| self.expressions.borrow().subexpressions_of_node(id).to_owned())
    }
}


// === Nodes Refreshing and Changes ===

impl State {
    pub fn assign_newly_created_node(&self, view_id: ViewNodeId) -> Option<Node> {
        self.nodes.borrow_mut().assign_newly_created_node(view_id).cloned()
    }

    pub fn retain_nodes(&self, nodes: &HashSet<AstNodeId>) -> Vec<ViewNodeId> {
        self.expressions.borrow_mut().retain_expression_of_nodes(nodes);
        self.nodes.borrow_mut().retain_nodes(nodes)
    }

    pub fn refresh_node_position(&self, node: AstNodeId, position: Vector2) -> Option<ViewNodeId> {
        let mut nodes = self.nodes.borrow_mut();
        let mut displayed = nodes.get_mut_or_create(node);
        if displayed.position != position {
            displayed.position = position;
            displayed.view_id
        } else {
            None
        }
    }

    pub fn refresh_node_expression(
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

    pub fn node_position_changed(
        &self,
        id: ViewNodeId,
        new_position: Vector2,
    ) -> Option<AstNodeId> {
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

    pub fn node_removed(&self, id: ViewNodeId) -> Option<AstNodeId> {
        self.nodes.borrow_mut().remove_node(id)
    }
}


// === Connections Refreshing and Changes ===

impl State {
    pub fn assign_connection_view(
        &self,
        connection: view::graph_editor::Edge,
    ) -> Option<AstConnection> {
        let source = connection.source()?;
        let target = connection.target()?;
        self.assign_connection_view_endpoints(connection.id(), source, target)
    }

    pub fn refresh_connection(
        &self,
        connection: AstConnection,
    ) -> Option<(EdgeEndpoint, EdgeEndpoint)> {
        self.connections
            .borrow_mut()
            .add_ast_connection(connection.clone())
            .and_option_from(move || self.view_edge_targets_of_ast_connection(connection))
    }

    pub fn assign_connection_view_endpoints(
        &self,
        connection: ViewConnection,
        source: EdgeEndpoint,
        target: EdgeEndpoint,
    ) -> Option<AstConnection> {
        let ast_connection = self.ast_connection_from_view_edge_targets(source, target)?;
        let mut connections = self.connections.borrow_mut();
        let should_update_controllers =
            connections.assign_connection_view(ast_connection.clone(), connection);
        should_update_controllers.then_some(ast_connection)
    }

    pub fn retain_connections(&self, connections: &HashSet<AstConnection>) -> Vec<ViewConnection> {
        self.connections.borrow_mut().retain_connections(connections)
    }

    pub fn connection_removed(&self, id: ViewConnection) -> Option<AstConnection> {
        self.connections.borrow_mut().remove_connection(id)
    }
}


// === Expression Refreshing ===

impl State {
    pub fn refresh_expression_type(
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

    pub fn refresh_expression_method_pointer(
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
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use engine_protocol::language_server::MethodPointer;
    use parser::Parser;

    fn create_test_node(expression: &str) -> controller::graph::Node {
        let parser = Parser::new_or_panic();
        let ast = parser.parse_line_ast(expression).unwrap();
        controller::graph::Node {
            info:     double_representation::node::NodeInfo {
                documentation: None,
                main_line:     double_representation::node::MainLine::from_ast(&ast).unwrap(),
            },
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
                    state.refresh_node_expression(&node, node_trees_of(&node));
                    state.assign_newly_created_node(view);
                    TestNode { node, view }
                })
                .collect();
            Fixture { state, nodes: displayed_nodes }
        }
    }

    #[test]
    fn adding_and_removing_nodes() {
        let state = State::default();
        let node1 = create_test_node("node1 = 2 + 2");
        let node2 = create_test_node("node2 = node1 + 2");
        let node_view_1 = ensogl::display::object::Id::from(1).into();
        let node_view_2 = ensogl::display::object::Id::from(2).into();

        assert_eq!(state.refresh_node_expression(&node1, node_trees_of(&node1)), None);
        assert_eq!(state.refresh_node_expression(&node2, node_trees_of(&node2)), None);

        assert_eq!(state.view_id_of_ast_node(node1.id()), None);
        assert_eq!(state.view_id_of_ast_node(node2.id()), None);

        let assigned = state.assign_newly_created_node(node_view_2);
        assert_eq!(assigned.map(|node| node.expression.code), Some("node1 + 2".to_owned()));
        let assigned = state.assign_newly_created_node(node_view_1);
        assert_eq!(assigned.map(|node| node.expression.code), Some("2 + 2".to_owned()));

        assert_eq!(state.view_id_of_ast_node(node1.id()), Some(node_view_1));
        assert_eq!(state.view_id_of_ast_node(node2.id()), Some(node_view_2));

        let node1_exprs =
            node1.info.main_line.ast().iter_recursive().filter_map(|a| a.id).collect_vec();
        assert_eq!(state.subexpressions_of_node(node_view_1), node1_exprs);
        let node2_exprs =
            node2.info.main_line.ast().iter_recursive().filter_map(|a| a.id).collect_vec();
        assert_eq!(state.subexpressions_of_node(node_view_2), node2_exprs);

        let views_to_remove = state.retain_nodes(&[node1.id()].iter().copied().collect());
        assert_eq!(views_to_remove, vec![node_view_2]);

        assert_eq!(state.view_id_of_ast_node(node1.id()), Some(node_view_1));
        assert_eq!(state.view_id_of_ast_node(node2.id()), None);

        assert_eq!(state.node_removed(node_view_1), Some(node1.id()));
        assert_eq!(state.view_id_of_ast_node(node1.id()), None)
    }

    #[test]
    fn adding_and_removing_connections() {
        use controller::graph::Endpoint;
        let Fixture { state, nodes } = Fixture::setup_nodes(&["node1 = 2", "node1 + node1"]);
        let source = Endpoint {
            node:       nodes[0].node.id(),
            port:       default(),
            var_crumbs: default(),
        };
        let destination1 = Endpoint {
            node:       nodes[1].node.id(),
            port:       span_tree::Crumbs::new(vec![0]),
            var_crumbs: default(),
        };
        let destination2 = Endpoint {
            node:       nodes[1].node.id(),
            port:       span_tree::Crumbs::new(vec![2]),
            var_crumbs: default(),
        };
        let ast_connection1 =
            AstConnection { source: source.clone(), destination: destination1.clone() };
        let ast_connection2 =
            AstConnection { source: source.clone(), destination: destination2.clone() };
        let view_connection1 = ensogl::display::object::Id::from(1).into();
        let view_connection2 = ensogl::display::object::Id::from(2).into();
        let view_source = EdgeEndpoint { node_id: nodes[0].view, port: source.port.clone() };
        let view_target1 =
            EdgeEndpoint { node_id: nodes[1].view, port: destination1.port.clone() };
        let view_target2 =
            EdgeEndpoint { node_id: nodes[1].view, port: destination2.port.clone() };
        let view_endpoints1 = (view_source.clone(), view_target1.clone());
        let view_endpoints2 = (view_source.clone(), view_target2.clone());

        assert_eq!(state.view_of_ast_connection(&ast_connection1), None);
        assert_eq!(state.view_of_ast_connection(&ast_connection2), None);

        assert_eq!(
            state.refresh_connection(ast_connection1.clone()),
            Some(view_endpoints1.clone())
        );

        assert_eq!(state.view_of_ast_connection(&ast_connection1), None);
        assert_eq!(state.view_of_ast_connection(&ast_connection2), None);

        assert_eq!(
            state.assign_connection_view_endpoints(
                view_connection1,
                view_source.clone(),
                view_target1
            ),
            None
        );
        assert_eq!(
            state.assign_connection_view_endpoints(
                view_connection2,
                view_source.clone(),
                view_target2
            ),
            Some(ast_connection2.clone())
        );

        assert_eq!(state.view_of_ast_connection(&ast_connection1), Some(view_connection1));
        assert_eq!(state.view_of_ast_connection(&ast_connection2), Some(view_connection2));

        assert_eq!(
            state.retain_connections(
                &[ast_connection1.clone(), ast_connection2.clone()].into_iter().collect()
            ),
            vec![]
        );
        assert_eq!(
            state.retain_connections(&[ast_connection2.clone()].into_iter().collect()),
            vec![view_connection1]
        );

        assert_eq!(state.view_of_ast_connection(&ast_connection1), None);
        assert_eq!(state.view_of_ast_connection(&ast_connection2), Some(view_connection2.clone()));

        assert_eq!(state.connection_removed(view_connection2), Some(ast_connection2.clone()));
        assert_eq!(state.view_of_ast_connection(&ast_connection2), None);
    }

    #[test]
    fn refreshing_node_expression() {
        let Fixture { state, nodes } = Fixture::setup_nodes(&["foo bar"]);
        let node_id = nodes[0].node.id();
        let new_ast = Parser::new_or_panic().parse_line_ast("foo baz").unwrap().with_id(node_id);
        let new_node = controller::graph::Node {
            info:     double_representation::node::NodeInfo {
                documentation: None,
                main_line:     double_representation::node::MainLine::from_ast(&new_ast).unwrap(),
            },
            metadata: None,
        };
        let new_subexpressions = new_ast.iter_recursive().filter_map(|ast| ast.id).collect_vec();
        let new_trees = node_trees_of(&new_node);
        let view = nodes[0].view;
        let expected_new_expression = view::graph_editor::component::node::Expression {
            pattern:             None,
            code:                "foo baz".to_string(),
            whole_expression_id: Some(node_id),
            input_span_tree:     new_trees.inputs.clone(),
            output_span_tree:    default(),
        };
        assert_eq!(
            state.refresh_node_expression(&new_node, new_trees.clone()),
            Some((view, expected_new_expression))
        );
        assert_eq!(state.refresh_node_expression(&new_node, new_trees), None);
        assert_eq!(state.subexpressions_of_node(view), new_subexpressions);
    }

    #[test]
    fn updating_node_position() {
        let Fixture { state, nodes } = Fixture::setup_nodes(&["foo"]);
        let node_id = nodes[0].node.id();
        let view_id = nodes[0].view;
        let position_from_ast = Vector2(1.0, 2.0);
        let position_from_view = Vector2(3.0, 4.0);

        assert_eq!(state.refresh_node_position(node_id, position_from_ast), Some(view_id));
        assert_eq!(state.node_position_changed(view_id, position_from_ast), None);
        assert_eq!(state.node_position_changed(view_id, position_from_view), Some(node_id));
        assert_eq!(state.refresh_node_position(node_id, position_from_view), None);
    }

    #[test]
    fn refreshing_expression_types() {
        use ast::crumbs::InfixCrumb;
        let Fixture { state, nodes } = Fixture::setup_nodes(&["2 + 3"]);
        let view = nodes[0].view;
        let node_ast = nodes[0].node.main_line.expression();
        let left_operand = node_ast.get(&InfixCrumb::LeftOperand.into()).unwrap().id.unwrap();
        let right_operand = node_ast.get(&InfixCrumb::RightOperand.into()).unwrap().id.unwrap();

        let number_type = Some(view::graph_editor::Type::from("Number".to_owned()));
        assert_eq!(state.refresh_expression_type(left_operand, number_type.clone()), Some(view));
        assert_eq!(state.refresh_expression_type(right_operand, number_type.clone()), Some(view));

        assert_eq!(state.refresh_expression_type(left_operand, number_type.clone()), None);
        assert_eq!(state.refresh_expression_type(right_operand, number_type), None);

        assert_eq!(state.refresh_expression_type(left_operand, None), Some(view));
        assert_eq!(state.refresh_expression_type(right_operand, None), Some(view));
    }

    #[test]
    fn refreshing_expression_method_pointers() {
        let Fixture { state, nodes } = Fixture::setup_nodes(&["foo bar"]);
        let view = nodes[0].view;
        let expr = nodes[0].node.id();

        let method_ptr = MethodPointer {
            module:          "Foo".to_string(),
            defined_on_type: "Foo".to_string(),
            name:            "foo".to_string(),
        };
        let method_ptr = Some(view::graph_editor::MethodPointer::from(method_ptr));
        assert_eq!(state.refresh_expression_method_pointer(expr, method_ptr.clone()), Some(view));
        assert_eq!(state.refresh_expression_method_pointer(expr, method_ptr), None);
        assert_eq!(state.refresh_expression_method_pointer(expr, None), Some(view));
    }
}
