//! Code related to connection discovery and operations.

use crate::prelude::*;

use crate::alias_analysis::analyze_crumbable;
use crate::definition::DefinitionInfo;
use crate::definition::ScopeKind;
use crate::node::Id;
use crate::node::MainLine;

use ast::crumbs::Crumb;
use ast::crumbs::Crumbs;



// ================
// === Endpoint ===
// ================

/// A connection endpoint.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Endpoint {
    /// Id of the node where the endpoint is located.
    pub node:   Id,
    /// Crumbs to the AST creating this endpoint. These crumbs are relative to the node's AST,
    /// not just expression, if the node is binding, there'll crumb for left/right operand first.
    pub crumbs: Crumbs,
}

impl Endpoint {
    /// First crumb identifies line in a given block, i.e. the node. Remaining crumbs identify
    /// AST within the node's AST.
    ///
    /// Returns None if first crumb is not present or does not denote a valid node.
    fn new_in_block(block: &ast::Block<Ast>, mut crumbs: Crumbs) -> Option<Endpoint> {
        let line_crumb = crumbs.pop_front()?;
        let line_crumb = match line_crumb {
            Crumb::Block(block_crumb) => Some(block_crumb),
            _ => None,
        }?;
        let line_ast = block.get(&line_crumb).ok()?;
        let definition = DefinitionInfo::from_line_ast(line_ast, ScopeKind::NonRoot, block.indent);
        let is_non_def = definition.is_none();
        let node = is_non_def.and_option_from(|| MainLine::from_ast(line_ast))?.id();
        Some(Endpoint { node, crumbs })
    }
}

/// Connection source, i.e. the port generating the data / identifier introducer.
pub type Source = Endpoint;

/// Connection destination, i.e. the port receiving data / identifier user.
pub type Destination = Endpoint;



// ==================
// === Connection ===
// ==================

/// Describes a connection between two endpoints: from `source` to `destination`.
#[allow(missing_docs)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Connection {
    pub source:      Source,
    pub destination: Destination,
}

/// Lists all the connection in the graph for the given code block.
pub fn list_block(block: &ast::Block<Ast>) -> Vec<Connection> {
    let identifiers = analyze_crumbable(block);
    let introduced_iter = identifiers.introduced.into_iter();
    type NameMap = HashMap<String, Endpoint>;
    let introduced_names = introduced_iter
        .flat_map(|name| {
            let endpoint = Endpoint::new_in_block(block, name.crumbs)?;
            Some((name.item, endpoint))
        })
        .collect::<NameMap>();
    identifiers
        .used
        .into_iter()
        .flat_map(|name| {
            // If name is both introduced and used in the graph's scope; and both of these
            // occurrences can be represented as endpoints, then we have a connection.
            let source = introduced_names.get(&name.item).cloned()?;
            let destination = Endpoint::new_in_block(block, name.crumbs)?;
            Some(Connection { source, destination })
        })
        .collect()
}

/// Lists all the connection in the single-expression definition body.
pub fn list_expression(_ast: &Ast) -> Vec<Connection> {
    // At this points single-expression graphs do not have any connection.
    // This will change when there will be input/output pseudo-nodes.
    vec![]
}

/// Lists connections in the given definition body. For now it only makes sense for block shape.
pub fn list(body: &Ast) -> Vec<Connection> {
    match body.shape() {
        ast::Shape::Block(block) => list_block(block),
        _ => list_expression(body),
    }
}



// ============================
// === Connections Analysis ===
// ============================

/// A function grouping a set of connections by their source node.
pub fn group_by_source_node(
    connections: impl IntoIterator<Item = Connection>,
) -> HashMap<Id, Vec<Connection>> {
    let mut result = HashMap::<Id, Vec<Connection>>::new();
    for connection in connections {
        result.entry(connection.source.node).or_default().push(connection)
    }
    result
}

/// Returns a set of nodes dependent of the given node in the block.
///
/// A node _A_ is dependent on node _B_ if its input is connected to _B_'s output, or to at least
/// one node dependent on _B_.
pub fn dependent_nodes_in_def(body: &Ast, node: Id) -> HashSet<Id> {
    let connections = list(body);
    let node_out_connections = group_by_source_node(connections);
    let mut result = HashSet::new();
    let mut to_visit = vec![node];
    while let Some(current_node) = to_visit.pop() {
        let opt_out_connections = node_out_connections.get(&current_node);
        let out_connections = opt_out_connections.iter().flat_map(|v| v.iter());
        let out_nodes = out_connections.map(|c| c.destination.node);
        let new_nodes_in_result = out_nodes.filter(|n| result.insert(*n));
        for node in new_nodes_in_result {
            to_visit.push(node)
        }
    }
    result
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::definition::DefinitionInfo;
    use crate::graph::GraphInfo;

    use ast::crumbs;
    use ast::crumbs::InfixCrumb;
    use parser::Parser;

    struct TestRun {
        graph:       GraphInfo,
        connections: Vec<Connection>,
    }

    impl TestRun {
        fn from_definition(definition: DefinitionInfo) -> TestRun {
            let graph = GraphInfo::from_definition(definition);
            let repr_of = |connection: &Connection| {
                let endpoint = &connection.source;
                let node = graph.find_node(endpoint.node).unwrap();
                let ast = node.ast().get_traversing(&endpoint.crumbs).unwrap();
                ast.repr()
            };

            let mut connections = graph.connections();
            connections.sort_by_key(|con| repr_of(con));

            TestRun { graph, connections }
        }

        fn from_main_def(code: impl Str) -> TestRun {
            let parser = Parser::new();
            let module = parser.parse_module(code, default()).unwrap();
            let definition = DefinitionInfo::from_root_line(&module.lines[0]).unwrap();
            Self::from_definition(definition)
        }

        fn from_block(code: impl Str) -> TestRun {
            let body = code.as_ref().lines().map(|line| format!("    {}", line.trim())).join("\n");
            let definition_code = format!("main =\n{body}");
            Self::from_main_def(definition_code)
        }

        fn endpoint_node_repr(&self, endpoint: &Endpoint) -> String {
            self.graph.find_node(endpoint.node).unwrap().ast().clone().repr()
        }
    }

    #[test]
    pub fn connection_listing_test_plain() {
        use InfixCrumb::LeftOperand;
        use InfixCrumb::RightOperand;

        let code_block = r"
d = p
a = d
b = d
c = a + b
fun a = a b
f = fun 2";


        let run = TestRun::from_block(code_block);
        let c = &run.connections[0];
        assert_eq!(run.endpoint_node_repr(&c.source), "a = d");
        assert_eq!(&c.source.crumbs, &crumbs![LeftOperand]);
        assert_eq!(run.endpoint_node_repr(&c.destination), "c = a + b");
        assert_eq!(&c.destination.crumbs, &crumbs![RightOperand, LeftOperand]);

        let c = &run.connections[1];
        assert_eq!(run.endpoint_node_repr(&c.source), "b = d");
        assert_eq!(&c.source.crumbs, &crumbs![LeftOperand]);
        assert_eq!(run.endpoint_node_repr(&c.destination), "c = a + b");
        assert_eq!(&c.destination.crumbs, &crumbs![RightOperand, RightOperand]);

        let c = &run.connections[2];
        assert_eq!(run.endpoint_node_repr(&c.source), "d = p");
        assert_eq!(&c.source.crumbs, &crumbs![LeftOperand]);
        assert_eq!(run.endpoint_node_repr(&c.destination), "a = d");
        assert_eq!(&c.destination.crumbs, &crumbs![RightOperand]);

        let c = &run.connections[3];
        assert_eq!(run.endpoint_node_repr(&c.source), "d = p");
        assert_eq!(&c.source.crumbs, &crumbs![LeftOperand]);
        assert_eq!(run.endpoint_node_repr(&c.destination), "b = d");
        assert_eq!(&c.destination.crumbs, &crumbs![RightOperand]);

        // Note that line `fun a = a b` des not introduce any connections, as it is a definition.

        assert_eq!(run.connections.len(), 4);
    }

    #[test]
    pub fn inline_definition() {
        let run = TestRun::from_main_def("main = a");
        assert!(run.connections.is_empty());
    }

    #[test]
    pub fn listing_dependent_nodes() {
        let code_block = "\
            f,g = p
            a = f
            b = g
            c = 2
            d = a + b
            e = b";
        let mut expected_dependent_nodes = HashMap::<&'static str, Vec<&'static str>>::new();
        expected_dependent_nodes.insert("a = f", vec!["d = a + b"]);
        expected_dependent_nodes.insert("b = g", vec!["d = a + b", "e = b"]);
        expected_dependent_nodes.insert("c = 2", vec![]);
        expected_dependent_nodes.insert("d = a + b", vec![]);
        expected_dependent_nodes.insert("e = b", vec![]);

        let TestRun { graph, .. } = TestRun::from_block(code_block);
        let nodes = graph.nodes();
        assert_eq!(nodes.len(), expected_dependent_nodes.len());
        for node in nodes {
            let node_repr = node.ast().repr();
            let expected = expected_dependent_nodes.get(node_repr.as_str()).unwrap();
            let result = dependent_nodes_in_def(graph.source.body().item, node.id());
            let result_node = result.iter().map(|id| graph.find_node(*id).unwrap());
            let mut result_repr = result_node.map(|n| n.ast().repr()).collect_vec();
            result_repr.sort();
            assert_eq!(result_repr, *expected);
        }
    }
}
