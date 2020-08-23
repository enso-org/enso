//! Code related to connection discovery and operations.

use crate::prelude::*;

use crate::double_representation::alias_analysis::analyze_crumbable;
use crate::double_representation::identifier::NormalizedName;
use crate::double_representation::node::Id;
use crate::double_representation::node::NodeInfo;

use ast::crumbs::Crumb;
use ast::crumbs::Crumbs;
use crate::double_representation::definition::{DefinitionInfo, ScopeKind};


// ================
// === Endpoint ===
// ================

/// A connection endpoint.
#[derive(Clone,Debug,PartialEq)]
pub struct Endpoint {
    /// Id of the node where the endpoint is located.
    pub node : Id,
    /// Crumbs to the AST creating this endpoint. These crumbs are relative to the node's AST,
    /// not just expression, if the node is binding, there'll crumb for left/right operand first.
    pub crumbs : Crumbs,
}

impl Endpoint {
    /// First crumb identifies line in a given block, i.e. the node. Remaining crumbs identify
    /// AST within the node's AST.
    ///
    /// Returns None if first crumb is not present or does not denote a valid node.
    fn new_in_block(block:&ast::Block<Ast>, mut crumbs:Crumbs) -> Option<Endpoint> {
        let line_crumb = crumbs.pop_front()?;
        let line_crumb = match line_crumb {
            Crumb::Block(block_crumb) => Some(block_crumb),
            _                         => None,
        }?;
        let line_ast   = block.get(&line_crumb).ok()?;
        let definition = DefinitionInfo::from_line_ast(&line_ast,ScopeKind::NonRoot,block.indent);
        let is_non_def = definition.is_none();
        let node       = is_non_def.and_option_from(|| NodeInfo::from_line_ast(&line_ast))?.id();
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
#[derive(Clone,Debug,PartialEq)]
pub struct Connection {
    pub source      : Source,
    pub destination : Destination,
}

/// Lists all the connection in the graph for the given code block.
pub fn list_block(block:&ast::Block<Ast>) -> Vec<Connection> {
    let identifiers      = analyze_crumbable(block);
    let introduced_iter  = identifiers.introduced.into_iter();
    type NameMap         = HashMap<NormalizedName,Endpoint>;
    let introduced_names = introduced_iter.flat_map(|name| {
        let endpoint = Endpoint::new_in_block(block,name.crumbs)?;
        Some((name.item,endpoint))
    }).collect::<NameMap>();
    identifiers.used.into_iter().flat_map(|name| {
        // If name is both introduced and used in the graph's scope; and both of these occurrences
        // can be represented as endpoints, then we have a connection.
        let source      = introduced_names.get(&name.item).cloned()?;
        let destination = Endpoint::new_in_block(block,name.crumbs)?;
        Some(Connection {source,destination})
    }).collect()
}

/// Lists all the connection in the single-expression definition body.
pub fn list_expression(_ast:&Ast) -> Vec<Connection> {
    // At this points single-expression graphs do not have any connection.
    // This will change when there will be input/output pseudo-nodes.
    vec![]
}

/// Lists connections in the given definition body. For now it only makes sense for block shape.
pub fn list(body:&Ast) -> Vec<Connection> {
    match body.shape() {
        ast::Shape::Block(block) => list_block(block),
        _                        => list_expression(body),
    }
}




// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use parser::Parser;
    use crate::double_representation::definition::DefinitionInfo;
    use crate::double_representation::graph::GraphInfo;
    use ast::crumbs;
    use ast::crumbs::InfixCrumb;

    struct TestRun {
        graph       : GraphInfo,
        connections : Vec<Connection>
    }

    impl TestRun {
        fn from_definition(definition:DefinitionInfo) -> TestRun {
            let graph   = GraphInfo::from_definition(definition.clone());
            let repr_of = |connection:&Connection| {
                let endpoint = &connection.source;
                let node     = graph.find_node(endpoint.node).unwrap();
                let ast      = node.ast().get_traversing(&endpoint.crumbs).unwrap();
                ast.repr()
            };

            let mut connections = graph.connections();
            connections.sort_by(|lhs,rhs| {
                repr_of(&lhs).cmp(&repr_of(&rhs))
            });

            TestRun {graph,connections}
        }

        fn from_main_def(code:impl Str) -> TestRun {
            let parser = Parser::new_or_panic();
            let module = parser.parse_module(code,default()).unwrap();
            let definition = DefinitionInfo::from_root_line(&module.lines[0]).unwrap();
            Self::from_definition(definition)
        }

        fn from_block(code:impl Str) -> TestRun {
            let body = code.as_ref().lines().map(|line| format!("    {}", line.trim())).join("\n");
            let definition_code = format!("main =\n{}",body);
            Self::from_main_def(definition_code)
        }

        fn endpoint_node_repr(&self, endpoint:&Endpoint) -> String {
            self.graph.find_node(endpoint.node).unwrap().ast().clone().repr()
        }
    }

    #[wasm_bindgen_test]
    pub fn connection_listing_test_plain() {
        use InfixCrumb::LeftOperand;
        use InfixCrumb::RightOperand;

        let code_block = r"
d,e = p
a = d
b = e
c = a + b
fun a = a b
f = fun 2";


        let run = TestRun::from_block(code_block);
        let c = &run.connections[0];
        assert_eq!(run.endpoint_node_repr(&c.source), "a = d");
        assert_eq!(&c.source.crumbs, &crumbs![LeftOperand]);
        assert_eq!(run.endpoint_node_repr(&c.destination), "c = a + b");
        assert_eq!(&c.destination.crumbs, &crumbs![RightOperand,LeftOperand]);

        let c = &run.connections[1];
        assert_eq!(run.endpoint_node_repr(&c.source), "b = e");
        assert_eq!(&c.source.crumbs, &crumbs![LeftOperand]);
        assert_eq!(run.endpoint_node_repr(&c.destination), "c = a + b");
        assert_eq!(&c.destination.crumbs, &crumbs![RightOperand,RightOperand]);

        let c = &run.connections[2];
        assert_eq!(run.endpoint_node_repr(&c.source), "d,e = p");
        assert_eq!(&c.source.crumbs, &crumbs![LeftOperand,LeftOperand]);
        assert_eq!(run.endpoint_node_repr(&c.destination), "a = d");
        assert_eq!(&c.destination.crumbs, &crumbs![RightOperand]);

        let c = &run.connections[3];
        assert_eq!(run.endpoint_node_repr(&c.source), "d,e = p");
        assert_eq!(&c.source.crumbs, &crumbs![LeftOperand,RightOperand]);
        assert_eq!(run.endpoint_node_repr(&c.destination), "b = e");
        assert_eq!(&c.destination.crumbs, &crumbs![RightOperand]);

        // Note that line `fun a = a b` des not introduce any connections, as it is a definition.

        assert_eq!(run.connections.len(),4);
    }

    #[wasm_bindgen_test]
    pub fn inline_definition() {
        let run = TestRun::from_main_def("main = a");
        assert!(run.connections.is_empty());
    }
}
