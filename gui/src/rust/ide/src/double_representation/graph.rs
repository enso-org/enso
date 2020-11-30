//! Code for retrieving graph description from AST.

use crate::prelude::*;

use crate::double_representation::definition;
use crate::double_representation::definition::DefinitionInfo;
use crate::double_representation::node;
use crate::double_representation::node::NodeInfo;

use ast::Ast;
use ast::BlockLine;
use ast::known;
use utils::fail::FallibleResult;
use crate::double_representation::connection::Connection;



/// Graph uses the same `Id` as the definition which introduces the graph.
pub type Id = double_representation::definition::Id;



// ====================
// === LocationHint ===
// ====================

/// Describes the desired position of the node's line in the graph's code block.
#[derive(Clone,Copy,Debug)]
pub enum LocationHint {
    /// Try placing this node's line before the line described by id.
    Before(ast::Id),
    /// Try placing this node's line after the line described by id.
    After(ast::Id),
    /// Try placing this node's line at the start of the graph's code block.
    Start,
    /// Try placing this node's line at the end of the graph's code block.
    End,
}



// =================
// === GraphInfo ===
// =================

/// Description of the graph, based on information available in AST.
#[derive(Clone,Debug)]
pub struct GraphInfo {
    /// The definition providing this graph.
    pub source:DefinitionInfo,
}

impl GraphInfo {
    /// Describe graph of the given definition.
    pub fn from_definition(source:DefinitionInfo) -> GraphInfo {
        GraphInfo {source}
    }

    /// Lists nodes in the given binding's ast (infix expression).
    fn from_function_binding(ast:known::Infix) -> Vec<NodeInfo> {
        let body = ast.rarg.clone();
        if let Ok(body_block) = known::Block::try_new(body.clone()) {
            block_nodes(&body_block)
        } else {
            expression_node(body)
        }
    }

    /// Gets the AST of this graph definition.
    pub fn ast(&self) -> Ast {
        self.source.ast.clone().into()
    }

    /// Gets all known nodes in this graph (does not include special pseudo-nodes like graph
    /// inputs and outputs).
    pub fn nodes(&self) -> Vec<NodeInfo> {
        Self::from_function_binding(self.source.ast.clone())
    }

    /// Gets the list of connections between the nodes in this graph.
    pub fn connections(&self) -> Vec<Connection> {
        double_representation::connection::list(&self.source.ast.rarg)
    }

    /// Adds a new node to this graph.
    pub fn add_node
    (&mut self, line_ast:Ast, location_hint:LocationHint) -> FallibleResult {
        let mut lines      = self.source.block_lines()?;
        let last_non_empty = || lines.iter().rposition(|line| line.elem.is_some());
        let index          = match location_hint {
            LocationHint::Start      => 0,
            LocationHint::End        => last_non_empty().map_or(lines.len(),|ix| ix + 1),
            LocationHint::After(id)  => node::index_in_lines(&lines, id)? + 1,
            LocationHint::Before(id) => node::index_in_lines(&lines, id)?
        };
        let elem = Some(line_ast);
        let off  = 0;
        lines.insert(index,BlockLine{elem,off});
        self.source.set_block_lines(lines)
    }

    /// Locates a node with the given id.
    pub fn find_node(&self,id:ast::Id) -> Option<NodeInfo> {
        self.nodes().iter().find(|node| node.id() == id).cloned()
    }

    /// After removing last node, we want to insert a placeholder value for definition value.
    /// This defines its AST. Currently it is just `Nothing`.
    pub fn empty_graph_body() -> Ast {
        Ast::cons(constants::keywords::NOTHING).with_new_id()
    }

    /// Removes the node from graph.
    pub fn remove_node(&mut self, node_id:ast::Id) -> FallibleResult {
        self.update_node(node_id, |_| None)
    }

    /// Sets a new state for the node. The id of the described node must denote already existing
    /// node.
    pub fn set_node(&mut self, node:&NodeInfo) -> FallibleResult {
        self.update_node(node.id(), |_| Some(node.clone()))
    }

    /// Sets a new state for the node. The id of the described node must denote already existing
    /// node.
    pub fn update_node(&mut self, id:ast::Id, f:impl FnOnce(NodeInfo) -> Option<NodeInfo>) -> FallibleResult {
        let mut lines = self.source.block_lines()?;
        let node_entry = lines.iter().enumerate().find_map(|(index,line)| {
            let node     = NodeInfo::from_block_line(line);
            let filtered = node.filter(|node| node.id() == id);
            filtered.map(|node| (index,node))
        });
        if let Some((index,node_info)) = node_entry {
            if let Some(updated_node) = f(node_info) {
                lines[index].elem = Some(updated_node.ast().clone_ref());
            } else {
                lines.remove(index);
            }
            if lines.is_empty() {
                self.source.set_body_ast(Self::empty_graph_body());
                Ok(())
            } else {
                self.source.set_block_lines(lines)
            }
        } else {
            Err(node::IdNotFound {id}.into())
        }
    }

    /// Sets expression of the given node.
    pub fn edit_node(&mut self, node_id:ast::Id, new_expression:Ast) -> FallibleResult {
        self.update_node(node_id, |mut node| {
            node.set_expression(new_expression);
            Some(node)
        })
    }

    #[cfg(test)]
    pub fn expect_code(&self, expected_code:impl Str) {
        let code = self.source.ast.repr();
        assert_eq!(code,expected_code.as_ref());
    }
}



// =====================
// === Listing nodes ===
// =====================

/// Collects information about nodes in given code `Block`.
pub fn block_nodes(ast:&known::Block) -> Vec<NodeInfo> {
    ast.iter().flat_map(|line_ast| {
        let kind   = definition::ScopeKind::NonRoot;
        let indent = ast.indent;
        // If this can be a definition, then don't treat it as a node.
        match definition::DefinitionInfo::from_line_ast(line_ast,kind,indent) {
            None    => NodeInfo::from_line_ast(line_ast),
            Some(_) => None
        }
    }).collect()
}

/// Collects information about nodes in given trivial definition body.
pub fn expression_node(ast:Ast) -> Vec<NodeInfo> {
    NodeInfo::new_expression(ast).into_iter().collect()
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::double_representation::definition::DefinitionName;
    use crate::double_representation::definition::DefinitionProvider;
    use crate::double_representation::module::get_definition;

    use ast::HasRepr;
    use ast::test_utils::expect_single_line;
    use utils::test::ExpectTuple;
    use wasm_bindgen_test::wasm_bindgen_test;

    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    /// Takes a program with main definition in root and returns main's graph.
    fn main_graph(parser:&parser::Parser, program:impl Str) -> GraphInfo {
        let module = parser.parse_module(program.into(), default()).unwrap();
        let name   = DefinitionName::new_plain("main");
        let main   = module.def_iter().find_by_name(&name).unwrap();
        GraphInfo::from_definition(main.item)
    }

    fn find_graph(parser:&parser::Parser, program:impl Str, name:impl Str) -> GraphInfo {
        let module     = parser.parse_module(program.into(), default()).unwrap();
        let crumbs     = name.into().split(".").map(|name| {
            DefinitionName::new_plain(name)
        }).collect();
        let id         = Id{crumbs};
        let definition = get_definition(&module, &id).unwrap();
        GraphInfo::from_definition(definition)
    }

    #[wasm_bindgen_test]
    fn detect_a_node() {
        let mut parser = parser::Parser::new_or_panic();
        // Each of these programs should have a `main` definition with a single `2+2` node.
        let programs = vec![
            "main = 2+2",
            "main = \n    2+2",
            "main = \n    foo = 2+2",
            "main = \n    foo = 2+2\n    bar b = 2+2", // `bar` is a definition, not a node
        ];
        for program in programs {
            let graph = main_graph(&mut parser, program);
            let nodes = graph.nodes();
            assert_eq!(nodes.len(), 1);
            let node = &nodes[0];
            assert_eq!(node.expression().repr(), "2+2");
            let _ = node.id(); // just to make sure it is available
        }
    }

    fn create_node_ast(parser:&parser::Parser, expression:&str) -> (Ast,ast::Id) {
        let node_ast = parser.parse(expression.to_string(), default()).unwrap();
        let line_ast = expect_single_line(&node_ast).clone();
        let id       = line_ast.id.expect("line_ast should have an ID");
        (line_ast,id)
    }

    #[wasm_bindgen_test]
    fn add_node_to_graph_with_single_line() {
        let program   = "main = print \"hello\"";
        let parser    = parser::Parser::new_or_panic();
        let mut graph = main_graph(&parser, program);
        let nodes     = graph.nodes();
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].expression().repr(), "print \"hello\"");

        let expr0 = "a + 2";
        let expr1 = "b + 3";
        let (line_ast0,id0) = create_node_ast(&parser, expr0);
        let (line_ast1,id1) = create_node_ast(&parser, expr1);

        graph.add_node(line_ast0, LocationHint::Start).unwrap();
        assert_eq!(graph.nodes().len(), 2);
        graph.add_node(line_ast1, LocationHint::Before(graph.nodes()[0].id())).unwrap();

        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 3);
        assert_eq!(nodes[0].expression().repr(), expr1);
        assert_eq!(nodes[0].id(), id1);
        assert_eq!(nodes[1].expression().repr(), expr0);
        assert_eq!(nodes[1].id(), id0);
        assert_eq!(nodes[2].expression().repr(), "print \"hello\"");
    }

    #[wasm_bindgen_test]
    fn add_node_to_graph_with_multiple_lines() {
        // TODO [dg] Also add test for binding node when it's possible to update its id.
        let program = r#"main =
    foo = node
    foo a = not_node
    print "hello""#;
        let mut parser = parser::Parser::new_or_panic();
        let mut graph  = main_graph(&mut parser, program);

        let (line_ast0,id0) = create_node_ast(&mut parser, "4 + 4");
        let (line_ast1,id1) = create_node_ast(&mut parser, "a + b");
        let (line_ast2,id2) = create_node_ast(&mut parser, "x * x");
        let (line_ast3,id3) = create_node_ast(&mut parser, "x / x");
        let (line_ast4,id4) = create_node_ast(&mut parser, "2 - 2");

        graph.add_node(line_ast0, LocationHint::Start).unwrap();
        graph.add_node(line_ast1, LocationHint::Before(graph.nodes()[0].id())).unwrap();
        graph.add_node(line_ast2, LocationHint::After(graph.nodes()[1].id())).unwrap();
        graph.add_node(line_ast3, LocationHint::End).unwrap();

        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 6);
        assert_eq!(nodes[0].expression().repr(), "a + b");
        assert_eq!(nodes[0].id(), id1);
        assert_eq!(nodes[1].expression().repr(), "4 + 4");
        assert_eq!(nodes[1].id(), id0);
        assert_eq!(nodes[2].expression().repr(), "x * x");
        assert_eq!(nodes[2].id(), id2);
        assert_eq!(nodes[3].expression().repr(), "node");
        assert_eq!(nodes[4].expression().repr(), "print \"hello\"");
        assert_eq!(nodes[5].expression().repr(), "x / x");
        assert_eq!(nodes[5].id(), id3);

        let expected_code = r#"main =
    a + b
    4 + 4
    x * x
    foo = node
    foo a = not_node
    print "hello"
    x / x"#;
        graph.expect_code(expected_code);

        let mut graph = find_graph(&mut parser, program, "main.foo");
        assert_eq!(graph.nodes().len(), 1);
        graph.add_node(line_ast4, LocationHint::Start).unwrap();
        assert_eq!(graph.nodes().len(), 2);
        assert_eq!(graph.nodes()[0].expression().repr(), "2 - 2");
        assert_eq!(graph.nodes()[0].id(), id4);
        assert_eq!(graph.nodes()[1].expression().repr(), "not_node");
    }

    #[wasm_bindgen_test]
    fn add_node_to_graph_with_blank_line() {
        // The trailing `foo` definition is necessary for the blank line after "node2" to be
        // included in the `main` block. Otherwise, the block would end on "node2" and the blank
        // line would be parented to the module.

        let program = r"main =

    node2

foo = 5";
        let mut parser = parser::Parser::new_or_panic();
        let mut graph  = main_graph(&mut parser, program);

        let id2             = graph.nodes()[0].id();
        let (line_ast0,_id0) = create_node_ast(&mut parser, "node0");
        let (line_ast1,_id1) = create_node_ast(&mut parser, "node1");
        let (line_ast3,_id3) = create_node_ast(&mut parser, "node3");
        let (line_ast4,_id4) = create_node_ast(&mut parser, "node4");

        graph.add_node(line_ast0, LocationHint::Start).unwrap();
        graph.add_node(line_ast1, LocationHint::Before(id2)).unwrap();
        graph.add_node(line_ast3, LocationHint::After(id2)).unwrap();
        graph.add_node(line_ast4, LocationHint::End).unwrap();

        let expected_code = r"main =
    node0

    node1
    node2
    node3
    node4
";
        // `foo` is not part of expected code, as it belongs to module, not `main` graph.
        graph.expect_code(expected_code);
    }

    #[wasm_bindgen_test]
    fn multiple_node_graph() {
        let mut parser = parser::Parser::new_or_panic();
        let program = r"
main =
    foo = node
    foo a = not_node
    node
";
        // TODO [mwu]
        //  Add case like `Int.+ a = not_node` once https://github.com/enso-org/enso/issues/565 is fixed

        let graph = main_graph(&mut parser, program);
        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 2);
        for node in nodes.iter() {
            assert_eq!(node.expression().repr(), "node");
        }
    }

    #[wasm_bindgen_test]
    fn removing_node_from_graph() {
        let mut parser = parser::Parser::new_or_panic();
        let program = r"
main =
    foo = 2 + 2
    bar = 3 + 17";
        let mut graph = main_graph(&mut parser, program);
        let nodes     = graph.nodes();
        assert_eq!(nodes.len(), 2);
        assert_eq!(nodes[0].expression().repr(), "2 + 2");
        assert_eq!(nodes[1].expression().repr(), "3 + 17");

        graph.remove_node(nodes[0].id()).unwrap();

        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].expression().repr(), "3 + 17");

        let expected_code = "main =\n    bar = 3 + 17";
        graph.expect_code(expected_code);

        assert!(graph.remove_node(uuid::Uuid::new_v4()).is_err());
        graph.expect_code(expected_code);
    }

    #[wasm_bindgen_test]
    fn removing_last_node_from_graph() {
        let mut parser = parser::Parser::new_or_panic();
        let program = r"
main =
    foo = 2 + 2";
        let mut graph = main_graph(&mut parser, program);
        println!("aa");
        let (node,)   = graph.nodes().expect_tuple();
        assert_eq!(node.expression().repr(), "2 + 2");
        println!("vv");
        graph.remove_node(node.id()).unwrap();
        println!("zz");

        let (node,)   = graph.nodes().expect_tuple();
        assert_eq!(node.expression().repr(), constants::keywords::NOTHING);
        graph.expect_code("main = Nothing");
    }


    #[wasm_bindgen_test]
    fn editing_nodes_expression_in_graph() {
        let mut parser = parser::Parser::new_or_panic();
        let program = r"
main =
    foo = 2 + 2
    bar = 3 + 17";
        let new_expression = parser.parse("print \"HELLO\"".to_string(), default()).unwrap();
        let new_expression = expect_single_line(&new_expression).clone();

        let mut graph = main_graph(&mut parser, program);
        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 2);
        assert_eq!(nodes[0].expression().repr(), "2 + 2");
        assert_eq!(nodes[1].expression().repr(), "3 + 17");

        graph.edit_node(nodes[0].id(),new_expression).unwrap();

        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 2);
        assert_eq!(nodes[0].expression().repr(), "print \"HELLO\"");
        assert_eq!(nodes[1].expression().repr(), "3 + 17");

        let expected_code = r#"main =
    foo = print "HELLO"
    bar = 3 + 17"#;
        graph.expect_code(expected_code);

        assert!(graph.edit_node(uuid::Uuid::new_v4(), Ast::var("foo")).is_err());
        graph.expect_code(expected_code);
    }
}
