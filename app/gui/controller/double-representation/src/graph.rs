//! Code for retrieving graph description from AST.

use crate::prelude::*;

use crate::connection;
use crate::connection::Connection;
use crate::definition;
use crate::definition::DefinitionInfo;
use crate::definition::DefinitionProvider;
use crate::node;
use crate::node::LocatedNode;
use crate::node::NodeInfo;

use ast::known;
use ast::Ast;
use ast::BlockLine;



/// Graph uses the same `Id` as the definition which introduces the graph.
pub type Id = definition::Id;



// ====================
// === LocationHint ===
// ====================

/// Describes the desired position of the node's line in the graph's code block.
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Debug, Shrinkwrap)]
pub struct GraphInfo {
    /// The definition providing this graph.
    pub source: DefinitionInfo,
}

impl GraphInfo {
    /// Look for a node with given id in the graph.
    pub fn locate_node(&self, id: node::Id) -> FallibleResult<LocatedNode> {
        let lines = self.source.block_lines();
        node::locate(&lines, self.source.context_indent, id)
    }

    /// Describe graph of the given definition.
    pub fn from_definition(source: DefinitionInfo) -> GraphInfo {
        GraphInfo { source }
    }

    /// Gets the AST of this graph definition.
    pub fn ast(&self) -> Ast {
        self.source.ast.clone().into()
    }

    /// Gets all known nodes in this graph (does not include special pseudo-nodes like graph
    /// inputs and outputs).
    pub fn nodes(&self) -> Vec<NodeInfo> {
        let ast = &self.source.ast;
        let body = &ast.rarg;
        if let Ok(body_block) = known::Block::try_new(body.clone()) {
            let context_indent = self.source.indent();
            let lines_iter = body_block.enumerate_non_empty_lines();
            let nodes_iter = node::NodeIterator { lines_iter, context_indent };
            nodes_iter.map(|n| n.node).collect()
        } else if let Some(node) = node::NodeInfo::from_main_line_ast(body) {
            // There's no way to attach a documentation comment to an inline node, it consists only
            // of the main line.
            vec![node]
        } else {
            // It should not be possible to have empty definition without any nodes but it is
            // possible to represent such thing in AST. Anyway, it has no nodes.
            vec![]
        }
    }

    /// Gets the list of connections between the nodes in this graph.
    pub fn connections(&self) -> Vec<Connection> {
        connection::list(&self.source.ast.rarg)
    }

    /// Adds a new node to this graph.
    pub fn add_node(&mut self, node: &NodeInfo, location_hint: LocationHint) -> FallibleResult {
        let mut lines = self.source.block_lines();
        let last_non_empty = || lines.iter().rposition(|line| line.elem.is_some());
        let index = match location_hint {
            LocationHint::Start => 0,
            LocationHint::End => last_non_empty().map_or(lines.len(), |ix| ix + 1),
            LocationHint::After(id) => self.locate_node(id)?.index.last() + 1,
            LocationHint::Before(id) => self.locate_node(id)?.index.first(),
        };
        let elem = Some(node.ast().clone_ref());
        let off = 0;
        lines.insert(index, BlockLine { elem, off });
        if let Some(documentation) = &node.documentation {
            let elem = Some(documentation.ast().into());
            let line = BlockLine { elem, off };
            lines.insert(index, line);
        }
        self.source.set_block_lines(lines)
    }

    /// Locates a node with the given id.
    pub fn find_node(&self, id: ast::Id) -> Option<NodeInfo> {
        self.nodes().iter().find(|node| node.id() == id).cloned()
    }

    /// After removing last node, we want to insert a placeholder value for definition value.
    /// This defines its AST. Currently it is just `Nothing`.
    pub fn empty_graph_body() -> Ast {
        Ast::cons(ast::constants::keywords::NOTHING).with_new_id()
    }

    /// Removes the node from graph.
    pub fn remove_node(&mut self, node_id: ast::Id) -> FallibleResult {
        self.update_node(node_id, |_| None)
    }

    /// Sets a new state for the node. The id of the described node must denote already existing
    /// node.
    pub fn set_node(&mut self, node: &NodeInfo) -> FallibleResult {
        self.update_node(node.id(), |_| Some(node.clone()))
    }

    /// Sets a new state for the node. The id of the described node must denote already existing
    /// node.
    pub fn update_node(
        &mut self,
        id: ast::Id,
        f: impl FnOnce(NodeInfo) -> Option<NodeInfo>,
    ) -> FallibleResult {
        let LocatedNode { index, node } = self.locate_node(id)?;

        let mut lines = self.source.block_lines();
        if let Some(updated_node) = f(node) {
            lines[index.main_line].elem = Some(updated_node.main_line.ast().clone_ref());
            match (index.documentation_line, updated_node.documentation) {
                (Some(old_comment_index), None) => {
                    lines.remove(old_comment_index);
                }
                (Some(old_comment_index), Some(new_comment)) =>
                    lines[old_comment_index] = new_comment.block_line(),
                (None, Some(new_comment)) =>
                    lines.insert(index.main_line, new_comment.block_line()),
                (None, None) => {}
            }
        } else {
            lines.remove(index.main_line);
            if let Some(doc_index) = index.documentation_line {
                lines.remove(doc_index);
            }
        }
        if lines.is_empty() {
            self.source.set_body_ast(Self::empty_graph_body());
            Ok(())
        } else {
            self.source.set_block_lines(lines)
        }

        // TODO tests for cases with comments involved
    }

    /// Sets expression of the given node.
    #[profile(Debug)]
    pub fn edit_node(&mut self, node_id: ast::Id, new_expression: Ast) -> FallibleResult {
        self.update_node(node_id, |mut node| {
            node.set_expression(new_expression);
            Some(node)
        })
    }

    #[cfg(test)]
    pub fn expect_code(&self, expected_code: impl Str) {
        let code = self.source.ast.repr();
        assert_eq!(code, expected_code.as_ref());
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::definition::DefinitionName;
    use crate::definition::DefinitionProvider;
    use crate::module::get_definition;

    use ast::macros::DocumentationCommentInfo;
    use ast::test_utils::expect_single_line;
    use ast::HasRepr;
    use wasm_bindgen_test::wasm_bindgen_test;

    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    /// Takes a program with main definition in root and returns main's graph.
    fn main_graph(parser: &parser_scala::Parser, program: impl Str) -> GraphInfo {
        let module = parser.parse_module(program.into(), default()).unwrap();
        let name = DefinitionName::new_plain("main");
        let main = module.def_iter().find_by_name(&name).unwrap();
        GraphInfo::from_definition(main.item)
    }

    fn find_graph(parser: &parser_scala::Parser, program: impl Str, name: impl Str) -> GraphInfo {
        let module = parser.parse_module(program.into(), default()).unwrap();
        let crumbs = name.into().split('.').map(DefinitionName::new_plain).collect();
        let id = Id { crumbs };
        let definition = get_definition(&module, &id).unwrap();
        GraphInfo::from_definition(definition)
    }

    #[wasm_bindgen_test]
    fn detect_a_node() {
        let parser = parser_scala::Parser::new_or_panic();
        // Each of these programs should have a `main` definition with a single `2+2` node.
        let programs = vec![
            "main = 2+2",
            "main = \n    2+2",
            "main = \n    foo = 2+2",
            "main = \n    foo = 2+2\n    bar b = 2+2", // `bar` is a definition, not a node
        ];
        for program in programs {
            let graph = main_graph(&parser, program);
            let nodes = graph.nodes();
            assert_eq!(nodes.len(), 1);
            let node = &nodes[0];
            assert_eq!(node.expression().repr(), "2+2");
            let _ = node.id(); // just to make sure it is available
        }
    }

    fn new_expression_node(parser: &parser_scala::Parser, expression: &str) -> NodeInfo {
        let node_ast = parser.parse(expression.to_string(), default()).unwrap();
        let line_ast = expect_single_line(&node_ast).clone();
        NodeInfo::from_main_line_ast(&line_ast).unwrap()
    }

    fn assert_all(nodes: &[NodeInfo], expected: &[NodeInfo]) {
        assert_eq!(nodes.len(), expected.len());
        for (left, right) in nodes.iter().zip(expected) {
            assert_same(left, right)
        }
    }

    fn assert_same(left: &NodeInfo, right: &NodeInfo) {
        assert_eq!(left.id(), right.id());
        assert_eq!(
            left.documentation.as_ref().map(DocumentationCommentInfo::to_string),
            right.documentation.as_ref().map(DocumentationCommentInfo::to_string)
        );
        assert_eq!(left.main_line.repr(), right.main_line.repr());
    }

    #[wasm_bindgen_test]
    fn add_node_to_graph_with_single_line() {
        let program = "main = print \"hello\"";
        let parser = parser_scala::Parser::new_or_panic();
        let mut graph = main_graph(&parser, program);
        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 1);
        let initial_node = nodes[0].clone();
        assert_eq!(initial_node.expression().repr(), "print \"hello\"");

        let expr0 = "a + 2";
        let expr1 = "b + 3";
        let node_to_add0 = new_expression_node(&parser, expr0);
        let node_to_add1 = new_expression_node(&parser, expr1);

        graph.add_node(&node_to_add0, LocationHint::Start).unwrap();
        assert_eq!(graph.nodes().len(), 2);
        graph.add_node(&node_to_add1, LocationHint::Before(graph.nodes()[0].id())).unwrap();

        let nodes = graph.nodes();
        assert_all(nodes.as_slice(), &[node_to_add1, node_to_add0, initial_node]);
    }

    #[wasm_bindgen_test]
    fn add_node_to_graph_with_multiple_lines() {
        // TODO [dg] Also add test for binding node when it's possible to update its id.
        let program = r#"main =
    foo = node
    foo a = not_node
    print "hello""#;
        let parser = parser_scala::Parser::new_or_panic();
        let mut graph = main_graph(&parser, program);

        let node_to_add0 = new_expression_node(&parser, "4 + 4");
        let node_to_add1 = new_expression_node(&parser, "a + b");
        let node_to_add2 = new_expression_node(&parser, "x * x");
        let node_to_add3 = new_expression_node(&parser, "x / x");
        let node_to_add4 = new_expression_node(&parser, "2 - 2");

        graph.add_node(&node_to_add0, LocationHint::Start).unwrap();
        graph.add_node(&node_to_add1, LocationHint::Before(graph.nodes()[0].id())).unwrap();
        graph.add_node(&node_to_add2, LocationHint::After(graph.nodes()[1].id())).unwrap();
        graph.add_node(&node_to_add3, LocationHint::End).unwrap();
        // Node 4 will be added later.

        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 6);
        assert_eq!(nodes[0].expression().repr(), "a + b");
        assert_eq!(nodes[0].id(), node_to_add1.id());
        // Sic: `node_to_add1` was added at index `0`.
        assert_eq!(nodes[1].expression().repr(), "4 + 4");
        assert_eq!(nodes[1].id(), node_to_add0.id());
        assert_eq!(nodes[2].expression().repr(), "x * x");
        assert_eq!(nodes[2].id(), node_to_add2.id());
        assert_eq!(nodes[3].expression().repr(), "node");
        assert_eq!(nodes[4].expression().repr(), "print \"hello\"");
        assert_eq!(nodes[5].expression().repr(), "x / x");
        assert_eq!(nodes[5].id(), node_to_add3.id());

        let expected_code = r#"main =
    a + b
    4 + 4
    x * x
    foo = node
    foo a = not_node
    print "hello"
    x / x"#;
        graph.expect_code(expected_code);

        let mut graph = find_graph(&parser, program, "main.foo");
        assert_eq!(graph.nodes().len(), 1);
        graph.add_node(&node_to_add4, LocationHint::Start).unwrap();
        assert_eq!(graph.nodes().len(), 2);
        assert_eq!(graph.nodes()[0].expression().repr(), "2 - 2");
        assert_eq!(graph.nodes()[0].id(), node_to_add4.id());
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
        let parser = parser_scala::Parser::new_or_panic();
        let mut graph = main_graph(&parser, program);

        let id2 = graph.nodes()[0].id();
        let node_to_add0 = new_expression_node(&parser, "node0");
        let node_to_add1 = new_expression_node(&parser, "node1");
        let node_to_add3 = new_expression_node(&parser, "node3");
        let node_to_add4 = new_expression_node(&parser, "node4");

        graph.add_node(&node_to_add0, LocationHint::Start).unwrap();
        graph.add_node(&node_to_add1, LocationHint::Before(id2)).unwrap();
        graph.add_node(&node_to_add3, LocationHint::After(id2)).unwrap();
        graph.add_node(&node_to_add4, LocationHint::End).unwrap();

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
        let parser = parser_scala::Parser::new_or_panic();
        let program = r"
main =
    ## Faux docstring
    ## Docstring 0
    foo = node0
    ## Docstring 1
    # disabled node1
    foo a = not_node
    ## Docstring 2
    node2
    node3
";
        // TODO [mwu]
        //  Add case like `Int.+ a = not_node` once https://github.com/enso-org/enso/issues/565 is fixed

        let graph = main_graph(&parser, program);
        let nodes = graph.nodes();
        assert_eq!(nodes[0].documentation_text(), Some(" Docstring 0".into()));
        assert_eq!(nodes[0].ast().repr(), "foo = node0");
        assert_eq!(nodes[1].documentation_text(), Some(" Docstring 1".into()));
        assert_eq!(nodes[1].ast().repr(), "# disabled node1");
        assert_eq!(nodes[2].documentation_text(), Some(" Docstring 2".into()));
        assert_eq!(nodes[2].ast().repr(), "node2");
        assert_eq!(nodes[3].documentation_text(), None);
        assert_eq!(nodes[3].ast().repr(), "node3");
        assert_eq!(nodes.len(), 4);
    }

    #[wasm_bindgen_test]
    fn removing_node_from_graph() {
        let parser = parser_scala::Parser::new_or_panic();
        let program = r"
main =
    foo = 2 + 2
    bar = 3 + 17";
        let mut graph = main_graph(&parser, program);
        let nodes = graph.nodes();
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
        let parser = parser_scala::Parser::new_or_panic();
        let program = r"
main =
    foo = 2 + 2";
        let mut graph = main_graph(&parser, program);
        DEBUG!("aa");
        let (node,) = graph.nodes().expect_tuple();
        assert_eq!(node.expression().repr(), "2 + 2");
        DEBUG!("vv");
        graph.remove_node(node.id()).unwrap();
        DEBUG!("zz");

        let (node,) = graph.nodes().expect_tuple();
        assert_eq!(node.expression().repr(), ast::constants::keywords::NOTHING);
        graph.expect_code("main = Nothing");
    }


    #[wasm_bindgen_test]
    fn editing_nodes_expression_in_graph() {
        let parser = parser_scala::Parser::new_or_panic();
        let program = r"
main =
    foo = 2 + 2
    bar = 3 + 17";
        let new_expression = parser.parse("print \"HELLO\"".to_string(), default()).unwrap();
        let new_expression = expect_single_line(&new_expression).clone();

        let mut graph = main_graph(&parser, program);
        let nodes = graph.nodes();
        assert_eq!(nodes.len(), 2);
        assert_eq!(nodes[0].expression().repr(), "2 + 2");
        assert_eq!(nodes[1].expression().repr(), "3 + 17");

        graph.edit_node(nodes[0].id(), new_expression).unwrap();

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
