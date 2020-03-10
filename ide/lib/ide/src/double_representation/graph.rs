//! Code for retrieving graph description from AST.

use crate::prelude::*;

use crate::double_representation::definition;
use crate::double_representation::node;

use ast::Ast;
use ast::known;



// =================
// === GraphInfo ===
// =================

/// Description of the graph, based on information available in AST.
#[derive(Clone,Debug)]
pub struct GraphInfo {
    name : definition::DefinitionName,
    args : Vec<Ast>,
    /// Describes all known nodes in this graph (does not include special pseudo-nodes like graph
    /// inputs and outputs).
    pub nodes:Vec<node::NodeInfo>,
}

impl GraphInfo {
    /// Describe graph of the given definition.
    pub fn from_definition(info:&definition::DefinitionInfo) -> GraphInfo {
        let name  = info.name.clone();
        let args  = info.args.clone();
        let nodes = Self::from_function_binding(info.ast.clone());
        GraphInfo {name,args,nodes}
    }

    /// Lists nodes in the given binding's ast (infix expression).
    fn from_function_binding(ast:known::Infix) -> Vec<node::NodeInfo> {
        let body = ast.rarg.clone();
        if let Ok(body_block) = known::Block::try_new(body.clone()) {
            block_nodes(&body_block)
        } else {
            expression_node(body)
        }
    }
}



// =====================
// === Listing nodes ===
// =====================

/// Collects information about nodes in given code `Block`.
pub fn block_nodes(ast:&known::Block) -> Vec<node::NodeInfo> {
    ast.iter().flat_map(|line_ast| {
        // If this can be a definition, then don't treat it as a node.
        match definition::DefinitionInfo::from_line_ast(line_ast, definition::ScopeKind::NonRoot) {
            None    => node::NodeInfo::from_line_ast(line_ast),
            Some(_) => None
        }
    }).collect()
}

/// Collects information about nodes in given trivial definition body.
pub fn expression_node(ast:Ast) -> Vec<node::NodeInfo> {
    node::NodeInfo::new_expression(ast).into_iter().collect()
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::double_representation::definition::DefinitionName;
    use crate::double_representation::definition::DefinitionProvider;

    use parser::api::IsParser;
    use wasm_bindgen_test::wasm_bindgen_test;

    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    /// Takes a program with main definition in root and returns main's graph.
    fn main_graph(parser:&mut impl IsParser, program:impl Str) -> GraphInfo {
        let module = parser.parse_module(program.into(), default()).unwrap();
        let name   = DefinitionName::new_plain("main");
        let main   = module.find_definition(&name).unwrap();
        println!("{:?}",module);
        GraphInfo::from_definition(&main)
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
            assert_eq!(graph.nodes.len(), 1);
            let node = &graph.nodes[0];
            assert_eq!(node.expression_text(), "2+2");
            let _ = node.id(); // just to make sure it is available
        }
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
        //  Add case like `Int.= a = node` once https://github.com/luna/enso/issues/565 is fixed

        let graph = main_graph(&mut parser, program);
        assert_eq!(graph.nodes.len(), 2);
        for node in graph.nodes.iter() {
            assert_eq!(node.expression_text(), "node");
        }
    }
}
