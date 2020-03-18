//! Code for retrieving graph description from AST.

use crate::prelude::*;

use crate::double_representation::definition;
use crate::double_representation::definition::DefinitionInfo;
use crate::double_representation::definition::DefinitionName;
use crate::double_representation::definition::DefinitionProvider;
use crate::double_representation::node;

use ast::Ast;
use ast::known;



// ================
// === Graph Id ===
// ================

/// Crumb describes step that needs to be done when going from context (for graph being a module)
/// to the target.
// TODO [mwu]
//  Currently we support only entering named definitions.
pub type Crumb = DefinitionName;

/// Identifies graph in the module.
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
pub struct Id {
    /// Sequence of traverses from module root up to the identified graph.
    pub crumbs : Vec<Crumb>,
}

impl Id {
    /// Creates a new graph identifier consisting of a single crumb.
    pub fn new_single_crumb(crumb:DefinitionName) -> Id {
        let crumbs = vec![crumb];
        Id {crumbs}
    }
}


// ===============================
// === Finding Graph In Module ===
// ===============================

#[derive(Fail,Clone,Debug)]
#[fail(display="Definition ID was empty")]
struct CannotFindDefinition(Id);

#[derive(Fail,Clone,Debug)]
#[fail(display="Definition ID was empty")]
struct EmptyDefinitionId;

/// Looks up graph in the module.
pub fn traverse_for_definition
(ast:ast::known::Module, id:&Id) -> FallibleResult<DefinitionInfo> {
    let err            = || CannotFindDefinition(id.clone());
    let mut crumb_iter = id.crumbs.iter();
    let first_crumb    = crumb_iter.next().ok_or(EmptyDefinitionId)?;
    let mut definition = ast.find_definition(first_crumb).ok_or_else(err)?;
    for crumb in crumb_iter {
        definition = definition.find_definition(crumb).ok_or_else(err)?;
    }
    Ok(definition)
}



// =================
// === GraphInfo ===
// =================

/// Description of the graph, based on information available in AST.
#[derive(Clone,Debug)]
pub struct GraphInfo {
    source : DefinitionInfo,
    /// Describes all known nodes in this graph (does not include special pseudo-nodes like graph
    /// inputs and outputs).
    pub nodes : Vec<node::NodeInfo>,
}

impl GraphInfo {
    /// Describe graph of the given definition.
    pub fn from_definition(source:DefinitionInfo) -> GraphInfo {
        let nodes = Self::from_function_binding(source.ast.clone());
        GraphInfo {source,nodes}
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

    use ast::HasRepr;
    use parser::api::IsParser;
    use wasm_bindgen_test::wasm_bindgen_test;

    wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

    /// Takes a program with main definition in root and returns main's graph.
    fn main_graph(parser:&mut impl IsParser, program:impl Str) -> GraphInfo {
        let module = parser.parse_module(program.into(), default()).unwrap();
        let name   = DefinitionName::new_plain("main");
        let main   = module.find_definition(&name).unwrap();
        GraphInfo::from_definition(main)
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
            assert_eq!(node.expression().repr(), "2+2");
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
    Int.= a = node
    node
";
        let graph = main_graph(&mut parser, program);
        assert_eq!(graph.nodes.len(), 2);
        for node in graph.nodes.iter() {
            assert_eq!(node.expression().repr(), "node");
        }
    }
}
