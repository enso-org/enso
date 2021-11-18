//! Module with logic for node collapsing.
//!
//! See the [`collapse`] function for details.

use crate::prelude::*;

use crate::connection::Connection;
use crate::connection::Endpoint;
use crate::definition;
use crate::definition::DefinitionInfo;
use crate::graph::GraphInfo;
use crate::identifier::Identifier;
use crate::node;
use crate::node::MainLine;
use crate::node::NodeInfo;

use ast::constants::keywords::HERE;
use ast::crumbs::Located;
use ast::BlockLine;
use parser::Parser;
use std::collections::BTreeSet;



// ====================
// === Collapse API ===
// ====================

// === Entry point ===

/// Run the "collapse node" refactoring. Generates output describing how to apply the refactoring.
///
/// "Collapsing nodes" means extracting a number of selected nodes from a graph into a new sibling
/// method definition. In place of them a new node shall be placed that invokes the method.
///
/// Any connections incoming into the extracted nodes shall be translated into the method arguments.
/// Any connections leaving the extracted nodes shall be treated as function outputs. Currently only
/// one output is supported, so an extracted function can return at most one unique identifier.
pub fn collapse(
    graph: &GraphInfo,
    selected_nodes: impl IntoIterator<Item = node::Id>,
    name: Identifier,
    parser: &Parser,
) -> FallibleResult<Collapsed> {
    Collapser::new(graph.clone(), selected_nodes, parser.clone_ref())?.collapse(name)
}


// === Collapsed ===

/// Result of running node collapse algorithm. Describes update to the refactored definition.
#[derive(Clone, Debug)]
pub struct Collapsed {
    /// New contents of the refactored definition.
    pub updated_definition: DefinitionInfo,
    /// Contents of the new definition that should be placed next to the refactored one.
    pub new_method:         definition::ToAdd,
    /// Identifier of the collapsed node in the updated definition.
    pub collapsed_node:     node::Id,
}


// === Errors ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
#[fail(display = "At least one node must be selected for collapsing refactoring.")]
pub struct NoNodesSelected;

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
#[fail(
    display = "Internal refactoring error: Endpoint refers to node {} that cannot be resolved.",
    _0
)]
pub struct CannotResolveEndpointNode(node::Id);

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
#[fail(display = "Internal refactoring error: Cannot generate collapsed node description.")]
pub struct CannotConstructCollapsedNode;

#[allow(missing_docs)]
#[derive(Clone, Debug, Fail)]
#[fail(display = "Internal refactoring error: Cannot resolve identifier for the endpoint {:?}", _0)]
pub struct EndpointIdentifierCannotBeResolved(Endpoint);

#[allow(missing_docs)]
#[derive(Clone, Debug, Fail)]
#[fail(
    display = "Currently collapsing nodes is supported only when there would be at most one \
output from the collapsed function. Found more than one output: `{}` and `{}`.",
    _0, _1
)]
pub struct MultipleOutputIdentifiers(String, String);



// ===================
// === GraphHelper ===
// ===================

/// Helper that stores the refactored graph information and provides methods for its processing.
#[derive(Clone, Debug)]
pub struct GraphHelper {
    /// The graph of definition where the node collapsing takes place.
    info:  GraphInfo,
    /// All the nodes in the graph. Cached for performance.
    nodes: Vec<NodeInfo>,
}

impl GraphHelper {
    /// Create a helper for the given graph.
    pub fn new(graph: GraphInfo) -> Self {
        GraphHelper { nodes: graph.nodes(), info: graph }
    }

    /// Get the information about node described byt the given ID.
    pub fn lookup_node(&self, id: node::Id) -> FallibleResult<&NodeInfo> {
        let err = CannotResolveEndpointNode(id).into();
        self.nodes.iter().find(|node| node.id() == id).ok_or(err)
    }

    /// Get the identifier constituting a connection's endpoint.
    pub fn endpoint_identifier(&self, endpoint: &Endpoint) -> FallibleResult<Identifier> {
        let node = self.lookup_node(endpoint.node)?;
        let err = || EndpointIdentifierCannotBeResolved(endpoint.clone()).into();
        let endpoint_ast = node.ast().get_traversing(&endpoint.crumbs)?.clone_ref();
        Identifier::new(endpoint_ast).ok_or_else(err)
    }

    /// Get the variable form of the identifier for the given connection.
    pub fn connection_variable(&self, connection: &Connection) -> FallibleResult<Identifier> {
        self.endpoint_identifier(&connection.source)
    }

    /// Rewrite lines of the refactored definition by calling given functor for each line.
    pub fn rewrite_definition(
        &self,
        line_rewriter: impl Fn(&BlockLine<Option<Ast>>) -> FallibleResult<LineDisposition>,
    ) -> FallibleResult<DefinitionInfo> {
        let mut updated_definition = self.info.source.clone();
        let mut new_lines = Vec::new();
        for line in updated_definition.block_lines() {
            match line_rewriter(&line)? {
                LineDisposition::Keep => new_lines.push(line),
                LineDisposition::Remove => {}
                LineDisposition::Replace(ast) => new_lines.push(BlockLine::new(Some(ast))),
            }
        }
        updated_definition.set_block_lines(new_lines)?;
        Ok(updated_definition)
    }
}



// =================
// === Output ===
// =================

/// Describes the output of the extracted function.
#[derive(Clone, Debug)]
pub struct Output {
    /// The node that introduces output variable.
    pub node:       node::Id,
    /// The identifier from the extracted nodes that is used outside.
    pub identifier: Identifier,
}



// =================
// === Extracted ===
// =================

/// Describes the nodes to be extracted into a new definition by collapsing.
#[derive(Clone, Debug)]
pub struct Extracted {
    /// Identifiers used in the collapsed nodes from the outside scope.
    inputs:          Vec<Identifier>,
    /// Information on node that will act as extracted function output.
    /// Currently we allow at most one output, to be revisited in the future.
    output:          Option<Output>,
    /// Nodes that are being collapsed and extracted into a separate method.
    extracted_nodes: Vec<NodeInfo>,
}

impl Extracted {
    /// Collect the extracted node information.
    pub fn new(
        graph: &GraphHelper,
        selected_nodes: impl IntoIterator<Item = node::Id>,
    ) -> FallibleResult<Self> {
        let extracted_nodes_set: HashSet<_> = selected_nodes.into_iter().collect();
        let extracted_nodes: Vec<_> = graph
            .nodes
            .iter()
            .filter(|node| extracted_nodes_set.contains(&node.id()))
            .cloned()
            .collect();

        // Leaf is an extracted node that has no outgoing connections.
        let mut leaves = extracted_nodes_set.clone();
        let mut inputs = Vec::new();
        let mut output = None;
        for connection in graph.info.connections() {
            let starts_inside = extracted_nodes_set.contains(&connection.source.node);
            let ends_inside = extracted_nodes_set.contains(&connection.destination.node);
            let identifier = graph.connection_variable(&connection)?;

            leaves.remove(&connection.source.node);
            if !starts_inside && ends_inside {
                inputs.push(identifier)
            } else if starts_inside && !ends_inside {
                match output {
                    Some(Output { identifier: previous_identifier, .. })
                        if identifier != previous_identifier =>
                    {
                        let ident1 = identifier.to_string();
                        let ident2 = previous_identifier.to_string();
                        return Err(MultipleOutputIdentifiers(ident1, ident2).into());
                    }
                    Some(_) => {} // Ignore duplicate usage of the same identifier.
                    None => {
                        let node = connection.source.node;
                        output = Some(Output { node, identifier });
                    }
                }
            }
        }

        // If there is no output found so far, it means that none of our nodes is used outside
        // the extracted function. In such we will return value from arbitrarily chosen leaf.
        output = output.or_else(|| {
            let output_leaf_id = leaves.into_iter().next()?;
            let output_node = extracted_nodes.iter().find(|node| node.id() == output_leaf_id)?;
            let identifier = Identifier::new(output_node.pattern()?.clone_ref())?;
            let node = output_node.id();
            Some(Output { node, identifier })
        });

        Ok(Self { inputs, output, extracted_nodes })
    }

    /// Check if the given line belongs to the selection (i.e. is extracted into a new method).
    pub fn belongs_to_selection(&self, line_ast: &Ast) -> bool {
        self.extracted_nodes.iter().any(|extracted_node| extracted_node.contains_line(line_ast))
    }

    /// Generate AST of a line that needs to be appended to the extracted nodes' Asts.
    /// None if there is no such need.
    pub fn return_line(&self) -> Option<Ast> {
        // To return value we just utter its identifier. But the expression needs a new ID.
        self.output.as_ref().map(|out| out.identifier.with_new_id().into())
    }

    /// Generate the description for the new method's definition with the extracted nodes.
    pub fn generate(&self, name: Identifier) -> definition::ToAdd {
        let name = definition::DefinitionName::new_plain(name);
        let inputs = self.inputs.iter().collect::<BTreeSet<_>>();
        let return_line = self.return_line();
        let mut selected_nodes_iter = self.extracted_nodes.iter().map(|node| node.ast().clone());
        let body_head = selected_nodes_iter.next().unwrap();
        let body_tail = selected_nodes_iter.chain(return_line).map(Some).collect();
        let explicit_parameter_names = inputs.iter().map(|input| input.name().into()).collect();
        definition::ToAdd { name, explicit_parameter_names, body_head, body_tail }
    }
}



// =================
// === Collapser ===
// =================

/// Collapser rewrites the refactored definition line-by-line. This enum describes action to be
/// taken for a given line.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum LineDisposition {
    Keep,
    Remove,
    Replace(Ast),
}

/// Helper type that stores some common data used for collapsing algorithm and implements its logic.
#[derive(Clone, Debug)]
pub struct Collapser {
    /// The graph of definition where the node collapsing takes place.
    graph:          GraphHelper,
    /// Information about nodes that are extracted into a separate definition.
    extracted:      Extracted,
    /// Which node from the refactored graph should be replaced with a call to a extracted method.
    /// This only exists because we care about this node line's position (not its state).
    replaced_node:  node::Id,
    parser:         Parser,
    /// Identifier of the node to be introduced as a result of collapsing.
    collapsed_node: node::Id,
}

impl Collapser {
    /// Does some early pre-processing and gathers common data used in various parts of the
    /// refactoring algorithm.
    pub fn new(
        graph: GraphInfo,
        selected_nodes: impl IntoIterator<Item = node::Id>,
        parser: Parser,
    ) -> FallibleResult<Self> {
        let graph = GraphHelper::new(graph);
        let extracted = Extracted::new(&graph, selected_nodes)?;
        let last_selected = extracted.extracted_nodes.iter().last().ok_or(NoNodesSelected)?.id();
        let replaced_node = extracted.output.as_ref().map(|out| out.node).unwrap_or(last_selected);
        let collapsed_node = node::Id::new_v4();
        Ok(Collapser { graph, extracted, replaced_node, parser, collapsed_node })
    }

    /// Generate the expression that calls the extracted method definition.
    ///
    /// Does not include any pattern for assigning the resulting value.
    pub fn call_to_extracted(&self, extracted: &definition::ToAdd) -> FallibleResult<Ast> {
        // TODO actually check that generated name is single-identifier
        let mut target = extracted.name.clone();
        target.extended_target.insert(0, Located::new_root(HERE.to_string()));
        let base = target.ast(&self.parser)?;
        let args = extracted.explicit_parameter_names.iter().map(Ast::var);
        let chain = ast::prefix::Chain::new(base, args);
        Ok(chain.into_ast())
    }

    /// Assign to a line from refactored definition one of 3 dispositions:
    /// 1) Lines that are kept intact -- not belonging to selected nodes;
    /// 2) Lines that are extracted and removed -- all selected nodes, except:
    /// 3) Line that introduces output of the extracted function (if present at all) -> its
    ///    expression shall be replaced with a call to the extracted function.
    ///    If there is no usage of the extracted function output, its invocation should be placed
    ///    in place of the last extracted line.
    pub fn rewrite_line(
        &self,
        line: &BlockLine<Option<Ast>>,
        extracted_definition: &definition::ToAdd,
    ) -> FallibleResult<LineDisposition> {
        let ast = match line.elem.as_ref() {
            // We leave lines without nodes (blank lines) intact.
            None => return Ok(LineDisposition::Keep),
            Some(ast) => ast,
        };
        if !self.extracted.belongs_to_selection(ast) {
            Ok(LineDisposition::Keep)
        } else if MainLine::from_ast(ast).contains_if(|n| n.id() == self.replaced_node) {
            let no_node_err = failure::Error::from(CannotConstructCollapsedNode);
            let expression_ast = self.call_to_extracted(extracted_definition)?;
            let expression = MainLine::from_ast(&expression_ast).ok_or(no_node_err)?;
            let mut new_node = NodeInfo { documentation: None, main_line: expression };
            new_node.set_id(self.collapsed_node);
            if let Some(Output { identifier, .. }) = &self.extracted.output {
                new_node.set_pattern(identifier.with_new_id().into())
            }
            Ok(LineDisposition::Replace(new_node.ast().clone_ref()))
        } else {
            Ok(LineDisposition::Remove)
        }
    }

    /// Run the collapsing refactoring on this input.
    pub fn collapse(&self, name: Identifier) -> FallibleResult<Collapsed> {
        let new_method = self.extracted.generate(name);
        let updated_definition =
            self.graph.rewrite_definition(|line| self.rewrite_line(line, &new_method))?;
        let collapsed_node = self.collapsed_node;
        Ok(Collapsed { updated_definition, new_method, collapsed_node })
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::definition::DefinitionName;
    use crate::graph;
    use crate::module;

    use ast::crumbs::Crumb;

    struct Case {
        refactored_name:     DefinitionName,
        introduced_name:     Identifier,
        initial_method_code: &'static str,
        extracted_lines:     Range<usize>,
        expected_generated:  &'static str,
        expected_refactored: &'static str,
    }

    impl Case {
        fn run(&self, parser: &Parser) {
            let logger = Logger::new("Collapsing_Test");
            let ast = parser.parse_module(self.initial_method_code, default()).unwrap();
            let main = module::locate_child(&ast, &self.refactored_name).unwrap();
            let graph = graph::GraphInfo::from_definition(main.item.clone());
            let nodes = graph.nodes();
            let run_internal = |selection: &Vec<node::Id>| {
                ast::test_utils::assert_unique_ids(ast.as_ref());
                let selection = selection.iter().copied();
                let new_name = self.introduced_name.clone();
                let collapsed = collapse(&graph, selection, new_name, parser).unwrap();
                let new_method = collapsed.new_method.ast(0, parser).unwrap();
                let placement = module::Placement::Before(self.refactored_name.clone());
                let new_main = &collapsed.updated_definition.ast;
                info!(logger, "Generated method:\n{new_method}");
                info!(logger, "Updated method:\n{new_method}");
                let mut module = module::Info { ast: ast.clone_ref() };
                let main_crumb = Crumb::from(main.crumb());
                module.ast = module.ast.set(&main_crumb, new_main.ast().clone()).unwrap();
                module.add_method(collapsed.new_method, placement, parser).unwrap();
                ast::test_utils::assert_unique_ids(&module.ast.as_ref());
                info!(logger, "Updated method:\n{&module.ast}");
                assert_eq!(new_method.repr(), self.expected_generated);
                assert_eq!(new_main.repr(), self.expected_refactored);
            };

            let extracted_lines = self.extracted_lines.clone();
            // We run case twice, with reversed node selection order. This way we assure that test
            // isn't passing just because it got selected nodes in some specific order.
            // The refactoring is expected to behave the same, no matter what the order of selected
            // nodes is.
            let mut selected_nodes =
                nodes[extracted_lines].iter().map(|node| node.id()).collect_vec();
            run_internal(&selected_nodes);
            selected_nodes.reverse();
            run_internal(&selected_nodes);
        }
    }

    #[allow(unused_parens)] // False warning.
    #[wasm_bindgen_test]
    fn test_collapse() {
        let parser = Parser::new_or_panic();
        let introduced_name = Identifier::try_from("custom_new").unwrap();
        let refactored_name = DefinitionName::new_plain("custom_old");
        let initial_method_code = r"custom_old =
    a = 1
    b = 2
    c = A + B
    d = a + b
    c + 7";
        let extracted_lines = 1..4;
        let expected_generated = r"custom_new a =
    b = 2
    c = A + B
    d = a + b
    c";
        let expected_refactored = r"custom_old =
    a = 1
    c = here.custom_new a
    c + 7";

        let mut case = Case {
            refactored_name,
            introduced_name,
            initial_method_code,
            extracted_lines,
            expected_generated,
            expected_refactored,
        };
        case.run(&parser);

        // Check that refactoring a single assignment line:
        // 1) Maintains the assignment and the introduced name for the value in the extracted
        //    method;
        // 2) Extracted method returns the same value as extracted node;
        // 3) That invocation appears in the extracted node's place and maintains assignment.
        case.extracted_lines = 3..4;
        case.expected_generated = r"custom_new a b =
    d = a + b
    d";
        case.expected_refactored = r"custom_old =
    a = 1
    b = 2
    c = A + B
    d = here.custom_new a b
    c + 7";
        case.run(&parser);

        // Check that when refactoring a single non-assignment line:
        // 1) the single extracted expression is an inline body of the generated method;
        // 2) the invocation appears in the extracted node's place but has no assignment.
        case.initial_method_code = r"custom_old =
    a = 1
    b = 2
    c = A + B
    a + b
    c + 7";
        case.extracted_lines = 3..4;
        case.expected_generated = r"custom_new a b = a + b";
        case.expected_refactored = r"custom_old =
    a = 1
    b = 2
    c = A + B
    here.custom_new a b
    c + 7";
        case.run(&parser);

        // Check that:
        // 1) method with no arguments can be extracted;
        // 2) method with result used multiple times can be extracted.
        // 3) identifiers not defined in the refactored method (`d`) are not made into parameters.
        case.initial_method_code = r"custom_old =
    c = 50 + d
    c + c + 10";
        case.extracted_lines = 0..1;
        case.expected_generated = r"custom_new =
    c = 50 + d
    c";
        case.expected_refactored = r"custom_old =
    c = here.custom_new
    c + c + 10";
        case.run(&parser);


        // Case reported in https://github.com/enso-org/ide/issues/1234
        case.initial_method_code = r"custom_old =
    number1 = 1
    number2 = 2
    range = number1.up_to number2
    vector = range.to_vector";
        case.extracted_lines = 2..4;
        case.expected_generated = r"custom_new number1 number2 =
    range = number1.up_to number2
    vector = range.to_vector
    vector";
        case.expected_refactored = r"custom_old =
    number1 = 1
    number2 = 2
    vector = here.custom_new number1 number2";
        case.run(&parser);
    }
}
