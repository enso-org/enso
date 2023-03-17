//! [`Parser`] adapts a [`enso_syntax::Parser`] to produce the [`ast::AST`]/[`span_tree`]
//! representation used by the Graph Editor.

// === Features ===
#![feature(extend_one)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(assert_matches)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]

use enso_prelude::*;
use enso_profiler::prelude::*;

use ast::prelude::FallibleResult;
use ast::HasIdMap;
use ast::IdMap;
use enso_profiler as profiler;



mod translation;



// ==============
// === Export ===
// ==============

pub mod api;



// ==============
// === Parser ===
// ==============

/// Parses Enso syntax.
#[derive(Debug, Default, Clone, CloneRef)]
pub struct Parser {
    parser: Rc<enso_parser::Parser>,
}


// === Core methods provided by the underlying parser ===

impl Parser {
    /// Create a new parser.
    pub fn new() -> Self {
        let parser = Rc::new(enso_parser::Parser::new());
        Self { parser }
    }

    /// Parse the given source code with the specified ID map.
    #[profile(Task)]
    pub fn parse(&self, program: impl Str, ids: IdMap) -> ast::Ast {
        let tree = self.parser.run(program.as_ref());
        let ids = ids
            .vec
            .into_iter()
            .map(|(range, id)| ((range.start.value, range.end.value), id))
            .collect();
        translation::tree_to_ast(&tree, ids)
    }

    /// Parse the given source code, using metadata (including ID map) found in the input string.
    #[profile(Task)]
    pub fn parse_with_metadata<M: api::Metadata>(
        &self,
        program: impl Str,
    ) -> api::ParsedSourceFile<M> {
        let (code, meta) = enso_parser::metadata::extract(program.as_ref());
        if meta.is_none() {
            info!("parse_with_metadata: No metadata found.");
        }
        let meta_lines = meta.and_then(|meta| meta.split_once('\n'));
        if meta.is_some() && meta_lines.is_none() {
            warn!("parse_with_metadata: Expected two lines of metadata.");
        }
        let ids = meta_lines.map(|lines| lines.0);
        let application_metadata = meta_lines.map(|lines| lines.1);
        let ids = enso_parser::metadata::parse_metadata(ids.unwrap_or_default());
        if ids.is_none() {
            warn!("parse_with_metadata: Failed to parse ID map.");
        }
        let ids = ids
            .unwrap_or_default()
            .iter()
            .map(|((start, len), id)| ((*start, start + len), uuid::Uuid::from_u128(id.as_u128())))
            .collect();
        let tree = self.parser.run(code);
        let metadata = application_metadata.and_then(|meta| serde_json::from_str(meta).ok());
        if application_metadata.is_some() && metadata.is_none() {
            warn!("parse_with_metadata: Failed to deserialize metadata.");
        }
        let ast = translation::tree_to_ast(&tree, ids);
        let id_map = ast.id_map();
        let ast = ast::known::Module::try_from(ast).unwrap();
        let mut metadata: M = metadata.unwrap_or_default();
        metadata.prune_unused_ids(&id_map);
        api::ParsedSourceFile { ast, metadata }
    }
}


// === Convenience methods ===

impl Parser {
    /// Parse the given source code as a module, and return a [`ast::known::Module`].
    pub fn parse_module(&self, program: impl Str, ids: IdMap) -> api::Result<ast::known::Module> {
        let ast = self.parse(program.as_ref(), ids);
        ast::known::Module::try_from(ast).map_err(|_| api::Error::NonModuleRoot)
    }

    /// Parse the given line of source code, and return just the [`ast::Ast`].
    pub fn parse_line_ast(&self, program: impl Str) -> FallibleResult<ast::Ast> {
        self.parse_line(program).map(|line| line.elem)
    }

    /// Parse the given line of source code.
    pub fn parse_line(&self, program: impl Str) -> FallibleResult<ast::BlockLine<ast::Ast>> {
        self.parse_line_with_id_map(program, default())
    }

    /// Parse the given line of source code, attaching the given IDs.
    pub fn parse_line_ast_with_id_map(
        &self,
        program: impl Str,
        id_map: IdMap,
    ) -> FallibleResult<ast::Ast> {
        self.parse_line_with_id_map(program, id_map).map(|line| line.elem)
    }

    /// Program is expected to be single non-empty line module. Return the parsed line.
    fn parse_line_with_id_map(
        &self,
        program: impl Str,
        id_map: IdMap,
    ) -> FallibleResult<ast::BlockLine<ast::Ast>> {
        let module = self.parse_module(program, id_map)?;
        let mut lines =
            module.lines.clone().into_iter().filter_map(|line| line.map(|elem| elem).transpose());
        if let Some(first_non_empty_line) = lines.next() {
            if lines.next().is_some() {
                Err(api::TooManyLinesProduced.into())
            } else {
                Ok(first_non_empty_line)
            }
        } else {
            Err(api::NoLinesProduced.into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::HasRepr;
    use std::assert_matches::assert_matches;

    #[test]
    fn test_group_repr() {
        let code = "bar (Foo (a b))";
        let ast = Parser::new().parse_line_ast(code).unwrap();
        assert_eq!(ast.repr(), code);
    }

    #[test]
    fn test_text_repr() {
        let code = "operator17 = operator16.order_by (Sort_Column.Name 'Orders Value' Sort_Direction.Descending)";
        let ast = Parser::new().parse_line_ast(code).unwrap();
        assert_eq!(ast.repr(), code);
    }

    #[test]
    fn test_orders_repr() {
        let code = r#"
from Standard.Base import all
from Standard.Table import all
import Standard.Visualization
import Standard.Examples

main =
    ## The file contains three different sheets relating to operations of an
       online store.
    operator2 = enso_project.data / 'store_data.xlsx'
    ## Read the customers table.
    operator3 = operator2.read (Excel (Worksheet 'Customers'))
    ## Read the products table.
    operator4 = operator2.read (Excel (Worksheet 'Items'))
    ## Read the orders history.
    operator5 = operator2.read (Excel (Worksheet 'Orders'))
    ## Join the item data to the order history, to get information on item
       prices in the orders table.
    operator8 = operator5.join operator4  Join_Kind.Inner ['Item ID']
    operator1 = operator8.at 'Unit Price'
    operator9 = operator8.at 'Quantity'
    ## Multiply item prices and counts to get total order value.
    product1 = operator1 * operator9
    operator10 = operator8.set 'Order Value' product1
    ## Group all orders by the Customer ID, to compute the total value of orders
       placed by each client.
    operator11 = operator10.aggregate [Aggregate_Column.Group_By 'Customer ID', Aggregate_Column.Sum 'Order Value' 'Orders Value']
    ## Join the customer data into orders table, to include names in the final
       ranking.
    operator16 = operator3.join operator11 Join_Kind.Inner ["Customer ID"]
    ## Sort the customers by their lifetime value, with the most valuable
       customers at the start of the table.
    operator17 = operator16.order_by (Sort_Column.Name 'Orders Value' Sort_Direction.Descending)
"#;
        let ast = Parser::new().parse_module(code, default()).unwrap();
        assert_eq!(ast.repr(), code);
    }

    #[test]
    fn test_as_lambda() {
        let ast = Parser::new().parse_line_ast("a->4").unwrap();
        assert!(ast::macros::as_lambda(&ast).is_some(), "{ast:?}");
    }

    #[test]
    fn test_negative_number() {
        let ast = Parser::new().parse_line_ast("-23").unwrap();
        assert_matches!(ast.shape(), ast::Shape::Number(_));
    }
}
