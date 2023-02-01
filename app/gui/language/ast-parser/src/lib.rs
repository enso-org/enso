// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_prelude::*;

use ast::prelude::FallibleResult;

mod translation;


// ==============
// === Export ===
// ==============

pub use ast::IdMap;
pub use parser_scala::DocParser;

pub mod api;


#[derive(Debug, Clone, CloneRef)]
pub struct Parser {
    parser: Rc<enso_parser::Parser>,
}


// === Core methods provided by the underlying parser ===

impl Parser {
    pub fn new() -> Self {
        let parser = Rc::new(enso_parser::Parser::new());
        Self { parser }
    }

    pub fn parse(&self, program: String, ids: IdMap) -> api::Result<ast::Ast> {
        let tree = self.parser.run(&program);
        Ok(translation::to_legacy_ast(&tree))
    }

    pub fn parse_with_metadata<M: api::Metadata>(
        &self,
        program: String,
    ) -> api::Result<api::ParsedSourceFile<M>> {
        let (code, meta) = enso_parser::metadata::extract(&program);
        let tree = self.parser.run(code);
        // TODO: Log errors.
        let metadata = meta.and_then(|meta| serde_json::from_str(meta).ok()).unwrap_or_default();
        let ast = ast::known::Module::try_from(translation::to_legacy_ast_module(&tree).unwrap()).unwrap();
        Ok(api::ParsedSourceFile { ast, metadata })
    }

    pub fn parse_module(&self, program: impl Str, ids: IdMap) -> api::Result<ast::known::Module> {
        let tree = self.parser.run(program.as_ref());
        let ast = translation::to_legacy_ast_module(&tree).unwrap();
        ast::known::Module::try_from(ast).map_err(|_| api::Error::NonModuleRoot)
    }
}


// === Convenience methods ===

impl Parser {
    pub fn parse_line_ast(&self, program: impl Str) -> FallibleResult<ast::Ast> {
        self.parse_line(program).map(|line| line.elem)
    }

    pub fn parse_line(&self, program: impl Str) -> FallibleResult<ast::BlockLine<ast::Ast>> {
        self.parse_line_with_id_map(program, default())
    }

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
    fn test_whole_file_repr() {
        let code = r#"
import Standard.Visualization
from Standard.Base import all
from Standard.Table import all
import Standard.Visualization
import Standard.Examples

main =
    operator2 = enso_project.data / 'store_data.xlsx'
    operator3 = operator2.read (Excel (Worksheet 'Customers'))
    operator4 = operator2.read (Excel (Worksheet 'Items'))
    operator5 = operator2.read (Excel (Worksheet 'Orders'))
    operator8 = operator5.join operator4  Join_Kind.Inner ['Item ID']
    operator1 = operator8.at 'Unit Price'
    operator9 = operator8.at 'Quantity'
    product1 = operator1 * operator9
    operator10 = operator8.set 'Order Value' product1
    operator11 = operator10.aggregate [Aggregate_Column.Group_By 'Customer ID', Aggregate_Column.Sum 'Order Value' 'Orders Value']
    operator16 = operator3.join operator11 Join_Kind.Inner ["Customer ID"]
    operator17 = operator16.order_by (Sort_Column.Name 'Orders Value' Sort_Direction.Descending)
"#;
        let ast = Parser::new().parse_module(code, default()).unwrap();
        assert_eq!(ast.repr(), code);
    }
}
