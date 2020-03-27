//! Utilities for writing tests using parser. Should not be used in production parts.

use crate::prelude::*;

use crate::api::IsParser;

use ast::Ast;
use ast::Shape;
use ast::HasLength;
use ast::HasRepr;
use ast::test_utils::expect_single_line;
use ast::test_utils::expect_shape;
use ast::test_utils::validate_spans;

/// Additional methods for parser to ease writing tests.
pub trait ParserTestExts : IsParser {
    /// Program is expected to be a module with a single non-emty line. Its AST
    /// is reinterpret as given `Shape`.
    fn parse_shape<T>(&mut self, program:impl Str) -> T
        where for<'t>  &'t Shape<Ast>: TryInto<&'t T>,
              T             : Clone + 'static {
        let ast  = self.parse_testing(program);
        let line = expect_single_line(&ast);
        let shape = expect_shape(line);
        shape.clone()
    }

    /// Runs parser on given input, panics on any error.
    fn parse_testing(&mut self, program:impl Str) -> Ast {
        let program = program.into();
        println!("parsing {}", program);
        let ast = self.parse(program.clone(), default()).unwrap();
        assert_eq!(ast.shape().len(), program.len());
        validate_spans(&ast);
        assert_eq!(ast.repr(), program, "{:?}", ast);
        ast
    }
}

impl<T:IsParser> ParserTestExts for T {}
