//! Utilities for writing tests using parser. Should not be used in production parts.

use crate::prelude::*;

use crate::Parser;

use ast::test_utils::expect_shape;
use ast::test_utils::expect_single_line;
use ast::test_utils::validate_spans;
use ast::Ast;
use ast::HasRepr;
use ast::Shape;

/// Additional methods for parser to ease writing tests.
pub trait ParserTestExts {
    /// Program is expected to be a module with a single non-emty line. Its AST
    /// is reinterpret as given `Shape`.
    fn parse_shape<T>(&self, program: impl Str) -> T
    where
        for<'t> &'t Shape<Ast>: TryInto<&'t T>,
        T: Clone + 'static;

    /// Runs parser on given input, panics on any error.
    fn parse_testing(&self, program: impl Str) -> Ast;
}

impl ParserTestExts for Parser {
    fn parse_shape<T>(&self, program: impl Str) -> T
    where
        for<'t> &'t Shape<Ast>: TryInto<&'t T>,
        T: Clone + 'static, {
        let ast = self.parse_testing(program);
        let line = expect_single_line(&ast);
        let shape = expect_shape(line);
        shape.clone()
    }

    fn parse_testing(&self, program: impl Str) -> Ast {
        let program = program.into();
        DEBUG!("parsing " program);
        let ast = self.parse(program.clone(), default()).unwrap();
        assert_eq!(ast.shape().len().as_usize(), program.len());
        validate_spans(&ast);
        assert_eq!(ast.repr(), program, "{:?}", ast);
        ast
    }
}
