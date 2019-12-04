use prelude::*;
use parser::api::IsParser;

use ast::{Ast, Shape};

/// Takes Ast being a module with a single line and returns that line's AST.
fn expect_single_line(ast: &Ast) -> &Ast {
    let shape = ast.shape();
    let module: &ast::Module<Ast> = shape.try_into().unwrap();
    assert_eq!(module.lines.len(), 1, "module expected to have a single line");
    let line = module.lines.iter().nth(0).unwrap();
    line.elem.as_ref().unwrap()
}

/// "Downcasts" given AST's Shape to `T`.
fn expect_shape<'t, T>(ast: &'t Ast) -> &'t T
where &'t Shape<Ast>: TryInto<&'t T> {
    match ast.shape().try_into() {
        Ok(shape) => shape,
        _         => panic!("failed converting shape"),
    }
}

/// Persists parser (which is expensive to construct, so we want to reuse it
/// between tests. Additionally, hosts a number of helper methods.
struct TestHelper(parser::Parser);

impl TestHelper {
    fn new() -> TestHelper {
        TestHelper(parser::Parser::new_or_panic())
    }

    fn parse_line(&mut self, program: &str) -> Ast {
        let ast = self.0.parse(program.into()).unwrap();
        let line = expect_single_line(&ast);
        line.clone()
    }

    // TODO: make generic, should work for all shape subtypes.
    fn parse_shape_var<F>(&mut self, program: &str, tester: F)
        where F: FnOnce(&ast::Var) -> () {
        let ast = self.parse_line(program);
        let shape = expect_shape(&ast);
        tester(shape);
    }

    fn deserialize_blank(&mut self) {
        let _ast = self.parse_line("_");
    }

    fn deserialize_cons(&mut self) {
        let _ast = self.parse_line("FooBar");
    }

    fn deserialize_mod(&mut self) {
        let _ast = self.parse_line("+=");
    }

    fn deserialize_prefix(&mut self) {
        let _ast = self.parse_line("foo bar");
    }

    fn deserialize_infix(&mut self) {
        let _ast = self.parse_line("foo + bar");
    }

    fn deserialize_var(&mut self) {
        self.parse_shape_var("foo", |var| {
            let expected_var = ast::Var { name: "foo".into() };
            assert_eq!(var, &expected_var);
        });
    }

    fn run(&mut self) {
        self.deserialize_blank();
        self.deserialize_cons();
        self.deserialize_mod();
        self.deserialize_prefix();
        self.deserialize_infix();
        self.deserialize_var();
    }
}

/// A single entry point for all the tests here using external parser.
///
/// Setting up the parser is costly, so we run all tests as a single batch.
/// Until proper CI solution for calling external parser is devised, this
/// test is marked with `#[ignore]`.
#[test]
#[ignore]
fn parser_tests() {
    TestHelper::new().run()
}
