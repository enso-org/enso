//! Test support

use crate::check;
use crate::s_expr::to_s_expr;

use std::borrow::Cow;

/// Check that the given [`Tree`] is a valid representation of the given source code:
/// - Assert that the given [`Tree`] is composed of tokens that concatenate back to the given source
///   code.
/// - Assert that the given [`Tree`] can be serialized and deserialized without error.
fn expect_tree_representing_code(code: &str, ast: &enso_parser::syntax::Tree) {
    assert_eq!(ast.code(), code, "{:?}", &ast);
    let serialized = enso_parser::serialization::serialize_tree(ast).unwrap();
    let deserialized = enso_parser::serialization::deserialize_tree(&serialized);
    deserialized.unwrap();
}

// === Testing valid inputs ===

/// Given a block of input Enso code, test that:
/// - The given code parses to the AST represented by the given S-expression.
/// - The AST pretty-prints back to the original code.
/// - Rust's deserialization is compatible with Rust's serialization for the type. (The Java format
///   tests check Java's deserialization against Rust's deserialization).
///
/// The S-expression format is as documented for [`enso_metamodel_lexpr`], with some
/// postprocessing:
/// - For concision, field names are stripped (as if all structs were tuple structs).
/// - Most token types are represented as their contents, rather than as a token struct. For
///   example, a `token::Number` may be represented like: `sexp![10]`, and a `token::Ident` may look
///   like `sexp![foo]`.
fn test_module<T: AsRef<str>>(code: T, expect: lexpr::Value) {
    let code = code.as_ref();
    let ast = parse_module(code);
    let ast_repr = to_s_expr(&ast, code).to_string();
    assert_eq!(ast_repr, expect.to_string(), "{:?}", &ast);
}

/// Returns the S-expr representation of the given code, parsed as a complete module.
pub fn module<T: AsRef<str>>(code: T) -> String {
    let code = code.as_ref();
    to_s_expr(&parse_module(code), code).to_string()
}

/// Returns the S-expr representation of the given code, parsed in body block context.
pub fn block<T: AsRef<str>>(code: T) -> String {
    let code = code.as_ref();
    to_s_expr(&parse_block(code), code).to_string()
}

/// Parses the given code as a module, performs some extra checks, and returns the syntax tree.
pub fn parse_module(code: &'_ str) -> enso_parser::syntax::tree::Tree<'_> {
    let ast = enso_parser::Parser::new().parse_module(code);
    validate_parse(code, &ast);
    ast
}

/// Parses the given code as a body block, performs some extra checks, and returns the syntax tree.
pub fn parse_block(code: &'_ str) -> enso_parser::syntax::tree::Tree<'_> {
    let ast = enso_parser::Parser::new().parse_block(code);
    validate_parse(code, &ast);
    ast
}

fn validate_spans(code: &str, ast: &enso_parser::syntax::Tree) {
    let expected_span = 0..(code.encode_utf16().count() as u32);
    let mut locations = enso_parser::source::code::debug::LocationCheck::new();
    check::validate_spans(ast, expected_span, &mut locations).unwrap();
    locations.check(code);
}

fn validate_parse(code: &str, ast: &enso_parser::syntax::Tree) {
    validate_spans(code, ast);
    expect_tree_representing_code(code, ast);
}

// === Testing inputs containing syntax errors ===

#[derive(Debug, Eq, PartialEq, Default, Clone)]
struct Errors {
    invalid_node: Option<Cow<'static, str>>,
    multiple_operator: bool,
}

impl Errors {
    fn collect(ast: &enso_parser::syntax::Tree) -> Self {
        let mut errors = Errors::default();
        ast.visit_trees(|tree| match &tree.variant {
            enso_parser::syntax::tree::Variant::Invalid(invalid)
                if errors.invalid_node.is_none() =>
            {
                errors.invalid_node = Some(invalid.error.message.clone());
            }
            enso_parser::syntax::tree::Variant::OprApp(opr_app) if opr_app.opr.is_err() => {
                errors.multiple_operator = true;
            }
            _ => (),
        });
        errors
    }
}

/// Returns the message of the first `Invalid` node encountered in a preorder DFS.
pub fn first_error(ast: &enso_parser::syntax::Tree) -> Option<Cow<'static, str>> {
    let errors = Errors::collect(ast);
    errors.invalid_node
}

/// Checks that an input contains a multiple-operator error somewhere.
pub fn expect_multiple_operator_error(code: &str) {
    let ast = parse_module(code);
    let errors = Errors::collect(&ast);
    assert!(errors.multiple_operator || errors.invalid_node.is_some(), "{}", to_s_expr(&ast, code));
    assert!(errors.multiple_operator, "{:?}", ast);
}

/// Check that the input can be parsed, and doesn't yield any `Invalid` nodes.
pub fn expect_valid(code: &str) {
    let ast = parse_module(code);
    let errors = Errors::collect(&ast);
    assert_eq!(errors.invalid_node, None);
}
