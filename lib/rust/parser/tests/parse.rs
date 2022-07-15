//! Parse expressions and compare their results to expected values.

// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(non_ascii_idents)]
#![deny(unconditional_recursion)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use lexpr::sexp;



// ===========================
// === Test support macros ===
// ===========================

/// Parses input as a sequence of S-expressions, and wraps it in a `Block`.
macro_rules! block {
    ( $statements:tt ) => {
        sexp![(Block #($statements))]
    }
}



// =============
// === Tests ===
// =============

#[test]
fn application() {
    test("a b c", block![(App (App (Ident a) (Ident b)) (Ident c))]);
}

#[test]
fn type_definition_bool() {
    test("type Bool", block![(TypeDef (Ident type) (Ident Bool) #())]);
}

#[test]
fn type_definition_option() {
    test("type Option a", block![(TypeDef (Ident type) (Ident Option) #((Ident a)))]);
}

#[test]
fn assignment_simple() {
    test("foo = 23", block![(Assignment (Ident foo) "=" (Number 23))]);
}

#[test]
fn function_inline_simple_args() {
    test("foo a = 23", block![(Function foo #((Ident a)) "=" (Number 23))]);
    test("foo a b = 23", block![(Function foo #((Ident a) (Ident b)) "=" (Number 23))]);
    test("foo a b c = 23", block![(Function foo #((Ident a) (Ident b) (Ident c)) "=" (Number 23))]);
}

#[test]
fn function_block_noargs() {
    test("foo =", block![(Function foo #() "=" ())]);
}

#[test]
fn function_block_simple_args() {
    test("foo a =", block![(Function foo #((Ident a)) "=" ())]);
    test("foo a b =", block![(Function foo #((Ident a) (Ident b)) "=" ())]);
    test("foo a b c =", block![(Function foo #((Ident a) (Ident b) (Ident c)) "=" ())]);
}



// ====================
// === Test Support ===
// ====================

use enso_metamodel_lexpr::ToSExpr;
use enso_reflect::Reflect;
use std::collections::HashSet;

/// Given a block of input Enso code, test that:
/// - The given code parses to the AST represented by the given S-expression.
/// - The AST pretty-prints back to the original code.
///
/// The S-expression format is as documented for [`enso_metamodel_lexpr`], with some
/// postprocessing:
/// - For concision, field names are stripped (as if all structs were tuple structs).
/// - Most token types are represented as their contents, rather than as a token struct. For
///   example, a `token::Number` may be represented like: `sexp![10]`, and a `token::Ident` may look
///   like `sexp![foo]`.
fn test(code: &str, expect: lexpr::Value) {
    let ast = enso_parser::Parser::new().run(code);
    let ast_s_expr = to_s_expr(&ast, code);
    assert_eq!(ast_s_expr.to_string(), expect.to_string());
    assert_eq!(ast.code(), code);
}



// =====================
// === S-expressions ===
// =====================

/// Produce an S-expression representation of the input AST type.
pub fn to_s_expr<T>(value: &T, code: &str) -> lexpr::Value
where T: serde::Serialize + Reflect {
    let (graph, rust_to_meta) = enso_metamodel::rust::to_meta(value.reflect_type());
    let ast_ty = rust_to_meta[&value.reflect_type().id];
    let base = code.as_bytes().as_ptr() as usize;
    let code: Box<str> = Box::from(code);
    let mut to_s_expr = ToSExpr::new(&graph);
    to_s_expr.mapper(ast_ty, strip_hidden_fields);
    let ident_token = rust_to_meta[&enso_parser::syntax::token::variant::Ident::reflect().id];
    let operator_token = rust_to_meta[&enso_parser::syntax::token::variant::Operator::reflect().id];
    let number_token = rust_to_meta[&enso_parser::syntax::token::variant::Number::reflect().id];
    let token_to_str = move |token: lexpr::Value| {
        let range = token_code_range(&token, base);
        code[range].to_owned().into_boxed_str()
    };
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(ident_token, move |token| lexpr::Value::symbol(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(operator_token, move |token| lexpr::Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str;
    to_s_expr.mapper(number_token, move |token| {
        lexpr::Value::Number(token_to_str_(token).parse::<u64>().unwrap().into())
    });
    tuplify(to_s_expr.value(ast_ty, &value))
}

/// Strip certain fields that should be excluded from output.
fn strip_hidden_fields(tree: lexpr::Value) -> lexpr::Value {
    let hidden_tree_fields =
        [":spanLeftOffsetVisible", ":spanLeftOffsetCodeRepr", ":spanCodeLength"];
    let hidden_tree_fields: HashSet<_> = hidden_tree_fields.into_iter().collect();
    lexpr::Value::list(tree.to_vec().unwrap().into_iter().filter(|val| match val {
        lexpr::Value::Cons(cons) => match cons.car() {
            lexpr::Value::Symbol(symbol) => !hidden_tree_fields.contains(symbol.as_ref()),
            _ => panic!(),
        },
        _ => true,
    }))
}

/// Given an S-expression representation of a [`Token`] and the base address for `Code` `Cow`s,
/// return the range of the input code the token references.
fn token_code_range(token: &lexpr::Value, base: usize) -> std::ops::Range<usize> {
    let code_repr = fields(token).find(|(name, _)| *name == ":codeRepr").unwrap().1;
    let mut begin = None;
    let mut len = None;
    for (name, value) in fields(code_repr) {
        match name {
            ":begin" => begin = Some(value.as_u64().unwrap() as u32),
            ":len" => len = Some(value.as_u64().unwrap() as u32),
            _ => (),
        }
    }
    let begin = begin.unwrap();
    let begin = (begin as u64) | (base as u64 & !0xFFFF_FFFF);
    let begin = if begin < (base as u64) { begin + 0x1_0000_0000 } else { begin };
    let begin = begin as usize - base;
    let len = len.unwrap() as usize;
    begin..(begin + len)
}

/// Iterate the field `(name, value)` pairs of the S-expression of a struct with named fields.
fn fields(value: &'_ lexpr::Value) -> impl Iterator<Item = (&'_ str, &'_ lexpr::Value)> {
    value.list_iter().unwrap().filter_map(|value| match value {
        lexpr::Value::Cons(cons) => match cons.car() {
            lexpr::Value::Symbol(symbol) => Some((&symbol[..], cons.cdr())),
            _ => None,
        },
        _ => None,
    })
}

/// Strip field names from struct representations, so that they are printed more concisely, as if
/// they were tuple-structs.
fn tuplify(value: lexpr::Value) -> lexpr::Value {
    let (car, cdr) = match value {
        lexpr::Value::Cons(cons) => cons.into_pair(),
        lexpr::Value::Vector(mut vector) => {
            for value in vector.iter_mut() {
                let original = std::mem::replace(value, lexpr::Value::Nil);
                *value = tuplify(original);
            }
            return lexpr::Value::Vector(vector);
        }
        value => return value,
    };
    if let lexpr::Value::Symbol(symbol) = &car {
        if let Some(':') = symbol.chars().next() {
            return tuplify(cdr);
        }
    }
    let car = tuplify(car);
    let cdr = tuplify(cdr);
    lexpr::Value::Cons(lexpr::Cons::new(car, cdr))
}
