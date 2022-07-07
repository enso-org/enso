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
#[ignore]
fn type_definition_bool() {
    #[rustfmt::skip]
    let bool = [
        "type Bool",
        "    False",
        "    True",
    ];
    let bool_ast = block![()];
    test(&bool.join("\n"), bool_ast);
}

#[test]
#[ignore]
fn type_definition_geo() {
    let geo = [
        "type Geo",
        "    Circle",
        "    radius : Float",
        "    Rectangle",
        "    width  : Float",
        "    height : Float",
    ];
    let geo_ast = block![()];
    test(&geo.join("\n"), geo_ast);
}

#[test]
#[ignore]
fn type_definition_option() {
    #[rustfmt::skip]
    let option = [
        "type Option a",
        "    Some elem:a",
        "    None",
    ];
    let option_ast = block![()];
    test(&option.join("\n"), option_ast);
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

use enso_metamodel::meta;
use enso_reflect::Reflect;
use std::collections::HashSet;

/// Given a block of input Enso code, test that:
/// - The given code parses to the AST represented by the given S-expression.
/// - The AST pretty-prints back to the original code.
///
/// The S-expression format is as documented for [`enso_metamodel::meta::sexp`], with some
/// postprocessing:
/// - For concision, field names are stripped (as if all structs were tuple structs).
/// - Most token types are represented as their contents, rather than as a token struct. For
///   example, a `token::Number` may be represented like: `sexp![10]`, and a `token::Ident` may look
///   like `sexp![foo]`.
fn test(code: &str, expect: lexpr::Value) {
    let ast = enso_parser::Parser::new().run(code);
    let ast_sexp = to_sexp(&ast, code);
    assert_eq!(ast_sexp.to_string(), expect.to_string());
    assert_eq!(ast.code(), code);
}



// =====================
// === S-expressions ===
// =====================

/// Produce an S-expression representation of the input AST type.
pub fn to_sexp<T>(value: &T, code: &str) -> lexpr::Value
where T: serde::Serialize + Reflect {
    use bincode::Options;
    let bincoder = bincode::DefaultOptions::new().with_fixint_encoding();
    let value_ = bincoder.serialize(&value).unwrap();

    let (graph, rust_to_meta) = enso_metamodel::rust::to_meta(value.reflect_type());
    let ast_ty = rust_to_meta[&value.reflect_type().id];

    let base = code.as_bytes().as_ptr() as usize;
    let code: Box<str> = Box::from(code);
    let mut value_ = &value_[..];
    let mut to_sexp = meta::ToSexp::new(&graph);
    let hidden_tree_fields = ["spanLeftOffsetVisible", "spanLeftOffsetCodeRepr", "spanCodeLength"];
    let hidden_tree_fields: HashSet<_> = hidden_tree_fields.into_iter().collect();
    to_sexp.mapper(ast_ty, move |tree| {
        lexpr::Value::list(tree.to_vec().unwrap().into_iter().filter(|val| match val {
            lexpr::Value::Cons(cons) => match cons.car() {
                lexpr::Value::Symbol(symbol) => !hidden_tree_fields.contains(symbol.as_ref()),
                _ => panic!(),
            },
            _ => true,
        }))
    });
    let ident_token = rust_to_meta[&enso_parser::syntax::token::variant::Ident::reflect().id];
    let operator_token = rust_to_meta[&enso_parser::syntax::token::variant::Operator::reflect().id];
    let number_token = rust_to_meta[&enso_parser::syntax::token::variant::Number::reflect().id];
    let token_to_str = move |token: lexpr::Value| {
        let code_repr = fields(&token).find(|(name, _)| *name == "codeRepr").unwrap().1;
        let mut begin = None;
        let mut len = None;
        for (name, value) in fields(code_repr) {
            match name {
                "begin" => begin = Some(value.as_u64().unwrap() as u32),
                "len" => len = Some(value.as_u64().unwrap() as u32),
                _ => (),
            }
        }
        let begin = begin.unwrap();
        let begin = (begin as u64) | (base as u64 & !0xFFFF_FFFF);
        let begin = if begin < (base as u64) { begin + 0x1_0000_0000 } else { begin };
        let begin = begin as usize - base;
        let len = len.unwrap() as usize;
        code[begin..(begin + len)].to_owned().into_boxed_str()
    };
    let token_to_str_ = token_to_str.clone();
    to_sexp.mapper(ident_token, move |token| lexpr::Value::symbol(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_sexp.mapper(operator_token, move |token| lexpr::Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str;
    to_sexp.mapper(number_token, move |token| {
        lexpr::Value::Number(token_to_str_(token).parse::<u64>().unwrap().into())
    });

    let sexp = to_sexp.value(ast_ty, &mut value_);
    let sexp = tuplify(sexp);
    assert_eq!(value_, &[0; 0], "{}", sexp);
    sexp
}

fn fields(value: &'_ lexpr::Value) -> impl Iterator<Item = (&'_ str, &'_ lexpr::Value)> {
    value.list_iter().unwrap().filter_map(|value| match value {
        lexpr::Value::Cons(cons) => match cons.car() {
            lexpr::Value::Symbol(symbol) => Some((symbol.as_ref(), cons.cdr())),
            _ => None,
        },
        _ => None,
    })
}

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
        if let Some(c) = symbol.chars().next() {
            if c.is_ascii_lowercase() {
                return tuplify(cdr);
            }
        }
    }
    let car = tuplify(car);
    let cdr = tuplify(cdr);
    lexpr::Value::Cons(lexpr::Cons::new(car, cdr))
}
