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
use lexpr::Value;



// ===========================
// === Test support macros ===
// ===========================

/// Parses input as a sequence of S-expressions, and wraps it in a `BodyBlock`.
macro_rules! block {
    ( $($statements:tt)* ) => {
        sexp![(BodyBlock #( $( $statements )* ) )]
    }
}



// =============
// === Tests ===
// =============

#[test]
fn nothing() {
    test("", block![()]);
}

#[test]
fn application() {
    test("a b c", block![(App (App (Ident a) (Ident b)) (Ident c))]);
}

#[test]
fn parentheses_simple() {
    let expected = block![
        (MultiSegmentApp #(((Symbol "(") (App (Ident a) (Ident b))) ((Symbol ")") ())))
    ];
    test("(a b)", expected);
}

#[test]
fn section_simple() {
    let expected_lhs = block![(OprSectionBoundary (OprApp () (Ok "+") (Ident a)))];
    test("+ a", expected_lhs);
    let expected_rhs = block![(OprSectionBoundary (OprApp (Ident a) (Ok "+") ()))];
    test("a +", expected_rhs);
}

#[test]
fn parentheses_nested() {
    #[rustfmt::skip]
    let expected = block![
        (MultiSegmentApp #(
         ((Symbol "(")
          (App (MultiSegmentApp #(((Symbol "(") (App (Ident a) (Ident b))) ((Symbol ")") ())))
               (Ident c)))
          ((Symbol ")") ())))
    ];
    test("((a b) c)", expected);
}

#[test]
fn comments() {
    // Basic, full-line comment.
    test("# a b c", block![(Comment "# a b c")]);
}


// === Type Definitions ===

#[test]
fn type_definition_no_body() {
    test("type Bool", block![(TypeDef (Ident type) (Ident Bool) #() #() #())]);
    test("type Option a", block![(TypeDef (Ident type) (Ident Option) #((Ident a)) #() #())]);
}

#[test]
fn type_constructors() {
    let code = [
        "type Geo",
        "    Circle",
        "        radius",
        "        4",
        "    Rectangle width height",
        "    Point",
    ];
    #[rustfmt::skip]
    let expected = block![
        (TypeDef (Ident type) (Ident Geo) #()
         #(((Circle #() #((Ident radius) (Number 4))))
           ((Rectangle #((Ident width) (Ident height)) #()))
           ((Point #() #())))
         #())
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn type_methods() {
    let code = ["type Geo", "    number =", "        23", "    area self = 1 + 1"];
    #[rustfmt::skip]
        let expected = block![
        (TypeDef (Ident type) (Ident Geo) #() #()
         #((Function number #() "=" (BodyBlock #((Number 23))))
           (Function area #((Ident self)) "=" (OprApp (Number 1) (Ok "+") (Number 1)))))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn type_def_full() {
    let code = [
        "type Geo",
        "    Circle",
        "        radius : float",
        "        4",
        "    Rectangle width height",
        "    Point",
        "",
        "    number =",
        "        23",
        "    area self = 1 + 1",
    ];
    #[rustfmt::skip]
    let expected = block![
        (TypeDef (Ident type) (Ident Geo) #()
         #(((Circle #() #((OprApp (Ident radius) (Ok ":") (Ident float)) (Number 4))))
           ((Rectangle #((Ident width) (Ident height)) #()))
           ((Point #() #()))
           (()))
         #((Function number #() "=" (BodyBlock #((Number 23))))
           (Function area #((Ident self)) "=" (OprApp (Number 1) (Ok "+") (Number 1)))))
    ];
    test(&code.join("\n"), expected);
}


// === Variable Assignment ===

#[test]
fn assignment_simple() {
    test("foo = 23", block![(Assignment (Ident foo) "=" (Number 23))]);
}


// === Functions ===

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


// === Code Blocks ===

#[test]
fn code_block_body() {
    let code = ["main =", "    4"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #((Number 4))))]);
    let code = ["main =", "      ", "    4"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #(() (Number 4))))]);
    let code = ["main =", "    ", "    4"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #(() (Number 4))))]);
    let code = ["main =", "  ", "    4"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #(() (Number 4))))]);
    let code = ["main =", "", "    4"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #(() (Number 4))))]);

    #[rustfmt::skip]
    let code = [
        "main =",
        "    +4",
        "    print 23",
    ];
    #[rustfmt::skip]
    let expect = block![
        (Function main #() "=" (BodyBlock #(
         (OprSectionBoundary (OprApp () (Ok "+") (Number 4)))
         (App (Ident print) (Number 23)))))
    ];
    test(&code.join("\n"), expect);
}

#[test]
fn code_block_operator() {
    let code = ["value = nums", "    * each random", "    + constant"];
    let expect = block![
        (Assignment (Ident value) "="
         (OperatorBlockApplication (Ident nums)
          #(((Ok "*") (App (Ident each) (Ident random)))
            ((Ok "+") (Ident constant)))
          #()))
    ];
    test(&code.join("\n"), expect);
}

#[test]
fn code_block_argument_list() {
    #[rustfmt::skip]
    let code = [
        "value = foo",
        "    bar",
    ];
    let expect = block![
        (Assignment (Ident value) "=" (ArgumentBlockApplication (Ident foo) #((Ident bar))))
    ];
    test(&code.join("\n"), expect);


    #[rustfmt::skip]
    let code = [
        "value = foo",
        "    +1",
        "    bar",
    ];
    #[rustfmt::skip]
    let expect = block![
        (Assignment (Ident value) "="
         (ArgumentBlockApplication (Ident foo) #(
          (OprSectionBoundary (OprApp () (Ok "+") (Number 1)))
          (Ident bar))))
    ];
    test(&code.join("\n"), expect);
}

#[test]
fn code_block_empty() {
    // The first line here should parse as a function with no body expression (which is an error).
    // No input would parse as an empty `ArgumentBlock` or `OperatorBlock`, because those types are
    // distinguished from a body continuation by the presence of non-empty indented lines.
    let code = ["foo =", "bar"];
    test(&code.join("\n"), block![(Function foo #() "=" ()) (Ident bar)]);
    // This parses similarly to above; a line with no non-whitespace content does not create a code
    // block.
    let code = ["foo =", "    ", "bar"];
    test(&code.join("\n"), block![(Function foo #() "=" ()) () (Ident bar)]);
}

#[test]
fn code_block_bad_indents1() {
    let code = ["main =", "  foo", " bar", "  baz"];
    let expected = block![
        (Function main #() "=" (BodyBlock #((Ident foo) (Ident bar) (Ident baz))))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn code_block_bad_indents2() {
    let code = ["main =", "  foo", " bar", "baz"];
    let expected = block![
        (Function main #() "=" (BodyBlock #((Ident foo) (Ident bar))))
        (Ident baz)
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn code_block_with_following_statement() {
    let code = ["main =", "    foo", "bar"];
    let expected = block![
        (Function main #() "=" (BodyBlock #((Ident foo))))
        (Ident bar)
    ];
    test(&code.join("\n"), expected);
}


// === Binary Operators ===

#[test]
fn multiple_operator_error() {
    let code = ["4 + + 1"];
    let expected = block![
        (OprApp (Number 4) (Err (#("+" "+"))) (Number 1))
    ];
    test(&code.join("\n"), expected);
    let code = ["4 + + + 1"];
    let expected = block![
        (OprApp (Number 4) (Err (#("+" "+" "+"))) (Number 1))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn precedence() {
    let code = ["1 * 2 + 3"];
    let expected = block![
        (OprApp (OprApp (Number 1) (Ok "*") (Number 2)) (Ok "+") (Number 3))
    ];
    test(&code.join("\n"), expected);
}


// === Unary Operators ===

#[test]
fn unevaluated_argument() {
    let code = ["main ~foo = 4"];
    let expected = block![
        (Function main #((UnaryOprApp "~" (Ident foo))) "=" (Number 4))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn unary_operator_missing_operand() {
    let code = ["main ~ = 4"];
    let expected = block![
        (Function main #((UnaryOprApp "~" ())) "=" (Number 4))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn unary_operator_at_end_of_expression() {
    let code = ["foo ~"];
    let expected = block![
        (App (Ident foo) (UnaryOprApp "~" ()))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn plus_negative() {
    let code = ["x = 4+-1"];
    let expected = block![
        (Assignment (Ident x) "=" (OprApp (Number 4) (Ok "+") (UnaryOprApp "-" (Number 1))))
    ];
    test(&code.join("\n"), expected);
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
/// - Rust's deserialization is compatible with Rust's serialization for the type. (The Java format
///   tests check Java's deserialization against Rust's deserialization).
///
/// The S-expression format is as documented for [`enso_metamodel_lexpr`], with some
/// postprocessing:
/// - For concision, field names are stripped (as if all structs were tuple structs).
/// - Most token types are represented as their contents, rather than as a token struct. For
///   example, a `token::Number` may be represented like: `sexp![10]`, and a `token::Ident` may look
///   like `sexp![foo]`.
fn test(code: &str, expect: Value) {
    let ast = enso_parser::Parser::new().run(code);
    let ast_s_expr = to_s_expr(&ast, code);
    assert_eq!(ast_s_expr.to_string(), expect.to_string(), "{:?}", &ast);
    assert_eq!(ast.code(), code, "{:?}", &ast);
    let serialized = enso_parser::serialization::serialize_tree(&ast).unwrap();
    let deserialized = enso_parser::serialization::deserialize_tree(&serialized);
    deserialized.unwrap();
}



// =====================
// === S-expressions ===
// =====================

/// Produce an S-expression representation of the input AST type.
pub fn to_s_expr<T>(value: &T, code: &str) -> Value
where T: serde::Serialize + Reflect {
    use enso_parser::syntax::token;
    use enso_parser::syntax::tree;
    let (graph, rust_to_meta) = enso_metamodel::rust::to_meta(value.reflect_type());
    let ast_ty = rust_to_meta[&value.reflect_type().id];
    let base = code.as_bytes().as_ptr() as usize;
    let code: Box<str> = Box::from(code);
    let mut to_s_expr = ToSExpr::new(&graph);
    to_s_expr.mapper(ast_ty, strip_hidden_fields);
    let ident_token = rust_to_meta[&token::variant::Ident::reflect().id];
    let comment_token = rust_to_meta[&token::variant::Comment::reflect().id];
    let operator_token = rust_to_meta[&token::variant::Operator::reflect().id];
    let symbol_token = rust_to_meta[&token::variant::Symbol::reflect().id];
    let number_token = rust_to_meta[&token::variant::Number::reflect().id];
    let newline_token = rust_to_meta[&token::variant::Newline::reflect().id];
    // TODO: Implement `#[reflect(flag = "enso::concrete")]`, which just attaches user data to the
    //  type info; then filter by flag here instead of hard-coding these simplifications.
    let line = rust_to_meta[&tree::block::Line::reflect().id];
    let operator_line = rust_to_meta[&tree::block::OperatorLine::reflect().id];
    let token_to_str = move |token: Value| {
        let range = token_code_range(&token, base);
        code[range].to_owned().into_boxed_str()
    };
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(ident_token, move |token| Value::symbol(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(comment_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(operator_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(symbol_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str;
    to_s_expr.mapper(number_token, move |token| {
        Value::Number(token_to_str_(token).parse::<u64>().unwrap().into())
    });
    let into_car = |cons| match cons {
        Value::Cons(cons) => cons.into_pair().0,
        _ => panic!(),
    };
    to_s_expr.mapper(line, into_car);
    to_s_expr.mapper(operator_line, into_car);
    to_s_expr.skip(newline_token);
    tuplify(to_s_expr.value(ast_ty, &value))
}

/// Strip certain fields that should be excluded from output.
fn strip_hidden_fields(tree: Value) -> Value {
    let hidden_tree_fields = [
        ":spanLeftOffsetVisible",
        ":spanLeftOffsetCodeReprBegin",
        ":spanLeftOffsetCodeReprLen",
        ":spanCodeLength",
    ];
    let hidden_tree_fields: HashSet<_> = hidden_tree_fields.into_iter().collect();
    Value::list(tree.to_vec().unwrap().into_iter().filter(|val| match val {
        Value::Cons(cons) => match cons.car() {
            Value::Symbol(symbol) => !hidden_tree_fields.contains(symbol.as_ref()),
            _ => panic!(),
        },
        _ => true,
    }))
}

/// Given an S-expression representation of a [`Token`] and the base address for `Code` `Cow`s,
/// return the range of the input code the token references.
fn token_code_range(token: &Value, base: usize) -> std::ops::Range<usize> {
    let get_u32 =
        |field| fields(token).find(|(name, _)| *name == field).unwrap().1.as_u64().unwrap() as u32;
    let begin = get_u32(":codeReprBegin");
    let len = get_u32(":codeReprLen");
    let begin = (begin as u64) | (base as u64 & !0xFFFF_FFFF);
    let begin = if begin < (base as u64) { begin + 0x1_0000_0000 } else { begin };
    let begin = begin as usize - base;
    let len = len as usize;
    begin..(begin + len)
}

/// Iterate the field `(name, value)` pairs of the S-expression of a struct with named fields.
fn fields(value: &'_ Value) -> impl Iterator<Item = (&'_ str, &'_ Value)> {
    value.list_iter().unwrap().filter_map(|value| match value {
        Value::Cons(cons) => match cons.car() {
            Value::Symbol(symbol) => Some((&symbol[..], cons.cdr())),
            _ => None,
        },
        _ => None,
    })
}

/// Strip field names from struct representations, so that they are printed more concisely, as if
/// they were tuple-structs.
fn tuplify(value: Value) -> Value {
    let (car, cdr) = match value {
        Value::Cons(cons) => cons.into_pair(),
        Value::Vector(mut vector) => {
            for value in vector.iter_mut() {
                let original = std::mem::replace(value, Value::Nil);
                *value = tuplify(original);
            }
            return Value::Vector(vector);
        }
        value => return value,
    };
    if let Value::Symbol(symbol) = &car {
        if let Some(':') = symbol.chars().next() {
            return tuplify(cdr);
        }
    }
    let car = tuplify(car);
    let cdr = tuplify(cdr);
    Value::Cons(lexpr::Cons::new(car, cdr))
}
