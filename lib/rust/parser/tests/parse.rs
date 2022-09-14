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



mod metadata;

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
    let expected = block![(Group "(" (App (Ident a) (Ident b)) ")")];
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
        (Group
         "("
         (App (Group "(" (App (Ident a) (Ident b)) ")")
              (Ident c))
         ")")];
    test("((a b) c)", expected);
}

#[test]
fn comments() {
    test("# a b c", block![() ()]);
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
        "        x",
        "    Rectangle width height",
        "    Point",
    ];
    #[rustfmt::skip]
    let expected = block![
        (TypeDef (Ident type) (Ident Geo) #()
         #(((Circle #() #((Ident radius) (Ident x))))
           ((Rectangle #((Ident width) (Ident height)) #()))
           ((Point #() #())))
         #())
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn type_methods() {
    let code = ["type Geo", "    number =", "        x", "    area self = x + x"];
    #[rustfmt::skip]
        let expected = block![
        (TypeDef (Ident type) (Ident Geo) #() #()
         #((Function number #() "=" (BodyBlock #((Ident x))))
           (Function area #((Ident self)) "=" (OprApp (Ident x) (Ok "+") (Ident x)))))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn type_def_full() {
    let code = [
        "type Geo",
        "    Circle",
        "        radius : float",
        "        x",
        "    Rectangle width height",
        "    Point",
        "",
        "    number =",
        "        x",
        "    area self = x + x",
    ];
    #[rustfmt::skip]
    let expected = block![
        (TypeDef (Ident type) (Ident Geo) #()
         #(((Circle #() #((TypeAnnotated (Ident radius) ":" (Ident float)) (Ident x))))
           ((Rectangle #((Ident width) (Ident height)) #()))
           ((Point #() #()))
           (()))
         #((Function number #() "=" (BodyBlock #((Ident x))))
           (Function area #((Ident self)) "=" (OprApp (Ident x) (Ok "+") (Ident x)))))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn type_def_nested() {
    #[rustfmt::skip]
    let code = [
        "type Foo",
        "    type Bar",
        "    type Baz",
    ];
    #[rustfmt::skip]
    let expected = block![
        (TypeDef (Ident type) (Ident Foo) #() #()
         #((TypeDef (Ident type) (Ident Bar) #() #() #())
           (TypeDef (Ident type) (Ident Baz) #() #() #())))
    ];
    test(&code.join("\n"), expected);
}


// === Variable Assignment ===

#[test]
fn assignment_simple() {
    test("foo = x", block![(Assignment (Ident foo) "=" (Ident x))]);
}


// === Functions ===

#[test]
fn function_inline_simple_args() {
    test("foo a = x", block![(Function foo #((Ident a)) "=" (Ident x))]);
    test("foo a b = x", block![(Function foo #((Ident a) (Ident b)) "=" (Ident x))]);
    test("foo a b c = x", block![(Function foo #((Ident a) (Ident b) (Ident c)) "=" (Ident x))]);
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
    let code = ["main =", "    x"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #((Ident x))))]);
    let code = ["main =", "      ", "    x"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #(() (Ident x))))]);
    let code = ["main =", "    ", "    x"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #(() (Ident x))))]);
    let code = ["main =", "  ", "    x"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #(() (Ident x))))]);
    let code = ["main =", "", "    x"];
    test(&code.join("\n"), block![(Function main #() "=" (BodyBlock #(() (Ident x))))]);

    #[rustfmt::skip]
    let code = [
        "main =",
        "    +x",
        "    print x",
    ];
    #[rustfmt::skip]
    let expect = block![
        (Function main #() "=" (BodyBlock #(
         (OprSectionBoundary (OprApp () (Ok "+") (Ident x)))
         (App (Ident print) (Ident x)))))
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
        "    +x",
        "    bar",
    ];
    #[rustfmt::skip]
    let expect = block![
        (Assignment (Ident value) "="
         (ArgumentBlockApplication (Ident foo) #(
          (OprSectionBoundary (OprApp () (Ok "+") (Ident x)))
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
    let code = ["x + + x"];
    let expected = block![
        (OprApp (Ident x) (Err (#("+" "+"))) (Ident x))
    ];
    test(&code.join("\n"), expected);
    let code = ["x + + + x"];
    let expected = block![
        (OprApp (Ident x) (Err (#("+" "+" "+"))) (Ident x))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn precedence() {
    let code = ["x * y + z"];
    let expected = block![
        (OprApp (OprApp (Ident x) (Ok "*") (Ident y)) (Ok "+") (Ident z))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn right_associative_operators() {
    let code = ["x --> y ---> z"];
    let expected = block![
        (OprApp (Ident x) (Ok "-->") (OprApp (Ident y) (Ok "--->") (Ident z)))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn pipeline_operators() {
    test("f <| a", block![(OprApp (Ident f) (Ok "<|") (Ident a))]);
    test("a |> f", block![(OprApp (Ident a) (Ok "|>") (Ident f))]);
}


// === Unary Operators ===

#[test]
fn unevaluated_argument() {
    let code = ["main ~foo = x"];
    let expected = block![
        (Function main #((UnaryOprApp "~" (Ident foo))) "=" (Ident x))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn unary_operator_missing_operand() {
    let code = ["main ~ = x"];
    let expected = block![
        (Function main #((UnaryOprApp "~" ())) "=" (Ident x))
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
    let code = ["x = x+-x"];
    let expected = block![
        (Assignment (Ident x) "=" (OprApp (Ident x) (Ok "+") (UnaryOprApp "-" (Ident x))))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn minus_binary() {
    let cases = [
        ("x - x", block![(OprApp (Ident x) (Ok "-") (Ident x))]),
        ("x-x", block![(OprApp (Ident x) (Ok "-") (Ident x))]),
        ("x.-y", block![(OprApp (Ident x) (Ok ".") (UnaryOprApp "-" (Ident y)))]),
        ("x.~y", block![(OprApp (Ident x) (Ok ".") (UnaryOprApp "~" (Ident y)))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn minus_section() {
    #[rustfmt::skip]
    let cases = [
        ("- x", block![(OprSectionBoundary (OprApp () (Ok "-") (Ident x)))]),
        ("(- x)", block![(Group "(" (OprSectionBoundary (OprApp () (Ok "-") (Ident x))) ")")]),
        ("- (x * x)", block![
            (OprSectionBoundary (OprApp () (Ok "-")
             (Group "(" (OprApp (Ident x) (Ok "*") (Ident x)) ")")))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn minus_unary() {
    #[rustfmt::skip]
    let cases = [
        ("f -x", block![(App (Ident f) (UnaryOprApp "-" (Ident x)))]),
        ("-x", block![(UnaryOprApp "-" (Ident x))]),
        ("(-x)", block![(Group "(" (UnaryOprApp "-" (Ident x)) ")")]),
        ("-(x * x)", block![
            (UnaryOprApp "-" (Group "(" (OprApp (Ident x) (Ok "*") (Ident x)) ")"))]),
        ("x=-x", block![(Assignment (Ident x) "=" (UnaryOprApp "-" (Ident x)))]),
        ("-x+x", block![(OprApp (UnaryOprApp "-" (Ident x)) (Ok "+") (Ident x))]),
        ("-x*x", block![(OprApp (UnaryOprApp "-" (Ident x)) (Ok "*") (Ident x))]),
        ("-1.x", block![(OprApp (UnaryOprApp "-" (Number () "1" ())) (Ok ".") (Ident x))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Import/Export ===

#[test]
fn import() {
    #[rustfmt::skip]
    let cases = [
        ("import project.IO", block![
            (Import () () () ((Ident import) (OprApp (Ident project) (Ok ".") (Ident IO))) () ())]),
        ("import Standard.Base as Enso_List", block![
            (Import () () ()
             ((Ident import) (OprApp (Ident Standard) (Ok ".") (Ident Base)))
             ((Ident as) (Ident Enso_List))
             ())]),
        ("from Standard.Base import all", block![
            (Import ()
             ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Base)))
             ()
             ((Ident import) (Ident all))
             () ())]),
        ("from Standard.Base import all hiding Number, Boolean", block![
            (Import ()
             ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Base)))
             ()
             ((Ident import) (Ident all))
             ()
             ((Ident hiding) (OprApp (Ident Number) (Ok ",") (Ident Boolean))))]),
        ("from Standard.Table as Column_Module import Column", block![
            (Import ()
             ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Table)))
             ((Ident as) (Ident Column_Module))
             ((Ident import) (Ident Column))
             () ())]),
        ("polyglot java import java.lang.Float", block![
            (Import
             ((Ident polyglot) (Ident java))
             ()
             ()
             ((Ident import)
              (OprApp (OprApp (Ident java) (Ok ".") (Ident lang)) (Ok ".") (Ident Float)))
             () ())]),
        ("polyglot java import java.net.URI as Java_URI", block![
            (Import
             ((Ident polyglot) (Ident java))
             ()
             ()
             ((Ident import)
              (OprApp (OprApp (Ident java) (Ok ".") (Ident net)) (Ok ".") (Ident URI)))
             ((Ident as) (Ident Java_URI))
             ())]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn export() {
    #[rustfmt::skip]
    let cases = [
        ("export prj.Data.Foo", block![
            (Export () ()
             ((Ident export)
              (OprApp (OprApp (Ident prj) (Ok ".") (Ident Data)) (Ok ".") (Ident Foo)))
             () ())]),
        ("export Foo as Bar", block![
            (Export () () ((Ident export) (Ident Foo)) ((Ident as) (Ident Bar)) ())]),
        ("from Foo export Bar, Baz", block![
            (Export
             ((Ident from) (Ident Foo))
             ()
             ((Ident export) (OprApp (Ident Bar) (Ok ",") (Ident Baz)))
             () ())]),
        ("from Foo export all hiding Bar, Baz", block![
            (Export
             ((Ident from) (Ident Foo))
             ()
             ((Ident export) (Ident all))
             ()
             ((Ident hiding) (OprApp (Ident Bar) (Ok ",") (Ident Baz))))]),
        ("from Foo as Bar export Baz, Quux", block![
            (Export
             ((Ident from) (Ident Foo))
             ((Ident as) (Ident Bar))
             ((Ident export) (OprApp (Ident Baz) (Ok ",") (Ident Quux)))
             () ())
        ]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Metadata ===


#[test]
fn metadata_raw() {
    let code = [
        "x",
        "#### METADATA ####",
        r#"[[{"index":{"value":7},"size":{"value":8}},"5bad897e-099b-4b00-9348-64092636746d"]]"#,
    ];
    let code = code.join("\n");
    let (_meta, code) = enso_parser::metadata::parse(&code).unwrap();
    let expected = block![
        (Ident x)
        ()
    ];
    test(code, expected);
}

#[test]
fn metadata_parsing() {
    let code = metadata::ORDERS_WITH_METADATA;
    let (meta, code) = enso_parser::metadata::parse(code).unwrap();
    let _ast = enso_parser::Parser::new().run(code);
    let _meta: enso_parser::metadata::Metadata = meta.unwrap();
}


// === Type annotations and signatures ===

#[test]
fn type_signatures() {
    let cases = [
        ("val : Bool", block![(TypeSignature val ":" (Ident Bool))]),
        ("val : List Int", block![(TypeSignature val ":" (App (Ident List) (Ident Int)))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn type_annotations() {
    #[rustfmt::skip]
    let cases = [
        ("val = x : Int", block![
            (Assignment (Ident val) "=" (TypeAnnotated (Ident x) ":" (Ident Int)))]),
        ("val = foo (x : Int)", block![
            (Assignment (Ident val) "="
             (App (Ident foo)
              (Group "(" (TypeAnnotated (Ident x) ":" (Ident Int)) ")")))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Text Literals ===

#[test]
fn inline_text_literals() {
    #[rustfmt::skip]
    let cases = [
        (r#""I'm an inline raw text!""#, block![
            (TextLiteral "\"" #((Section "I'm an inline raw text!")) "\"" 0)]),
        (r#"zero_length = """#, block![
            (Assignment (Ident zero_length) "=" (TextLiteral "\"" #() "\"" 0))]),
        (r#"unclosed = ""#, block![(Assignment (Ident unclosed) "=" (TextLiteral "\"" #() () 0))]),
        (r#"unclosed = "a"#, block![
            (Assignment (Ident unclosed) "=" (TextLiteral "\"" #((Section "a")) () 0))]),
        (r#"'Other quote type'"#, block![(TextLiteral "'" #((Section "Other quote type")) "'" 0)]),
        (r#""Non-escape: \n""#, block![(TextLiteral "\"" #((Section "Non-escape: \\n")) "\"" 0)]),
        (r#""String with \" escape""#, block![
            (TextLiteral
             "\""
             #((Section "String with ") (Escape "\\") (Section "\" escape"))
             "\"" 0)]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn multiline_text_literals() {
    test("'''", block![(TextLiteral "'''" #() () 0)]);
    const CODE: &str = r#"'''
    part of the string
       3-spaces indented line, part of the Text Block
    this does not end the string -> '''

    also part of the string

x"#;
    #[rustfmt::skip]
    let expected = block![
        (TextLiteral
         "'''"
         #((Section "\n") (Section "part of the string")
           (Section "\n") (Section "3-spaces indented line, part of the Text Block")
           (Section "\n") (Section "this does not end the string -> '''")
           (Section "\n") (Section "")
           (Section "\n") (Section "also part of the string")
           (Section "\n") (Section ""))
        () 4)
        (Ident x)
    ];
    test(CODE, expected);
}


// === Lambdas ===

#[test]
fn lambdas() {
    let cases = [
        ("\\v -> v", block![(Lambda "\\" (Arrow #((Ident v)) "->" (Ident v)))]),
        ("\\a b -> x", block![(Lambda "\\" (Arrow #((Ident a) (Ident b)) "->" (Ident x)))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Pattern Matching ===

#[test]
fn pattern_irrefutable() {
    let code = "Point x_val = my_point";
    let expected = block![(Assignment (App (Ident Point) (Ident x_val)) "=" (Ident my_point))];
    test(code, expected);

    let code = "Point _ = my_point";
    let expected = block![(Assignment (App (Ident Point) (Wildcard)) "=" (Ident my_point))];
    test(code, expected);
}

#[test]
fn case_expression() {
    #[rustfmt::skip]
    let code = [
        "case a of",
        "    Some -> x",
        "    Int ->",
    ];
    #[rustfmt::skip]
    let expected = block![
        (Case (Ident a) () #(
         (Arrow #((Ident Some)) "->" (Ident x))
         (Arrow #((Ident Int)) "->" ())))
    ];
    test(&code.join("\n"), expected);

    #[rustfmt::skip]
    let code = [
        "case a of",
        "    Vector_2d x y -> x",
    ];
    #[rustfmt::skip]
    let expected = block![
        (Case (Ident a) () #((Arrow #((Ident Vector_2d) (Ident x) (Ident y)) "->" (Ident x))))
    ];
    test(&code.join("\n"), expected);

    #[rustfmt::skip]
    let code = [
        "case self of",
        "    Vector_2d -> x",
        "    _ -> x",
    ];
    #[rustfmt::skip]
    let expected = block![
        (Case (Ident self) () #(
         (Arrow #((Ident Vector_2d)) "->" (Ident x))
         (Arrow #((Wildcard)) "->" (Ident x))))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn pattern_match_auto_scope() {
    #[rustfmt::skip]
    let code = [
        "case self of",
        "    Vector_2d ... -> x",
    ];
    #[rustfmt::skip]
    let expected = block![
        (Case (Ident self) () #((Arrow #((Ident Vector_2d) (AutoScope)) "->" (Ident x))))
    ];
    test(&code.join("\n"), expected);
}


// === Array/tuple literals ===

#[test]
fn array_literals() {
    let cases = [
        ("[]", block![(Array "[" () #() "]")]),
        ("[x]", block![(Array "[" (Ident x) #() "]")]),
        ("[x, y]", block![(Array "[" (Ident x) #(("," (Ident y))) "]")]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn tuple_literals() {
    let cases = [
        ("{}", block![(Tuple "{" () #() "}")]),
        ("{x}", block![(Tuple "{" (Ident x) #() "}")]),
        ("{x, y}", block![(Tuple "{" (Ident x) #(("," (Ident y))) "}")]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Numeric literals ===

#[test]
fn numbers() {
    let cases = [
        ("100_000", block![(Number () "100_000" ())]),
        ("10_000.99", block![(Number () "10_000" ("." "99"))]),
        ("0b10101010", block![(Number "0b" "10101010" ())]),
        ("0o122137", block![(Number "0o" "122137" ())]),
        ("0xAE2F14", block![(Number "0x" "AE2F14" ())]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
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
    let operator_token = rust_to_meta[&token::variant::Operator::reflect().id];
    let symbol_token = rust_to_meta[&token::variant::Symbol::reflect().id];
    let number_token = rust_to_meta[&token::variant::Digits::reflect().id];
    let number_base_token = rust_to_meta[&token::variant::NumberBase::reflect().id];
    let newline_token = rust_to_meta[&token::variant::Newline::reflect().id];
    let text_start_token = rust_to_meta[&token::variant::TextStart::reflect().id];
    let text_end_token = rust_to_meta[&token::variant::TextEnd::reflect().id];
    let text_escape_token = rust_to_meta[&token::variant::TextEscape::reflect().id];
    let text_section_token = rust_to_meta[&token::variant::TextSection::reflect().id];
    let wildcard_token = rust_to_meta[&token::variant::Wildcard::reflect().id];
    let autoscope_token = rust_to_meta[&token::variant::AutoScope::reflect().id];
    // TODO: Implement `#[reflect(flag = "enso::concrete")]`, which just attaches user data to the
    //  type info; then filter by flag here instead of hard-coding these simplifications.
    let token_to_str = move |token: Value| {
        let range = token_code_range(&token, base);
        code[range].to_owned().into_boxed_str()
    };
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(ident_token, move |token| Value::symbol(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(operator_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(symbol_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(text_start_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(text_end_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(text_escape_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(text_section_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str.clone();
    to_s_expr.mapper(number_token, move |token| Value::string(token_to_str_(token)));
    let token_to_str_ = token_to_str;
    to_s_expr.mapper(number_base_token, move |token| Value::string(token_to_str_(token)));
    let into_car = |cons| match cons {
        Value::Cons(cons) => cons.into_pair().0,
        _ => panic!(),
    };
    let simplify_case = |list| {
        let list = strip_hidden_fields(list);
        let (_, list) = match list {
            Value::Cons(cons) => cons.into_pair(),
            _ => panic!(),
        };
        let (expression, list) = match list {
            Value::Cons(cons) => cons.into_pair(),
            _ => panic!(),
        };
        let (_, list) = match list {
            Value::Cons(cons) => cons.into_pair(),
            _ => panic!(),
        };
        Value::cons(expression, list)
    };
    let line = rust_to_meta[&tree::block::Line::reflect().id];
    let operator_line = rust_to_meta[&tree::block::OperatorLine::reflect().id];
    let case = rust_to_meta[&tree::Case::reflect().id];
    to_s_expr.mapper(line, into_car);
    to_s_expr.mapper(operator_line, into_car);
    to_s_expr.mapper(case, simplify_case);
    to_s_expr.skip(newline_token);
    to_s_expr.skip(wildcard_token);
    to_s_expr.skip(autoscope_token);
    tuplify(to_s_expr.value(ast_ty, &value))
}

/// Strip certain fields that should be excluded from output.
fn strip_hidden_fields(tree: Value) -> Value {
    let hidden_tree_fields = [
        ":spanLeftOffsetVisible",
        ":spanLeftOffsetCodeReprBegin",
        ":spanLeftOffsetCodeReprLen",
        ":spanLeftOffsetCodeUtf16",
        ":spanCodeLengthUtf8",
        ":spanCodeLengthUtf16",
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
