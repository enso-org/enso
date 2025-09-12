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

use enso_parser_debug::test::block;
use enso_parser_debug::test::expect_invalid_node;
use enso_parser_debug::test::expect_multiple_operator_error;
use enso_parser_debug::test::expect_valid;
use enso_parser_debug::test::module;
use enso_parser_debug::test::parse;
use insta::assert_snapshot;



// ===========================
// === Test support macros ===
// ===========================

macro_rules! test_module {
    ( $code:expr, @$expected:tt ) => {
        assert_snapshot!(module($code), @$expected)
    }
}

macro_rules! test_block {
    ( $code:expr, @$expected:tt ) => {
        assert_snapshot!(block($code), @$expected)
    }
}



// ================================
// === Language Construct Tests ===
// ================================

#[test]
fn nothing() {
    test_module!("", @"(BodyBlock #(()))");
}

#[test]
fn application() {
    test_block!("a b c",
        @"(BodyBlock #((ExpressionStatement () (App (App (Ident a) (Ident b)) (Ident c)))))");
}

#[test]
fn parentheses() {
    test_block!("(a b)",
        @"(BodyBlock #((ExpressionStatement () (Group (App (Ident a) (Ident b))))))");
    expect_invalid_node("x)");
    test_block!("(x", @"(BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("(a) (b)",
        @"(BodyBlock #((ExpressionStatement () (App (Group (Ident a)) (Group (Ident b))))))");
    test_block!("((a b) c)",
        @"(BodyBlock #((ExpressionStatement () (Group (App (Group (App (Ident a) (Ident b))) (Ident c))))))");
    test_block!("(a).b",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Group (Ident a)) (Ok ".") (Ident b)))))"#);
}

#[test]
fn section_simple() {
    test_block!("+ a",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp () (Ok "+") (Ident a))))))"#);
    test_block!("a +",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp (Ident a) (Ok "+") ())))))"#);
}

#[test]
fn inline_if() {
    test_block!("if True then True else False",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident if) (Ident True)) ((Ident then) (Ident True)) ((Ident else) (Ident False)))))))");
}

#[test]
fn then_block() {
    test_block!("if True then\n True",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident if) (Ident True)) ((Ident then) (BodyBlock #((ExpressionStatement () (Ident True))))))))))");
}

#[test]
fn else_block() {
    test_block!("if True then True else\n False",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident if) (Ident True)) ((Ident then) (Ident True)) ((Ident else) (BodyBlock #((ExpressionStatement () (Ident False))))))))))");
}

#[test]
fn if_then_else_chained_block() {
    test_block!("if True then True else False\n    . to_text",
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (MultiSegmentApp #(((Ident if) (Ident True)) ((Ident then) (Ident True)) ((Ident else) (Ident False)))) #(((Ok ".") (Ident to_text))) #()))))"#);
    test_block!("(if True then True else False)\n    . to_text",
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (Group (MultiSegmentApp #(((Ident if) (Ident True)) ((Ident then) (Ident True)) ((Ident else) (Ident False))))) #(((Ok ".") (Ident to_text))) #()))))"#);
    test_block!("if True then True else False\n    . to_text\n    . as_value",
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (MultiSegmentApp #(((Ident if) (Ident True)) ((Ident then) (Ident True)) ((Ident else) (Ident False)))) #(((Ok ".") (Ident to_text)) ((Ok ".") (Ident as_value))) #()))))"#);
    test_block!("if True then True else False\n    . to_text\n    . as_value\n    . done 42",
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (MultiSegmentApp #(((Ident if) (Ident True)) ((Ident then) (Ident True)) ((Ident else) (Ident False)))) #(((Ok ".") (Ident to_text)) ((Ok ".") (Ident as_value)) ((Ok ".") (App (Ident done) (Number () "42" ())))) #()))))"#);
}

// === Comments ===

#[test]
fn plain_comments() {
    test_module!("# a b c", @"(BodyBlock #(() ()))");
    test_block!("# a b c", @"(BodyBlock #(() ()))");
    test_module!("main = # define main\n 4",
        @r#"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #(() (ExpressionStatement () (Number () "4" ())))))))"#);
}

#[test]
fn function_documentation() {
    test_module!([
            "## The Identity Function",
            "",
            "   Arguments:",
            "   - x: value to do nothing to",
            "id x = x",
        ].join("\n"),
        @r#"(BodyBlock #((Function ((#((Section " The Identity Function") (Newline) (Newline) (Section "Arguments:") (Newline) (Section "- x: value to do nothing to"))) #(())) #() () () (Ident id) #((() (Ident x) () ())) () (Ident x))))"#);
    test_module!(&["type Foo", " ## Test indent handling", "  ", " foo bar = foo"].join("\n"),
        @r#"(BodyBlock #((TypeDef Foo #() #((Function ((#((Section " Test indent handling"))) #(() ())) #() () () (Ident foo) #((() (Ident bar) () ())) () (Ident foo))))))"#);
    expect_invalid_node("expression ## unexpected doc comment on same line");
}

#[test]
fn expression_documentation() {
    test_block!("## The value of x\nx",
        @r#"(BodyBlock #((ExpressionStatement ((#((Section " The value of x"))) #(())) (Ident x))))"#);
}

#[test]
fn unused_documentation() {
    test_module!("## First docs\n## More docs\n\n## More docs after a gap",
        @r#"(BodyBlock #((Documentation (#((Section " First docs")))) (Documentation (#((Section " More docs")))) () (Documentation (#((Section " More docs after a gap"))))))"#);
}


// === Type Definitions ===

#[test]
fn type_definition_no_body() {
    test_module!("type Bool", @"(BodyBlock #((TypeDef Bool #() #())))");
    test_module!("type Option a", @"(BodyBlock #((TypeDef Option #((() (Ident a) () ())) #())))");
    test_module!("type Option (a)", @"(BodyBlock #((TypeDef Option #((() (Ident a) () ())) #())))");
    test_module!("type Foo (a : Int)",
        @r#"(BodyBlock #((TypeDef Foo #((() (Ident a) (":" (Ident Int)) ())) #())))"#);
    test_module!("type A a=0",
        @r#"(BodyBlock #((TypeDef A #((() (Ident a) () ((Number () "0" ())))) #())))"#);
    test_module!("type Existing_Headers (column_names : Vector Text)",
        @r#"(BodyBlock #((TypeDef Existing_Headers #((() (Ident column_names) (":" (App (Ident Vector) (Ident Text))) ())) #())))"#);
    test_module!("type 1", @"(BodyBlock #((Invalid)))");
}

#[test]
fn type_constructors() {
    test_module!([
            "type Geo",
            "    Circle",
            "        radius",
            "        x",
            "    Rectangle width height",
            "    Point",
        ].join("\n"),
        @"(BodyBlock #((TypeDef Geo #() #((ConstructorDefinition () #() () Circle #() #(((() (Ident radius) () ())) ((() (Ident x) () ())))) (ConstructorDefinition () #() () Rectangle #((() (Ident width) () ()) (() (Ident height) () ())) #()) (ConstructorDefinition () #() () Point #() #())))))");
    test_module!("type Foo\n Bar (a : B = C.D)",
        @r#"(BodyBlock #((TypeDef Foo #() #((ConstructorDefinition () #() () Bar #((() (Ident a) (":" (Ident B)) ((OprApp (Ident C) (Ok ".") (Ident D))))) #())))))"#);
    test_module!(["type A", "    Foo (a : Integer, b : Integer)"].join("\n"),
        @r#"(BodyBlock #((TypeDef A #() #((ConstructorDefinition () #() () Foo #((() (Ident a) (":" (Invalid)) ())) #())))))"#);
}

#[test]
fn type_constructor_documentation() {
    test_module!("type Foo\n ## Bar\n Baz",
        @r#"(BodyBlock #((TypeDef Foo #() #((ConstructorDefinition ((#((Section " Bar"))) #(())) #() () Baz #() #())))))"#);
}

#[test]
fn type_constructor_private() {
    test_module!(["type Foo", "    private Bar"].join("\n"),
        @"(BodyBlock #((TypeDef Foo #() #((ConstructorDefinition () #() private Bar #() #())))))");
    test_module!(["type Foo", "    private Bar", "    Foo"].join("\n"),
        @"(BodyBlock #((TypeDef Foo #() #((ConstructorDefinition () #() private Bar #() #()) (ConstructorDefinition () #() () Foo #() #())))))");
    test_module!([ "type Geo",
            "    private Circle",
            "        radius",
            "        x",
            "    Rectangle width height",
            "    Point",
        ].join("\n"),
        @"(BodyBlock #((TypeDef Geo #() #((ConstructorDefinition () #() private Circle #() #(((() (Ident radius) () ())) ((() (Ident x) () ())))) (ConstructorDefinition () #() () Rectangle #((() (Ident width) () ()) (() (Ident height) () ())) #()) (ConstructorDefinition () #() () Point #() #())))))");
    test_module!(["type My_Type", "    private Value a b c"].join("\n"),
        @"(BodyBlock #((TypeDef My_Type #() #((ConstructorDefinition () #() private Value #((() (Ident a) () ()) (() (Ident b) () ()) (() (Ident c) () ())) #())))))");
}

#[test]
fn type_methods() {
    test_module!(["type Geo", "    number =", "        x", "    area self = x + x"].join("\n"),
        @r#"(BodyBlock #((TypeDef Geo #() #((Function () #() () () (Ident number) #() () (BodyBlock #((ExpressionStatement () (Ident x))))) (Function () #() () () (Ident area) #((() (Ident self) () ())) () (OprApp (Ident x) (Ok "+") (Ident x)))))))"#);
    test_module!([
            "type Problem_Builder",
            "    ## Returns a vector containing all reported problems, aggregated.",
            "    build_problemset : Vector",
            "    build_problemset self =",
            "        self",
        ].join("\n"),
        @r#"(BodyBlock #((TypeDef Problem_Builder #() #((Function ((#((Section " Returns a vector containing all reported problems, aggregated."))) #(())) #() ((Ident build_problemset) ":" (Ident Vector)) () (Ident build_problemset) #((() (Ident self) () ())) () (BodyBlock #((ExpressionStatement () (Ident self)))))))))"#);
    test_module!("[foo., bar.]",
        @r#"(BodyBlock #((ExpressionStatement () (Array (OprSectionBoundary 1 (OprApp (Ident foo) (Ok ".") ())) #(("," (OprSectionBoundary 1 (OprApp (Ident bar) (Ok ".") ()))))))))"#);
}

#[test]
fn type_operator_methods() {
    test_module!([ "type Foo",
            "    + : Foo -> Foo -> Foo",
            "    + self b = b",
            "    Foo.+ : Foo",
            "    Foo.+ self b = b",
        ].join("\n"),
        @r#"(BodyBlock #((TypeDef Foo #() #((Function () #() ((Ident +) ":" (OprApp (Ident Foo) (Ok "->") (OprApp (Ident Foo) (Ok "->") (Ident Foo)))) () (Ident +) #((() (Ident self) () ()) (() (Ident b) () ())) () (Ident b)) (Function () #() ((OprApp (Ident Foo) (Ok ".") (Ident +)) ":" (Ident Foo)) () (OprApp (Ident Foo) (Ok ".") (Ident +)) #((() (Ident self) () ()) (() (Ident b) () ())) () (Ident b))))))"#);
    test_block!("Any.==",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident Any) (Ok ".") (Ident ==)))))"#);
    expect_invalid_node("x.-y");
    expect_invalid_node("x.-1");
    expect_invalid_node("x.+y");
    expect_invalid_node("x.+1");
    expect_invalid_node("x.+'a'");
    // Compile-time operators are never operator-identifiers.
    test_block!("x.~y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok ".") (UnaryOprApp "~" (Ident y))))))"#);
    test_block!("x.~1",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok ".") (UnaryOprApp "~" (Number () "1" ()))))))"#);
}

#[test]
fn type_def_full() {
    test_module!([ "type Geo",
            "    Circle",
            "        radius : float",
            "        x",
            "    Rectangle width height",
            "    Point",
            "",
            "    number =",
            "        x",
            "    area self = x + x",
        ].join("\n"),
        @r#"(BodyBlock #((TypeDef Geo #() #((ConstructorDefinition () #() () Circle #() #(((() (Ident radius) (":" (Ident float)) ())) ((() (Ident x) () ())))) (ConstructorDefinition () #() () Rectangle #((() (Ident width) () ()) (() (Ident height) () ())) #()) (ConstructorDefinition () #() () Point #() #()) () (Function () #() () () (Ident number) #() () (BodyBlock #((ExpressionStatement () (Ident x))))) (Function () #() () () (Ident area) #((() (Ident self) () ())) () (OprApp (Ident x) (Ok "+") (Ident x)))))))"#);
}

#[test]
fn type_def_defaults() {
    test_module!("type Result error ok=Nothing\n    Ok value:ok=Nothing\n    Error (value:e = Nothing)",
        @r#"(BodyBlock #((TypeDef Result #((() (Ident error) () ()) (() (Ident ok) () ((Ident Nothing)))) #((ConstructorDefinition () #() () Ok #((() (Ident value) (":" (Ident ok)) ((Ident Nothing)))) #()) (ConstructorDefinition () #() () Error #((() (Ident value) (":" (Ident e)) ((Ident Nothing)))) #())))))"#);
    expect_invalid_node("type Result\n    Ok value:ok = Nothing");
}

#[test]
fn type_def_nested() {
    test_module!(["type Foo", "    type Bar", "    type Baz"].join("\n"),
        @"(BodyBlock #((TypeDef Foo #() #((TypeDef Bar #() #()) (TypeDef Baz #() #())))))");
}


// === Variable Assignment ===

#[test]
fn assignment_simple() {
    // At the top level of a module, this defines a function with no arguments.
    test_module!("foo = x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #() () (Ident x))))");
    // In a body block, this is a variable binding.
    test_block!("main =\n    foo = x",
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((Assignment () (Ident foo) (Ident x)))))))");
    test_block!("foo=x",
        @"(BodyBlock #((Assignment () (Ident foo) (Ident x))))");
    test_block!("foo= x",
        @"(BodyBlock #((Assignment () (Ident foo) (Ident x))))");
    expect_invalid_node("foo =x");
}

#[test]
fn assignment_documentation() {
    test_block!("## The Foo\nfoo = x",
        @r#"(BodyBlock #((Assignment ((#((Section " The Foo"))) #(())) (Ident foo) (Ident x))))"#);
}


// === Functions ===

#[test]
fn function_inline_simple_args() {
    test_module!("foo a = x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ())) () (Ident x))))");
    test_module!("foo a b = x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ())) () (Ident x))))");
    test_module!("foo a b c = x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ()) (() (Ident c) () ())) () (Ident x))))");
    test_module!("foo _ = x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Wildcard -1) () ())) () (Ident x))))");
    expect_invalid_node("foo a =x");
}

#[test]
fn function_noargs_nobody() {
    test_module!("foo =", @"(BodyBlock #((Function () #() () () (Ident foo) #() () ())))");
}

#[test]
fn function_no_body() {
    test_module!("foo a =",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ())) () ())))");
    test_module!("foo a b =",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ())) () ())))");
    test_module!("foo a b c =",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ()) (() (Ident c) () ())) () ())))");
    test_module!("foo _ =",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Wildcard -1) () ())) () ())))");
}

#[test]
fn function_block_body() {
    test_module!("foo a =\n    a",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ())) () (BodyBlock #((ExpressionStatement () (Ident a)))))))");
    test_module!("foo a b =\n    a",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ())) () (BodyBlock #((ExpressionStatement () (Ident a)))))))");
    test_module!("foo a b c =\n    a",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ()) (() (Ident c) () ())) () (BodyBlock #((ExpressionStatement () (Ident a)))))))");
}

#[test]
fn function_qualified() {
    test_module!("Id.id x = x",
        @r#"(BodyBlock #((Function () #() () () (OprApp (Ident Id) (Ok ".") (Ident id)) #((() (Ident x) () ())) () (Ident x))))"#);
}

#[test]
fn ignored_arguments() {
    test_module!("f _ = x",
        @"(BodyBlock #((Function () #() () () (Ident f) #((() (Wildcard -1) () ())) () (Ident x))))");
    test_module!("f ~_ = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #(("~" (Wildcard -1) () ())) () (Ident x))))"#);
}

#[test]
fn foreign_functions() {
    test_module!("foreign python my_method a b = \"42\"",
        @r#"(BodyBlock #((ForeignFunction python my_method #((() (Ident a) () ()) (() (Ident b) () ())) (TextLiteral #((Section "42"))))))"#);
    test_module!("foreign python my_method = \"42\"",
        @r#"(BodyBlock #((ForeignFunction python my_method #() (TextLiteral #((Section "42"))))))"#);
}

#[test]
fn function_inline_return_specification() {
    // Typical usage
    test_module!("id self that:Integer -> Integer = that",
        @r#"(BodyBlock #((Function () #() () () (Ident id) #((() (Ident self) () ()) (() (Ident that) (":" (Ident Integer)) ())) ("->" (Ident Integer)) (Ident that))))"#);
    // Edge case
    test_module!("number -> Integer = 23",
        @r#"(BodyBlock #((Function () #() () () (Ident number) #() ("->" (Ident Integer)) (Number () "23" ()))))"#);
    expect_invalid_node("f x : Integer -> Integer = 23");
}


// === Named arguments ===

#[test]
fn named_arguments() {
    test_module!("f x=y",
        @"(BodyBlock #((ExpressionStatement () (NamedApp (Ident f) x (Ident y)))))");
    test_module!("f (x = y)",
        @"(BodyBlock #((ExpressionStatement () (NamedApp (Ident f) x (Ident y)))))");
    expect_invalid_node("f (x = y");
    expect_invalid_node("f (x=y");
    expect_invalid_node("f x=)");
    expect_invalid_node("f (x =)");
    test_block!("(x a=b)",
        @"(BodyBlock #((ExpressionStatement () (Group (NamedApp (Ident x) a (Ident b))))))");
    test_block!("(x a=b.c)",
        @r#"(BodyBlock #((ExpressionStatement () (Group (NamedApp (Ident x) a (OprApp (Ident b) (Ok ".") (Ident c)))))))"#);
    test_block!("catch handler=exc->\n    throw",
        @r#"(BodyBlock #((ExpressionStatement () (NamedApp (Ident catch) handler (OprApp (Ident exc) (Ok "->") (BodyBlock #((ExpressionStatement () (Ident throw)))))))))"#);
    test_block!("sort by=x-> y-> compare x y",
        @r#"(BodyBlock #((ExpressionStatement () (NamedApp (Ident sort) by (OprApp (Ident x) (Ok "->") (OprApp (Ident y) (Ok "->") (App (App (Ident compare) (Ident x)) (Ident y))))))))"#);
    test_block!("sort by=(<) xs",
        @r#"(BodyBlock #((ExpressionStatement () (App (NamedApp (Ident sort) by (Group (OprSectionBoundary 2 (OprApp () (Ok "<") ())))) (Ident xs)))))"#);
    test_block!("sort by=(x-> x) y-> compare x y",
        @r#"(BodyBlock #((ExpressionStatement () (App (NamedApp (Ident sort) by (Group (OprApp (Ident x) (Ok "->") (Ident x)))) (OprApp (Ident y) (Ok "->") (App (App (Ident compare) (Ident x)) (Ident y)))))))"#);
    test_block!("sort by=(x-> x) 1",
        @r#"(BodyBlock #((ExpressionStatement () (App (NamedApp (Ident sort) by (Group (OprApp (Ident x) (Ok "->") (Ident x)))) (Number () "1" ())))))"#);
    test_block!("foo to=", @"(BodyBlock #((ExpressionStatement () (App (Ident foo) (Invalid)))))");
    test_module!("foo to=", @"(BodyBlock #((ExpressionStatement () (App (Ident foo) (Invalid)))))");
    test_block!("(foo to=)",
        @"(BodyBlock #((ExpressionStatement () (Group (App (Ident foo) (Invalid))))))");
    test_block!("filter (foo to=(1))",
        @r#"(BodyBlock #((ExpressionStatement () (App (Ident filter) (Group (NamedApp (Ident foo) to (Group (Number () "1" ()))))))))"#);
    test_block!("foo . bar baz=quux",
        @r#"(BodyBlock #((ExpressionStatement () (NamedApp (OprApp (Ident foo) (Ok ".") (Ident bar)) baz (Ident quux)))))"#);
}


// === Default arguments ===

#[test]
fn default_app() {
    test_block!("f default",
        @"(BodyBlock #((ExpressionStatement () (App (Ident f) (Ident default)))))");
}

#[test]
fn argument_named_default() {
    test_module!("f default x = x",
        @"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident default) () ()) (() (Ident x) () ())) () (Ident x))))");
    test_module!("f x default = x",
        @"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) () ()) (() (Ident default) () ())) () (Ident x))))");
}

#[test]
fn complex_arguments() {
    test_module!("f x=1 = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) () ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f (x : Number) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ())) () (Ident x))))"#);
    test_module!("f (x = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) () ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f ((x = 1) : Number) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) (":" (Ident Number)) ())) () (Ident x))))"#);
    test_module!("f (x=1 : Number) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) (":" (Ident Number)) ())) () (Ident x))))"#);
    test_module!("f (x : Number = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f (x y) = x",
        @"(BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) () ())) () (Ident x))))");
    test_module!("f ((x : Number) = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f ((x : Array Number) = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (App (Ident Array) (Ident Number))) ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f (x):Number=1 = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) (":" (Ident Number)) ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f ((x:Number=1)) = x",
        @"(BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) () ())) () (Ident x))))");
    test_module!("f (x : Number)=1 = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f (x:Number = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f (x:Number=1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f x:Number=1 = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Ident x))))"#);
    // Pattern in LHS:
    test_module!("f ~x=1 = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #(("~" (Ident x) () ((Number () "1" ())))) () (Ident x))))"#);
    test_module!("f (~x = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #(("~" (Ident x) () ((Number () "1" ())))) () (Ident x))))"#);
}


// === Code Blocks ===

#[test]
fn code_block_body() {
    test_module!(["main =", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((ExpressionStatement () (Ident x)))))))");
    test_module!(["main =", "      ", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #(() (ExpressionStatement () (Ident x)))))))");
    test_module!(["main =", "    ", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #(() (ExpressionStatement () (Ident x)))))))");
    test_module!(["main =", "  ", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #(() (ExpressionStatement () (Ident x)))))))");
    test_module!(["main =", "", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #(() (ExpressionStatement () (Ident x)))))))");
}

#[test]
fn operator_block() {
    test_block!(["value = nums", "    * each random", "    + constant"].join("\n"),
        @r#"(BodyBlock #((Assignment () (Ident value) (OperatorBlockApplication (Ident nums) #(((Ok "*") (App (Ident each) (Ident random))) ((Ok "+") (Ident constant))) #()))))"#);
}

#[test]
fn operator_block_precedence() {
    // Operator block application precedence is lower than general operators, e.g. `+`.
    test_block!(["1 + 2", "    * 3"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (OprApp (Number () "1" ()) (Ok "+") (Number () "2" ())) #(((Ok "*") (Number () "3" ()))) #()))))"#);
    // Operator block application precedence is higher than assigment (`=`).
    test_block!(["x = 1 + 2", "    * 3"].join("\n"),
        @r#"(BodyBlock #((Assignment () (Ident x) (OperatorBlockApplication (OprApp (Number () "1" ()) (Ok "+") (Number () "2" ())) #(((Ok "*") (Number () "3" ()))) #()))))"#);
}

#[test]
fn argument_block_precedence() {
    // Argument block has lower precedence than application, so combined inline/block application is
    // possible.
    test_module!(["f 1 n=2", "    3", "    4"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (ArgumentBlockApplication (NamedApp (App (Ident f) (Number () "1" ())) n (Number () "2" ())) #((Number () "3" ()) (Number () "4" ()))))))"#);
    // Argument block has lower precedence than assignment.
    test_block!(["x = f 1 2", "    3", "    4"].join("\n"),
        @r#"(BodyBlock #((Assignment () (Ident x) (ArgumentBlockApplication (App (App (Ident f) (Number () "1" ())) (Number () "2" ())) #((Number () "3" ()) (Number () "4" ()))))))"#);
}

#[test]
fn dot_operator_blocks() {
    test_module!(["rect1", "    . width * 7", "    . abs", "        + x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (Ident rect1) #(((Ok ".") (OprApp (Ident width) (Ok "*") (Number () "7" ()))) ((Ok ".") (OperatorBlockApplication (Ident abs) #(((Ok "+") (Ident x))) #()))) #()))))"#);
    expect_invalid_node("rect1\n    . width = 7");
}

#[test]
fn argument_blocks() {
    test_block!("foo\n    bar",
        @"(BodyBlock #((ExpressionStatement () (ArgumentBlockApplication (Ident foo) #((Ident bar))))))");
    test_block!("value = foo\n    bar",
        @"(BodyBlock #((Assignment () (Ident value) (ArgumentBlockApplication (Ident foo) #((Ident bar))))))");
    test_block!(["value = foo", "    +x", "    bar"].join("\n"),
        @r#"(BodyBlock #((Assignment () (Ident value) (ArgumentBlockApplication (Ident foo) #((OprSectionBoundary 1 (OprApp () (Ok "+") (Ident x))) (Ident bar))))))"#);
}

#[test]
fn code_block_empty() {
    // The first line here should parse as a function with no body expression (which is an error).
    // No input would parse as an empty `ArgumentBlock` or `OperatorBlock`, because those types are
    // distinguished from a body continuation by the presence of non-empty indented lines.
    test_module!("foo =\nbar",
        @"(BodyBlock #((Function () #() () () (Ident foo) #() () ()) (ExpressionStatement () (Ident bar))))");
    // This parses similarly to above; a line with no non-whitespace content does not create a code
    // block.
    test_module!(["foo =", "    ", "bar"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident foo) #() () ()) () (ExpressionStatement () (Ident bar))))");
}

#[test]
fn code_block_bad_indents() {
    test_module!(["main =", "  foo", " bar", "  baz"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((ExpressionStatement () (Ident foo)) (ExpressionStatement () (Ident bar)) (ExpressionStatement () (Ident baz)))))))");
    test_module!(["main =", "  foo", " bar", "baz"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((ExpressionStatement () (Ident foo)) (ExpressionStatement () (Ident bar))))) (ExpressionStatement () (Ident baz))))");
}

#[test]
fn code_block_with_following_statement() {
    test_module!(["main =", "    foo", "bar"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((ExpressionStatement () (Ident foo))))) (ExpressionStatement () (Ident bar))))");
}

#[test]
fn operator_block_nested() {
    test_block!(["foo", "    + bar", "        - baz"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (Ident foo) #(((Ok "+") (OperatorBlockApplication (Ident bar) #(((Ok "-") (Ident baz))) #()))) #()))))"#);
}

#[test]
fn operator_section_in_operator_block() {
    test_block!(["foo", "    + bar +"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (Ident foo) #(((Ok "+") (OprSectionBoundary 1 (OprApp (Ident bar) (Ok "+") ())))) #()))))"#);
}

#[test]
fn first_line_indented() {
    test_module!(" a",
        @"(BodyBlock #((BodyBlock #((ExpressionStatement () (Ident a))))))");
}


// === Binary Operators ===

#[test]
fn multiple_operator_error() {
    expect_multiple_operator_error("x + + x");
    expect_multiple_operator_error("x + + + x");
    expect_multiple_operator_error("x + +");
    expect_multiple_operator_error("+ + x");
    expect_multiple_operator_error("+ +");
    expect_multiple_operator_error("+ -");
    expect_multiple_operator_error("x + -");
}

#[test]
fn precedence() {
    test_block!("x * y + z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Ident x) (Ok "*") (Ident y)) (Ok "+") (Ident z)))))"#);
    test_block!("x + y * z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "+") (OprApp (Ident y) (Ok "*") (Ident z))))))"#);
    test_block!("w + x + y * z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Ident w) (Ok "+") (Ident x)) (Ok "+") (OprApp (Ident y) (Ok "*") (Ident z))))))"#);
    test_block!("x - 1 + 2",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Ident x) (Ok "-") (Number () "1" ())) (Ok "+") (Number () "2" ())))))"#);
    test_block!("x+y * z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "+") (OprApp (Ident y) (Ok "*") (Ident z))))))"#);
}

#[test]
fn dot_operator_precedence() {
    test_block!("x y . f v",
        @r#"(BodyBlock #((ExpressionStatement () (App (OprApp (App (Ident x) (Ident y)) (Ok ".") (Ident f)) (Ident v)))))"#);
}

#[test]
fn dot_operator_template_function() {
    test_block!("foo._",
        @r#"(BodyBlock #((ExpressionStatement () (TemplateFunction 1 (OprApp (Ident foo) (Ok ".") (Wildcard 0))))))"#);
    test_block!("_.foo",
        @r#"(BodyBlock #((ExpressionStatement () (TemplateFunction 1 (OprApp (Wildcard 0) (Ok ".") (Ident foo))))))"#);
}

#[test]
fn right_associative_operators() {
    test_block!("x --> y ---> z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "-->") (OprApp (Ident y) (Ok "--->") (Ident z))))))"#);
    test_block!("x <| y <<| z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "<|") (OprApp (Ident y) (Ok "<<|") (Ident z))))))"#);
}

#[test]
fn left_associative_operators() {
    test_block!("x + y + z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Ident x) (Ok "+") (Ident y)) (Ok "+") (Ident z)))))"#);
}

#[test]
fn pipeline_operators() {
    test_block!("f <| a",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident f) (Ok "<|") (Ident a)))))"#);
    test_block!("a |> f",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident a) (Ok "|>") (Ident f)))))"#);
}

#[test]
fn accessor_operator() {
    // Test that the accessor operator `.` is treated like any other operator.
    test_block!("Console.",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp (Ident Console) (Ok ".") ())))))"#);
    test_block!(".",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 2 (OprApp () (Ok ".") ())))))"#);
    test_block!(".log",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp () (Ok ".") (Ident log))))))"#);
}

#[test]
fn operator_sections() {
    test_block!(".map (+2 * 3) *7",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (App (App (OprApp () (Ok ".") (Ident map)) (Group (OprSectionBoundary 1 (OprApp (OprApp () (Ok "+") (Number () "2" ())) (Ok "*") (Number () "3" ()))))) (OprSectionBoundary 1 (OprApp () (Ok "*") (Number () "7" ()))))))))"#);
    test_block!(".sum 1",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (App (OprApp () (Ok ".") (Ident sum)) (Number () "1" ()))))))"#);
    test_block!("+1 + x",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp (OprApp () (Ok "+") (Number () "1" ())) (Ok "+") (Ident x))))))"#);
    test_block!("increment = 1 +",
        @r#"(BodyBlock #((Assignment () (Ident increment) (OprSectionBoundary 1 (OprApp (Number () "1" ()) (Ok "+") ())))))"#);
    test_block!("1+ << 2*",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp (OprApp (Number () "1" ()) (Ok "+") ()) (Ok "<<") (OprSectionBoundary 1 (OprApp (Number () "2" ()) (Ok "*") ())))))))"#);
    test_block!("1+1+ << 2*2*",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp (OprApp (OprApp (Number () "1" ()) (Ok "+") (Number () "1" ())) (Ok "+") ()) (Ok "<<") (OprSectionBoundary 1 (OprApp (OprApp (Number () "2" ()) (Ok "*") (Number () "2" ())) (Ok "*") ())))))))"#);
    test_block!("+1 << *2",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp (OprApp () (Ok "+") (Number () "1" ())) (Ok "<<") (OprSectionBoundary 1 (OprApp () (Ok "*") (Number () "2" ()))))))))"#);
    test_block!("+1+1 << *2*2",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp (OprApp (OprApp () (Ok "+") (Number () "1" ())) (Ok "+") (Number () "1" ())) (Ok "<<") (OprSectionBoundary 1 (OprApp (OprApp () (Ok "*") (Number () "2" ())) (Ok "*") (Number () "2" ()))))))))"#);
}

#[test]
fn template_functions() {
    test_block!("_.map (_ + 2*3) _*7",
        @r#"(BodyBlock #((ExpressionStatement () (TemplateFunction 1 (App (App (OprApp (Wildcard 0) (Ok ".") (Ident map)) (Group (TemplateFunction 1 (OprApp (Wildcard 0) (Ok "+") (OprApp (Number () "2" ()) (Ok "*") (Number () "3" ())))))) (TemplateFunction 1 (OprApp (Wildcard 0) (Ok "*") (Number () "7" ()))))))))"#);
    test_block!("_.sum 1",
        @r#"(BodyBlock #((ExpressionStatement () (TemplateFunction 1 (App (OprApp (Wildcard 0) (Ok ".") (Ident sum)) (Number () "1" ()))))))"#);
    test_block!("_+1 + x",
        @r#"(BodyBlock #((ExpressionStatement () (TemplateFunction 1 (OprApp (OprApp (Wildcard 0) (Ok "+") (Number () "1" ())) (Ok "+") (Ident x))))))"#);
}


// === Unary Operators ===

#[test]
fn unevaluated_argument() {
    test_module!("main ~foo = x",
        @r#"(BodyBlock #((Function () #() () () (Ident main) #(("~" (Ident foo) () ())) () (Ident x))))"#);
}

#[test]
fn unary_operator_missing_operand() {
    expect_invalid_node("main ~ = x");
}

#[test]
fn unary_operator_at_end_of_expression() {
    expect_invalid_node("foo ~");
}

#[test]
fn unspaced_operator_sequence() {
    // Add a negated value.
    test_block!("x = y+-z",
        @r#"(BodyBlock #((Assignment () (Ident x) (OprApp (Ident y) (Ok "+") (UnaryOprApp "-" (Ident z))))))"#);
    // Create an operator section that adds a negated value to its input.
    test_block!("x = +-z",
        @r#"(BodyBlock #((Assignment () (Ident x) (OprSectionBoundary 1 (OprApp () (Ok "+") (UnaryOprApp "-" (Ident z)))))))"#);
    // The `-` can only be lexed as a unary operator, and unary operators cannot form sections.
    expect_invalid_node("main =\n    x = y+-");
    // Assign a negative number to x.
    test_block!("x=-1",
        @r#"(BodyBlock #((Assignment () (Ident x) (UnaryOprApp "-" (Number () "1" ())))))"#);
    // Assign a negated value to x.
    test_block!("x=-y",
        @r#"(BodyBlock #((Assignment () (Ident x) (UnaryOprApp "-" (Ident y)))))"#);
}

#[test]
fn minus_binary() {
    test_block!("x - x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "-") (Ident x)))))"#);
    test_block!("x-x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "-") (Ident x)))))"#);
    test_block!("x-1",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "-") (Number () "1" ())))))"#);
    test_block!("(x)-1",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Group (Ident x)) (Ok "-") (Number () "1" ())))))"#);
}

#[test]
fn minus_section() {
    test_block!("- x",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp () (Ok "-") (Ident x))))))"#);
    test_block!("(- x)",
        @r#"(BodyBlock #((ExpressionStatement () (Group (OprSectionBoundary 1 (OprApp () (Ok "-") (Ident x)))))))"#);
    test_block!("- (x * x)",
        @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (OprApp () (Ok "-") (Group (OprApp (Ident x) (Ok "*") (Ident x))))))))"#);
}

#[test]
fn minus_unary() {
    test_block!("f -x",
        @r#"(BodyBlock #((ExpressionStatement () (App (Ident f) (UnaryOprApp "-" (Ident x))))))"#);
    test_block!("-x",
        @r#"(BodyBlock #((ExpressionStatement () (UnaryOprApp "-" (Ident x)))))"#);
    test_block!("(-x)",
        @r#"(BodyBlock #((ExpressionStatement () (Group (UnaryOprApp "-" (Ident x))))))"#);
    test_block!("-(x * x)",
        @r#"(BodyBlock #((ExpressionStatement () (UnaryOprApp "-" (Group (OprApp (Ident x) (Ok "*") (Ident x)))))))"#);
    test_block!("x=-x",
        @r#"(BodyBlock #((Assignment () (Ident x) (UnaryOprApp "-" (Ident x)))))"#);
    test_block!("-x+x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (UnaryOprApp "-" (Ident x)) (Ok "+") (Ident x)))))"#);
    test_block!("-x*x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (UnaryOprApp "-" (Ident x)) (Ok "*") (Ident x)))))"#);
}

#[test]
fn minus_unary_decimal() {
    test_block!("-2.1",
        @r#"(BodyBlock #((ExpressionStatement () (UnaryOprApp "-" (Number () "2" ("." "1"))))))"#);
}

#[test]
fn minus_unary_in_method_app() {
    test_block!("-1.x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (UnaryOprApp "-" (Number () "1" ())) (Ok ".") (Ident x)))))"#);
    test_block!("-1.up_to 100",
        @r#"(BodyBlock #((ExpressionStatement () (App (OprApp (UnaryOprApp "-" (Number () "1" ())) (Ok ".") (Ident up_to)) (Number () "100" ())))))"#);
}

#[test]
fn method_app_in_minus_unary() {
    test_block!("-Number.positive_infinity",
        @r#"(BodyBlock #((ExpressionStatement () (UnaryOprApp "-" (OprApp (Ident Number) (Ok ".") (Ident positive_infinity))))))"#);
}

#[test]
fn autoscope_operator() {
    test_module!("x : ..True",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident x) ":" (AutoscopedIdentifier ".." True)))))"#);
    test_block!("x = ..True",
        @r#"(BodyBlock #((Assignment () (Ident x) (AutoscopedIdentifier ".." True))))"#);
    test_block!("x = f ..True",
        @r#"(BodyBlock #((Assignment () (Ident x) (App (Ident f) (AutoscopedIdentifier ".." True)))))"#);
    expect_invalid_node("x = ..not_a_constructor");
    expect_invalid_node("x = case a of ..True -> True");
    expect_invalid_node("x = ..4");
    expect_invalid_node("x = ..Foo.Bar");
    expect_invalid_node("x = f .. True");
    expect_invalid_node("x = f (.. ..)");
    expect_invalid_node("x = f (.. *)");
    expect_invalid_node("x = f (.. True)");
    expect_invalid_node("x = True..");
    expect_invalid_node("x = True..True");
    expect_invalid_node("x = ..");
    expect_invalid_node("x = .. True");
    expect_invalid_node("x : .. True");
}


// === Import/Export ===

#[test]
fn import() {
    test_module!("import project.IO",
        @r#"(BodyBlock #((Import () () ((Ident import) (OprApp (Ident project) (Ok ".") (Ident IO))) () () ())))"#);
    test_module!("import Standard.Base as Enso_List",
        @r#"(BodyBlock #((Import () () ((Ident import) (OprApp (Ident Standard) (Ok ".") (Ident Base))) () ((Ident as) (Ident Enso_List)) ())))"#);
    test_module!("from Standard.Base import Foo",
        @r#"(BodyBlock #((Import () ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Base))) ((Ident import) (Ident Foo)) () () ())))"#);
    test_module!("from Standard.Base import all",
        @r#"(BodyBlock #((Import () ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Base))) ((Ident import) ()) all () ())))"#);
    test_module!("from Standard.Base import all hiding Number, Boolean",
        @r#"(BodyBlock #((Import () ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Base))) ((Ident import) ()) all () ((Ident hiding) (OprApp (Ident Number) (Ok ",") (Ident Boolean))))))"#);
    test_module!("polyglot java import java.lang.Float",
        @r#"(BodyBlock #((Import ((Ident polyglot) (Ident java)) () ((Ident import) (OprApp (OprApp (Ident java) (Ok ".") (Ident lang)) (Ok ".") (Ident Float))) () () ())))"#);
    test_module!("polyglot java import java.net.URI as Java_URI",
        @r#"(BodyBlock #((Import ((Ident polyglot) (Ident java)) () ((Ident import) (OprApp (OprApp (Ident java) (Ok ".") (Ident net)) (Ok ".") (Ident URI))) () ((Ident as) (Ident Java_URI)) ())))"#);
    test_module!("from Standard.Base import Foo, Bar, Baz",
        @r#"(BodyBlock #((Import () ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Base))) ((Ident import) (OprApp (OprApp (Ident Foo) (Ok ",") (Ident Bar)) (Ok ",") (Ident Baz))) () () ())))"#);
    expect_invalid_node("from Standard.Base.Data.Array import new as array_new");
}

#[test]
fn export() {
    test_module!("export prj.Data.Foo",
        @r#"(BodyBlock #((Export () ((Ident export) (OprApp (OprApp (Ident prj) (Ok ".") (Ident Data)) (Ok ".") (Ident Foo))) ())))"#);
    test_module!("export Foo as Bar",
        @"(BodyBlock #((Export () ((Ident export) (Ident Foo)) ((Ident as) (Ident Bar)))))");
    test_module!("from Foo export Bar, Baz",
        @r#"(BodyBlock #((Export ((Ident from) (Ident Foo)) ((Ident export) (OprApp (Ident Bar) (Ok ",") (Ident Baz))) ())))"#);
    expect_invalid_node("from Foo export all hiding Bar, Baz");
    test_module!("from Foo export all", @"(BodyBlock #((ExpressionStatement () (Invalid))))");
}


// === Metadata ===


#[test]
fn metadata_raw() {
    let code = [
        "x",
        "",
        "",
        "",
        "#### METADATA ####",
        r#"[[{"index":{"value":7},"size":{"value":8}},"5bad897e-099b-4b00-9348-64092636746d"]]"#,
    ];
    let code = code.join("\n");
    let (_meta, code) = enso_parser::metadata::parse(&code).unwrap();
    test_module!(code, @"(BodyBlock #((ExpressionStatement () (Ident x)) ()))");
}

#[test]
fn metadata_parsing() {
    let code = metadata::ORDERS_WITH_METADATA;
    let (meta, code) = enso_parser::metadata::parse(code).unwrap();
    let _ast = parse(code);
    let _meta: enso_parser::metadata::Metadata = meta.unwrap();
}


// === Type annotations and signatures ===

#[test]
fn type_signatures() {
    test_module!("val : Bool",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident val) ":" (Ident Bool)))))"#);
    test_block!("val : Bool\nval",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident val) ":" (Ident Bool))) (ExpressionStatement () (Ident val))))"#);
    test_block!("val : Bool",
        @r#"(BodyBlock #((TypeAnnotated (Ident val) ":" (Ident Bool))))"#);
    test_module!("val : Bool\nval = True",
        @r#"(BodyBlock #((Function () #() ((Ident val) ":" (Ident Bool)) () (Ident val) #() () (Ident True))))"#);
    test_module!("val : Bool\n\nval = True",
        @r#"(BodyBlock #((Function () #() ((Ident val) ":" (Ident Bool)) () (Ident val) #() () (Ident True))))"#);
    test_module!("val : Bool\n\n\nval = True",
        @r#"(BodyBlock #((Function () #() ((Ident val) ":" (Ident Bool)) () (Ident val) #() () (Ident True))))"#);
    test_module!("val : Bool\ndifferent_name = True",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident val) ":" (Ident Bool))) (Function () #() () () (Ident different_name) #() () (Ident True))))"#);
    test_module!("val : List Int",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident val) ":" (App (Ident List) (Ident Int))))))"#);
    test_module!("foo : [Integer | Text] -> (Integer | Text)",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident foo) ":" (OprApp (Array (OprApp (Ident Integer) (Ok "|") (Ident Text)) #()) (Ok "->") (Group (OprApp (Ident Integer) (Ok "|") (Ident Text))))))))"#);
    test_module!("f a (b : Int) : Double",
        @r#"(BodyBlock #((TypeAnnotated (App (App (Ident f) (Ident a)) (Group (TypeAnnotated (Ident b) ":" (Ident Int)))) ":" (Ident Double))))"#);
    test_module!("f a (b = 1 : Int) : Double",
        @r#"(BodyBlock #((TypeAnnotated (NamedApp (App (Ident f) (Ident a)) b (TypeAnnotated (Number () "1" ()) ":" (Ident Int))) ":" (Ident Double))))"#);
}

#[test]
fn type_annotations() {
    test_block!("val = x : Int",
        @r#"(BodyBlock #((Assignment () (Ident val) (TypeAnnotated (Ident x) ":" (Ident Int)))))"#);
    test_block!("val = x : A : B : C",
        @r#"(BodyBlock #((Assignment () (Ident val) (TypeAnnotated (TypeAnnotated (TypeAnnotated (Ident x) ":" (Ident A)) ":" (Ident B)) ":" (Ident C)))))"#);
    test_block!("val = foo (x : Int)",
        @r#"(BodyBlock #((Assignment () (Ident val) (App (Ident foo) (Group (TypeAnnotated (Ident x) ":" (Ident Int)))))))"#);
    test_block!("(x : My_Type _)",
        @r#"(BodyBlock #((ExpressionStatement () (Group (TypeAnnotated (Ident x) ":" (App (Ident My_Type) (TemplateFunction 1 (Wildcard 0))))))))"#);
    test_module!("x : List Int -> Int",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident x) ":" (OprApp (App (Ident List) (Ident Int)) (Ok "->") (Ident Int))))))"#);
    test_module!("p:Plus + m:Plus",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (TypeAnnotated (Ident p) ":" (Ident Plus)) (Ok "+") (TypeAnnotated (Ident m) ":" (Ident Plus))))))"#);
}


// === Text Literals ===

#[test]
fn inline_text_literals() {
    test_block!(r#""I'm an inline raw text!""#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "I'm an inline raw text!"))))))"#);
    test_block!(r#"zero_length = """#,
        @"(BodyBlock #((Assignment () (Ident zero_length) (TextLiteral #()))))");
    test_block!(r#""type""#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "type"))))))"#);
    test_block!(r#"unclosed = ""#,
        @"(BodyBlock #((Assignment () (Ident unclosed) (TextLiteral #()))))");
    test_block!(r#"unclosed = "a"#,
        @r#"(BodyBlock #((Assignment () (Ident unclosed) (TextLiteral #((Section "a"))))))"#);
    test_block!(r#"'Other quote type'"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "Other quote type"))))))"#);
    test_block!(r#""Non-escape: \n""#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "Non-escape: \\n"))))))"#);
    test_block!(r#""Non-escape: \""#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "Non-escape: \\"))))))"#);
    test_block!(r#"'String with \' escape'"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "String with ") (Escape 39) (Section " escape"))))))"#);
    test_block!(r#"'\u0915\u094D\u0937\u093F'"#,
        @"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 2325) (Escape 2381) (Escape 2359) (Escape 2367))))))");
    test_block!(r#"('\n')"#,
        @"(BodyBlock #((ExpressionStatement () (Group (TextLiteral #((Escape 10)))))))");
    test_block!(r#"`"#, @"(BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!(r#"(")")"#,
        @r#"(BodyBlock #((ExpressionStatement () (Group (TextLiteral #((Section ")")))))))"#);
    test_block!(r#"'\x'"#,
        @"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 4294967295))))))");
    test_block!(r#"'\u'"#,
        @"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 4294967295))))))");
    test_block!(r#"'\U'"#,
        @"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 4294967295))))))");
}

#[test]
fn multiline_text_literals() {
    test_block!("'''", @"(BodyBlock #((ExpressionStatement () (TextLiteral #()))))");
    let code = r#""""
    part of the string
       3-spaces indented line, part of the Text Block
    this does not end the string -> '''

    `also` part of the string

x"#;
    test_block!(code,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "part of the string") (Newline) (Section "   3-spaces indented line, part of the Text Block") (Newline) (Section "this does not end the string -> '''") (Newline) (Newline) (Section "`also` part of the string")))) () (ExpressionStatement () (Ident x))))"#);
    test_block!(r#""""
    multiline string that doesn't end in a newline
x"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "multiline string that doesn't end in a newline")))) (ExpressionStatement () (Ident x))))"#);
    test_block!("x = \"\"\"\n    Indented multiline\nx",
        @r#"(BodyBlock #((Assignment () (Ident x) (TextLiteral #((Section "Indented multiline")))) (ExpressionStatement () (Ident x))))"#);
    test_block!("'''\n    \\nEscape at start\n",
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 10) (Section "Escape at start")))) ()))"#);
    test_module!("x =\n x = '''\n  x\nx",
        @r#"(BodyBlock #((Function () #() () () (Ident x) #() () (BodyBlock #((Assignment () (Ident x) (TextLiteral #((Section "x"))))))) (ExpressionStatement () (Ident x))))"#);
    test_block!("foo = bar '''\n baz",
        @r#"(BodyBlock #((Assignment () (Ident foo) (App (Ident bar) (TextLiteral #((Section "baz")))))))"#);
    test_block!("'''\n \\t'",
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 9) (Section "'"))))))"#);
    test_block!("'''\n x\n \\t'",
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "x") (Newline) (Escape 9) (Section "'"))))))"#);
}

#[test]
fn interpolated_literals_in_inline_text() {
    test_block!(r#"'Simple case.'"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "Simple case."))))))"#);
    test_block!(r#"'With a `splice`.'"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "With a ") (Splice (Ident splice)) (Section "."))))))"#);
    test_block!(r#"'` SpliceWithLeadingWhitespace`'"#,
        @"(BodyBlock #((ExpressionStatement () (TextLiteral #((Splice (Ident SpliceWithLeadingWhitespace)))))))");
    test_block!(r#"'String with \n escape'"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "String with ") (Escape 10) (Section " escape"))))))"#);
    test_block!(r#"'\x0Aescape'"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 10) (Section "escape"))))))"#);
    test_block!(r#"'\u000Aescape'"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 10) (Section "escape"))))))"#);
    test_block!(r#"'\u{0000A}escape'"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 10) (Section "escape"))))))"#);
    test_block!(r#"'\U0000000Aescape'"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 10) (Section "escape"))))))"#);
}

#[test]
fn interpolated_literals_in_multiline_text() {
    let code = r#"'''
    `splice` at start"#;
    test_block!(code,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Splice (Ident splice)) (Section " at start"))))))"#);
    let code = r#"'''
    text with a `splice`
    and some \u000Aescapes\'"#;
    test_block!(code,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "text with a ") (Splice (Ident splice)) (Newline) (Section "and some ") (Escape 10) (Section "escapes") (Escape 39))))))"#);
}


// === Lambdas ===

#[test]
fn new_lambdas() {
    test_block!(r#"\v-> v"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Ident v)))))"#);
    test_block!(r#"\ v -> v"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Ident v)))))"#);
    test_block!(r#"\v -> v"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Ident v)))))"#);
    test_block!(r#"\ v-> v"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Ident v)))))"#);
    test_block!(r#"\ x -> x + y"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident x) () ())) "->" (OprApp (Ident x) (Ok "+") (Ident y))))))"#);
    test_block!("\\v->\n    v",
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (BodyBlock #((ExpressionStatement () (Ident v))))))))"#);
    test_block!("\\ v ->\n    v",
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (BodyBlock #((ExpressionStatement () (Ident v))))))))"#);
    test_block!("f \\ v ->\n    v",
        @r#"(BodyBlock #((ExpressionStatement () (App (Ident f) (Lambda "\\" #((() (Ident v) () ())) "->" (BodyBlock #((ExpressionStatement () (Ident v)))))))))"#);
    test_block!(r#"\a b -> x"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident a) () ()) (() (Ident b) () ())) "->" (Ident x)))))"#);
    test_block!(r#"\~x -> x"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #(("~" (Ident x) () ())) "->" (Ident x)))))"#);
    test_block!(r#"\a (b = f _ 1) -> f a"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident a) () ()) (() (Ident b) () ((App (App (Ident f) (TemplateFunction 1 (Wildcard 0))) (Number () "1" ()))))) "->" (App (Ident f) (Ident a))))))"#);
    expect_invalid_node("\\");
    expect_invalid_node("\\ v");
    expect_invalid_node("\\v");
    expect_invalid_node("\\v->");
    expect_invalid_node("\\v->\n");
    expect_invalid_node("\\v->\nv");
}

#[test]
fn old_lambdas() {
    test_block!("x -> y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (Ident y)))))"#);
    test_block!("x->y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (Ident y)))))"#);
    test_block!("x-> y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (Ident y)))))"#);
    test_block!("x-> x + y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (OprApp (Ident x) (Ok "+") (Ident y))))))"#);
    test_block!("x->\n y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (BodyBlock #((ExpressionStatement () (Ident y))))))))"#);
    test_block!("x ->\n y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (BodyBlock #((ExpressionStatement () (Ident y))))))))"#);
    test_block!("f x->\n y",
        @r#"(BodyBlock #((ExpressionStatement () (App (Ident f) (OprApp (Ident x) (Ok "->") (BodyBlock #((ExpressionStatement () (Ident y)))))))))"#);
    test_block!("x->y-> z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (OprApp (Ident y) (Ok "->") (Ident z))))))"#);
    test_block!("foo = x -> (y = bar x) -> x + y",
        @r#"(BodyBlock #((Assignment () (Ident foo) (OprApp (Ident x) (Ok "->") (OprApp (Group (OprApp (Ident y) (Ok "=") (App (Ident bar) (Ident x)))) (Ok "->") (OprApp (Ident x) (Ok "+") (Ident y)))))))"#);
}


// === Pattern Matching ===

#[test]
fn pattern_irrefutable() {
    test_block!("Point x_val = my_point",
        @"(BodyBlock #((Assignment () (App (Ident Point) (Ident x_val)) (Ident my_point))))");
    test_block!("Vector _ = x",
        @"(BodyBlock #((Assignment () (App (Ident Vector) (Wildcard -1)) (Ident x))))");
    test_block!("X.y = z",
        @r#"(BodyBlock #((Function () #() () () (OprApp (Ident X) (Ok ".") (Ident y)) #() () (Ident z))))"#);
}

#[test]
fn pattern_invalid() {
    expect_invalid_node("x + y = z");
    expect_invalid_node("(x y) = z");
}

#[test]
fn case_expression() {
    test_block!(["case a of", "    Some -> x", "    Int -> x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident a) #(((() (Ident Some) "->" (Ident x))) ((() (Ident Int) "->" (Ident x))))))))"#);
    test_block!(["case a of", "    Vector_2d x y -> x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident a) #(((() (App (App (Ident Vector_2d) (Ident x)) (Ident y)) "->" (Ident x))))))))"#);
    test_block!(["case self of", "    Vector_2d -> x", "    _ -> x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident self) #(((() (Ident Vector_2d) "->" (Ident x))) ((() (Wildcard -1) "->" (Ident x))))))))"#);
    test_block!(["case foo of", "    v:My_Type -> x", "    v:(My_Type _ _) -> x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident foo) #(((() (TypeAnnotated (Ident v) ":" (Ident My_Type)) "->" (Ident x))) ((() (TypeAnnotated (Ident v) ":" (Group (App (App (Ident My_Type) (TemplateFunction 1 (Wildcard 0))) (TemplateFunction 1 (Wildcard 0))))) "->" (Ident x))))))))"#);
}

#[test]
fn case_documentation() {
    test_block!([
            "case a of",
            "    ## The Some case",
            "    Some -> x",
            "    ## The Int case",
            "    Int -> x",
        ].join("\n"),
    @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident a) #(((((#((Section " The Some case"))) #()) () () ())) ((() (Ident Some) "->" (Ident x))) ((((#((Section " The Int case"))) #()) () () ())) ((() (Ident Int) "->" (Ident x))))))))"#);
}

#[test]
fn case_by_type() {
    macro_rules! test_case {
        ( $code:expr, $($case:tt)* ) => {
            test_block!(&format!("case foo of\n {}", $code), $( $case )* );
        }
    }
    test_case!("f:A->B -> x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident foo) #(((() (TypeAnnotated (Ident f) ":" (OprApp (Ident A) (Ok "->") (Ident B))) "->" (Ident x))))))))"#);
    test_case!("f : A->B -> x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident foo) #(((() (TypeAnnotated (Ident f) ":" (OprApp (Ident A) (Ok "->") (Ident B))) "->" (Ident x))))))))"#);
    test_case!("v : A -> x->x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident foo) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (OprApp (Ident x) (Ok "->") (Ident x)))))))))"#);
    test_case!("v : A -> x -> x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident foo) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (OprApp (Ident x) (Ok "->") (Ident x)))))))))"#);
    test_case!("v:A->x->x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident foo) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (OprApp (Ident x) (Ok "->") (Ident x)))))))))"#);
    test_case!("v:A->x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident foo) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (Ident x))))))))"#);
    test_case!("v : A -> _ + x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident foo) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (TemplateFunction 1 (OprApp (Wildcard 0) (Ok "+") (Ident x))))))))))"#);
}

#[test]
fn suspended_default_arguments_in_pattern() {
    test_block!("case self of\n    Vector_2d ... -> x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Ident self) #(((() (App (Ident Vector_2d) (SuspendedDefaultArguments)) "->" (Ident x))))))))"#);
}

#[test]
fn suspended_default_arguments_in_expression() {
    test_block!("c = self.value ...",
        @r#"(BodyBlock #((Assignment () (Ident c) (App (OprApp (Ident self) (Ok ".") (Ident value)) (SuspendedDefaultArguments)))))"#);
    test_block!("c = self.value...",
        @r#"(BodyBlock #((Assignment () (Ident c) (App (OprApp (Ident self) (Ok ".") (Ident value)) (SuspendedDefaultArguments)))))"#);
}

// === Private (project-private) keyword ===

#[test]
fn private_keyword() {
    test_module!("private", @"(BodyBlock #((Private private)))");
    expect_invalid_node("private func");
    // Private binding is not supported.
    expect_invalid_node("main =\n    private var = 42");
    // Private function is not allowed in body block.
    expect_invalid_node("main =\n    private func x = 42");
    expect_invalid_node("private ConstructorOutsideType");
    expect_invalid_node("type My_Type\n    private");
    expect_invalid_node("private type My_Type\n    Ctor");
}

#[test]
fn private_methods() {
    test_module!("private method x = x",
        @"(BodyBlock #((Function () #() () private (Ident method) #((() (Ident x) () ())) () (Ident x))))");
    test_module!("private method =\n    42",
        @r#"(BodyBlock #((Function () #() () private (Ident method) #() () (BodyBlock #((ExpressionStatement () (Number () "42" ())))))))"#);
    test_module!("type T\n    private method x = x",
        @"(BodyBlock #((TypeDef T #() #((Function () #() () private (Ident method) #((() (Ident x) () ())) () (Ident x))))))");
}

#[test]
#[ignore]
fn private_is_first_statement() {
    expect_invalid_node(&["type T", "", "private"].join("\n"));
    // Comments and empty lines are allowed before `private`.
    test_module!(["# Some comment", "# Other comment", "", "private"].join("\n"),
        @"(BodyBlock #(() () () () () (Private private)))");
}

// === Array/tuple literals ===

#[test]
fn array_literals() {
    test_block!("[]",
        @"(BodyBlock #((ExpressionStatement () (Array () #()))))");
    test_block!("[x]",
        @"(BodyBlock #((ExpressionStatement () (Array (Ident x) #()))))");
    test_block!("[x, y]",
        @r#"(BodyBlock #((ExpressionStatement () (Array (Ident x) #(("," (Ident y)))))))"#);
    test_block!("[x, y, z]",
        @r#"(BodyBlock #((ExpressionStatement () (Array (Ident x) #(("," (Ident y)) ("," (Ident z)))))))"#);
    test_block!("[ x , y ]",
        @r#"(BodyBlock #((ExpressionStatement () (Array (Ident x) #(("," (Ident y)))))))"#);
    test_block!("[ x , y , z ]",
        @r#"(BodyBlock #((ExpressionStatement () (Array (Ident x) #(("," (Ident y)) ("," (Ident z)))))))"#);
}

#[test]
fn tuple_literals() {
    test_block!("{}",
        @"(BodyBlock #((ExpressionStatement () (Tuple () #()))))");
    test_block!("{x}",
        @"(BodyBlock #((ExpressionStatement () (Tuple (Ident x) #()))))");
    test_block!("{x, y}",
        @r#"(BodyBlock #((ExpressionStatement () (Tuple (Ident x) #(("," (Ident y)))))))"#);
}


// === Numeric literals ===

#[cfg(test)]
mod numbers {
    use super::*;

    #[test]
    fn with_decimal() {
        test_block!("pi = 3.14",
            @r#"(BodyBlock #((Assignment () (Ident pi) (Number () "3" ("." "14")))))"#);
    }

    #[test]
    fn digits_spaced_dot() {
        test_block!("1 . 0",
            @r#"(BodyBlock #((ExpressionStatement () (OprApp (Number () "1" ()) (Ok ".") (Number () "0" ())))))"#);
        test_block!("1 .0",
            @r#"(BodyBlock #((ExpressionStatement () (App (Number () "1" ()) (OprSectionBoundary 1 (OprApp () (Ok ".") (Number () "0" ())))))))"#);
        test_block!("1. 0",
            @r#"(BodyBlock #((ExpressionStatement () (OprSectionBoundary 1 (App (OprApp (Number () "1" ()) (Ok ".") ()) (Number () "0" ()))))))"#);
    }

    #[test]
    fn non_digits_dot_digits() {
        test_block!("x.0",
            @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok ".") (Number () "0" ())))))"#);
    }

    #[test]
    fn digits_dot_non_digits() {
        test_block!("0.0.x",
            @r#"(BodyBlock #((ExpressionStatement () (OprApp (Number () "0" ("." "0")) (Ok ".") (Ident x)))))"#);
        test_block!("1.0.0",
            @r#"(BodyBlock #((ExpressionStatement () (OprApp (Number () "1" ("." "0")) (Ok ".") (Number () "0" ())))))"#);
        test_block!("1.0x",
            @r#"(BodyBlock #((ExpressionStatement () (OprApp (Number () "1" ()) (Ok ".") (Number "0x" () ())))))"#);
        test_block!("876543.is_even.should_be_false",
            @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Number () "876543" ()) (Ok ".") (Ident is_even)) (Ok ".") (Ident should_be_false)))))"#);
    }

    #[test]
    fn with_base() {
        test_block!("0b10101010",
            @r#"(BodyBlock #((ExpressionStatement () (Number "0b" "10101010" ()))))"#);
        test_block!("0o122137",
            @r#"(BodyBlock #((ExpressionStatement () (Number "0o" "122137" ()))))"#);
        test_block!("0xAE2F14",
            @r#"(BodyBlock #((ExpressionStatement () (Number "0x" "AE2F14" ()))))"#);
    }

    #[test]
    fn base_only() {
        test_block!("0x", @r#"(BodyBlock #((ExpressionStatement () (Number "0x" () ()))))"#);
        test_block!("0b", @r#"(BodyBlock #((ExpressionStatement () (Number "0b" () ()))))"#);
        test_block!("0o", @r#"(BodyBlock #((ExpressionStatement () (Number "0o" () ()))))"#);
    }

    #[test]
    fn delimited() {
        test_block!("100_000",
            @r#"(BodyBlock #((ExpressionStatement () (Number () "100_000" ()))))"#);
        test_block!("10_000.99",
            @r#"(BodyBlock #((ExpressionStatement () (Number () "10_000" ("." "99")))))"#);
    }

    #[test]
    fn old_hex() {
        expect_invalid_node("16_17ffffffffffffffa");
    }
}


// === Whitespace ===

#[test]
fn trailing_whitespace() {
    test_block!("a ", @"(BodyBlock #((ExpressionStatement () (Ident a)) ()))");
    test_block!("a \n", @"(BodyBlock #((ExpressionStatement () (Ident a)) ()))");
    test_module!("a = \n x",
        @"(BodyBlock #((Function () #() () () (Ident a) #() () (BodyBlock #((ExpressionStatement () (Ident x)))))))");
}


// === Annotations ===

#[test]
fn at_operator() {
    expect_invalid_node("foo@bar");
    expect_invalid_node("foo @ bar");
}

#[test]
fn annotations() {
    test_module!("@on_problems P.g\nselect_columns : Text -> Table\nselect_columns text = to_table text",
        @r#"(BodyBlock #((Function () #(((on_problems (OprApp (Ident P) (Ok ".") (Ident g))) #(()))) ((Ident select_columns) ":" (OprApp (Ident Text) (Ok "->") (Ident Table))) () (Ident select_columns) #((() (Ident text) () ())) () (App (Ident to_table) (Ident text)))))"#);
    test_module!("@a\n@b 1 + 1\nf x = x",
        @r#"(BodyBlock #((Function () #(((a ()) #(())) ((b (OprApp (Number () "1" ()) (Ok "+") (Number () "1" ()))) #(()))) () () (Ident f) #((() (Ident x) () ())) () (Ident x))))"#);
}

#[test]
fn annotations_on_type_methods() {
    test_module!("type A\n @a z\n @b\n x y = x",
        @"(BodyBlock #((TypeDef A #() #((Function () #(((a (Ident z)) #(())) ((b ()) #(()))) () () (Ident x) #((() (Ident y) () ())) () (Ident x))))))");
}

#[test]
fn annotations_on_type_constructors() {
    test_module!("type A\n @a z\n @b\n Baz x",
        @"(BodyBlock #((TypeDef A #() #((ConstructorDefinition () #(((a (Ident z)) #(())) ((b ()) #(()))) () Baz #((() (Ident x) () ())) #())))))");
}

#[test]
fn inline_builtin_annotations() {
    test_module!("@Tail_Call go t",
        @"(BodyBlock #((AnnotatedBuiltin Tail_Call #() (App (Ident go) (Ident t)))))");
    test_module!("@Tail_Call go (x = y)",
        @"(BodyBlock #((AnnotatedBuiltin Tail_Call #() (NamedApp (Ident go) x (Ident y)))))");
    test_module!("@Tail_Call go\n a\n b",
        @"(BodyBlock #((AnnotatedBuiltin Tail_Call #() (ArgumentBlockApplication (Ident go) #((Ident a) (Ident b))))))");
    test_module!("map _-> @Tail_Call f",
        @r#"(BodyBlock #((ExpressionStatement () (App (Ident map) (OprApp (Wildcard 0) (Ok "->") (AnnotatedBuiltin Tail_Call #() (Ident f)))))))"#);
}

#[test]
fn multiline_builtin_annotations() {
    test_module!("@Builtin_Type\ntype Date",
        @"(BodyBlock #((AnnotatedBuiltin Builtin_Type #(()) (TypeDef Date #() #()))))");
}


// === SKIP and FREEZE ===

#[test]
fn freeze() {
    test_block!("FREEZE x",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident FREEZE) (Ident x)))))))");
    test_block!("FREEZE x + y",
        @r#"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident FREEZE) (OprApp (Ident x) (Ok "+") (Ident y))))))))"#);
    test_block!("FREEZE x.f",
        @r#"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident FREEZE) (OprApp (Ident x) (Ok ".") (Ident f))))))))"#);
    test_block!("FREEZE x.f y",
        @r#"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident FREEZE) (App (OprApp (Ident x) (Ok ".") (Ident f)) (Ident y))))))))"#);
}

#[test]
fn skip() {
    test_block!("SKIP x",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident SKIP) (Ident x)))))))");
    test_block!("SKIP x + y",
        @r#"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident SKIP) (OprApp (Ident x) (Ok "+") (Ident y))))))))"#);
    test_block!("SKIP x.f",
        @r#"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident SKIP) (OprApp (Ident x) (Ok ".") (Ident f))))))))"#);
    test_block!("SKIP x.f y",
        @r#"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident SKIP) (App (OprApp (Ident x) (Ok ".") (Ident f)) (Ident y))))))))"#);
}

// === Context errors ===

#[test]
fn statement_in_expression_context() {
    test_block!("x = y = z", @"(BodyBlock #((Assignment () (Ident x) (Invalid))))");
    test_block!("(y = z)", @"(BodyBlock #((ExpressionStatement () (Group (Invalid)))))");
    test_block!("(y = z) x",
        @"(BodyBlock #((ExpressionStatement () (App (Group (Invalid)) (Ident x)))))");
    test_block!("(f x = x)", @"(BodyBlock #((ExpressionStatement () (Group (Invalid)))))");
    test_block!("y = f x = x", @"(BodyBlock #((Assignment () (Ident y) (Invalid))))");
}



// =========================
// === Scalability Tests ===
// =========================

/// Test an input that caused a stack overflow in a version of the parser that used recursion to
/// resolve macro segments.
#[test]
fn big_array() {
    let mut big_array = "[".to_owned();
    // This value was chosen to be large enough to cause a stack overflow, but not so large that it
    // would take a long time to do so.
    let array_length = 1000;
    for _ in 0..array_length {
        big_array.push_str(
            r#"[{"index":{"value":1},"size":{"value":8}},"6063e6d3-3341-40f4-b4fb-7e986eb31ae8"],"#,
        );
    }
    big_array.push_str("1]");
    expect_valid(&big_array);
}



// ==========================
// === Syntax Error Tests ===
// ==========================

#[test]
fn space_required() {
    expect_invalid_node("foo = if cond.x else.y");
}

#[test]
fn incomplete_type_definition() {
    expect_invalid_node("type");
}

#[test]
fn bad_case() {
    expect_invalid_node("foo = case x of\n 4");
    expect_invalid_node("foo = case x of\n 4 ->");
    expect_invalid_node("foo = case x of\n 4->");
}

#[test]
fn malformed_sequence() {
    expect_invalid_node("(1, )");
    expect_invalid_node("foo = (1, )");
}

#[test]
fn unmatched_delimiter() {
    expect_invalid_node("(");
    expect_invalid_node(")");
    expect_invalid_node("[");
    expect_invalid_node("]");
    expect_invalid_node("foo = (");
    expect_invalid_node("foo = )");
    expect_invalid_node("foo = [");
    expect_invalid_node("foo = ]");
}

#[test]
fn unexpected_special_operator() {
    expect_invalid_node("foo = 1, 2");
}

#[test]
fn malformed_import() {
    expect_invalid_node("import");
    expect_invalid_node("import as Foo");
    expect_invalid_node("import Foo as Foo, Bar");
    expect_invalid_node("import Foo as Foo.Bar");
    expect_invalid_node("import Foo as");
    expect_invalid_node("import Foo as Bar.Baz");
    expect_invalid_node("import Foo hiding");
    expect_invalid_node("import Foo hiding X,");
    expect_invalid_node("polyglot import Foo");
    expect_invalid_node("polyglot java import");
    expect_invalid_node("from import all");
    expect_invalid_node("from Foo import all hiding");
    expect_invalid_node("from Foo import all hiding X.Y");
    expect_invalid_node("export");
    expect_invalid_node("export as Foo");
    expect_invalid_node("export Foo as Foo, Bar");
    expect_invalid_node("export Foo as Foo.Bar");
    expect_invalid_node("export Foo as");
    expect_invalid_node("export Foo as Bar.Baz");
    expect_invalid_node("export Foo hiding");
    expect_invalid_node("export Foo hiding X,");
    expect_invalid_node("from export all");
    expect_invalid_node("from Foo export all hiding");
    expect_invalid_node("from Foo export all hiding X.Y");
}

#[test]
fn invalid_token() {
    expect_invalid_node("`");
    expect_invalid_node("splice_outside_text = `");
}

#[test]
fn illegal_foreign_body() {
    // Foreign is only a keyword on the LHS of an assignment operator.
    test_module!("foreign 4",
        @r#"(BodyBlock #((ExpressionStatement () (App (Ident foreign) (Number () "4" ())))))"#);
    // Missing name
    expect_invalid_node("foreign foo = \"4\"");
    // Body must be a type
    expect_invalid_node("foreign js foo = 4");
}

#[test]
fn unexpected_tokens_in_inner_macro_segment() {
    expect_invalid_node("from Foo import all What_Is_This_Doing_Here hiding Bar");
}

#[test]
fn invalid_unspaced_operator_sequence() {
    // Typically, a sequence of operator identifiers is lexed as a single operator. However, an
    // exception is made for some sequences of operator characters ending in the `-` character: An
    // expression such as `x+-x` is accepted, and read equivalently to `x + -x` (see
    // [`unspaced_operator_sequence`]).
    //
    // Due to this special case, there is no reasonable way to interpret this type of expression as
    // valid when spaces are added in the following way:
    expect_invalid_node("x = y +- z");
    expect_multiple_operator_error("x =- y");
    //
    // Treating the `-` as a unary operator applied to `z` would be confusing, as it would be in
    // contradiction to the associativity implied by the whitespace rules.
    //
    // However, it would also be confusing to lex a sequence of characters like `+-` as a single
    // operator in spaced expressions, but as two operators in unspaced expressions.
    //
    // Lacking any reasonable valid interpretation, we treat this case as an error.
    //
    // Similar expressions with missing operands should be treated likewise:
    expect_invalid_node("x = y +-");
    expect_invalid_node("x = +- z");
    expect_multiple_operator_error("x =-");
    expect_multiple_operator_error("=- y");
    expect_multiple_operator_error("=-");
}

#[test]
fn nonsense_inputs() {
    expect_invalid_node("`a (b = 1).`");
    expect_invalid_node("type M = B<d f<'a> F(M<'a>) -> S>;");
    expect_invalid_node("'`'\nx `y`\nz");
    expect_invalid_node("if (asGuestValue\n  a");
    expect_invalid_node("foo(\n  a");
    expect_invalid_node("(Vector(), true)");
    expect_invalid_node("x @Builtin_Method \"a\"");
}

#[test]
#[ignore]
fn nonsense_inputs_broken() {
    // FIXME
    expect_invalid_node("'`\n");
    expect_invalid_node(".'\\\n");
}
