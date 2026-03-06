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

use enso_parser_debug::test::expect_multiple_operator_error;
use enso_parser_debug::test::expect_valid;
use enso_parser_debug::test::parse_module;
use insta::assert_snapshot;

// ===========================
// === Test support macros ===
// ===========================

macro_rules! test_parse {
    ( $code:expr_2021, @$expected:tt, $parse:ident ) => {
        let code = $code;
        let code = code.as_ref();
        let tree = enso_parser_debug::test::$parse(code);
        let s_expr = enso_parser_debug::to_s_expr(&tree, code).to_string();
        let error = enso_parser_debug::test::first_error(&tree);
        if let Some(error) = error {
            assert_snapshot!(format!("{error}: {s_expr}"), @$expected)
        } else {
            assert_snapshot!(s_expr, @$expected)
        }
    };
}

macro_rules! test_module {
    ( $code:expr_2021, @$expected:tt ) => {
        test_parse!($code, @$expected, parse_module);
    };
}

macro_rules! test_block {
    ( $code:expr_2021, @$expected:tt ) => {
        test_parse!($code, @$expected, parse_block);
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
        @"(BodyBlock #((ExpressionStatement () (Call (App (App (Ident a) (Call (Ident b))) (Call (Ident c)))))))");
}

#[test]
fn parentheses() {
    test_block!("(a b)",
        @"(BodyBlock #((ExpressionStatement () (Group (Call (App (Ident a) (Call (Ident b))))))))");
    test_block!("x)",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("(x",
        @"Unclosed parenthesis in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("(a) (b)",
        @"(BodyBlock #((ExpressionStatement () (Call (App (Group (Call (Ident a))) (Group (Call (Ident b))))))))");
    test_block!("((a b) c)",
        @"(BodyBlock #((ExpressionStatement () (Group (Call (App (Group (Call (App (Ident a) (Call (Ident b))))) (Call (Ident c))))))))");
    test_block!("(a).b",
        @"(BodyBlock #((ExpressionStatement () (Call (PropertyAccess (Group (Call (Ident a))) b)))))");
}

#[test]
fn section_simple() {
    test_block!("+ a",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp () (Ok "+") (Call (Ident a))))))"#);
    test_block!("a +",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident a)) (Ok "+") ()))))"#);
}

#[test]
fn inline_if() {
    test_block!("if True then True else False",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident if) (Call (Ident True))) ((Ident then) (Call (Ident True))) ((Ident else) (Call (Ident False))))))))");
}

#[test]
fn then_block() {
    test_block!("if True then\n True",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident if) (Call (Ident True))) ((Ident then) (BodyBlock #((ExpressionStatement () (Call (Ident True)))))))))))");
}

#[test]
fn else_block() {
    test_block!("if True then True else\n False",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident if) (Call (Ident True))) ((Ident then) (Call (Ident True))) ((Ident else) (BodyBlock #((ExpressionStatement () (Call (Ident False)))))))))))");
}

#[test]
fn if_then_else_chained_block() {
    test_block!("if True then True else False\n    . to_text",
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (MultiSegmentApp #(((Ident if) (Call (Ident True))) ((Ident then) (Call (Ident True))) ((Ident else) (Call (Ident False))))) #(((Ok ".") (Call (Ident to_text)))) #()))))"#);
    test_block!("(if True then True else False)\n    . to_text",
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (Group (MultiSegmentApp #(((Ident if) (Call (Ident True))) ((Ident then) (Call (Ident True))) ((Ident else) (Call (Ident False)))))) #(((Ok ".") (Call (Ident to_text)))) #()))))"#);
    test_block!("if True then True else False\n    . to_text\n    . as_value",
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (MultiSegmentApp #(((Ident if) (Call (Ident True))) ((Ident then) (Call (Ident True))) ((Ident else) (Call (Ident False))))) #(((Ok ".") (Call (Ident to_text))) ((Ok ".") (Call (Ident as_value)))) #()))))"#);
    test_block!("if True then True else False\n    . to_text\n    . as_value\n    . done 42",
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (MultiSegmentApp #(((Ident if) (Call (Ident True))) ((Ident then) (Call (Ident True))) ((Ident else) (Call (Ident False))))) #(((Ok ".") (Call (Ident to_text))) ((Ok ".") (Call (Ident as_value))) ((Ok ".") (Call (App (Ident done) (Number () "42" ()))))) #()))))"#);
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
        @r#"(BodyBlock #((Function ((#((Section " The Identity Function") (Newline) (Newline) (Section "Arguments:") (Newline) (Section "- x: value to do nothing to"))) #(())) #() () () (Ident id) #((() (Ident x) () ())) () (Call (Ident x)))))"#);
    test_module!(&["type Foo", " ## Test indent handling", "  ", " foo bar = foo"].join("\n"),
        @r#"(BodyBlock #((TypeDef Foo #() #((Function ((#((Section " Test indent handling"))) #(() ())) #() () () (Ident foo) #((() (Ident bar) () ())) () (Call (Ident foo)))))))"#);
    test_module!("expression ## unexpected",
        @"Unexpected documentation at end of line: (BodyBlock #((ExpressionStatement () (Call (App (Ident expression) (Invalid))))))");
    test_block!("expression ## unexpected",
        @"Unexpected documentation at end of line: (BodyBlock #((ExpressionStatement () (Call (App (Ident expression) (Invalid))))))");
}

#[test]
fn expression_documentation() {
    test_block!("## The value of x\nx",
        @r#"(BodyBlock #((ExpressionStatement ((#((Section " The value of x"))) #(())) (Call (Ident x)))))"#);
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
    test_module!("type 1",
        @"Expected type identifier in type declaration: (BodyBlock #((Invalid)))");
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
        @r#"(BodyBlock #((TypeDef Foo #() #((ConstructorDefinition () #() () Bar #((() (Ident a) (":" (Ident B)) ((Call (PropertyAccess (Call (Ident C)) D))))) #())))))"#);
    test_module!(["type A", "    Foo (a : Integer, b : Integer)"].join("\n"),
        @r#"Invalid use of syntactic operator in expression: (BodyBlock #((TypeDef A #() #((ConstructorDefinition () #() () Foo #((() (Ident a) (":" (Invalid)) ())) #())))))"#);
}

#[test]
fn type_constructor_documentation() {
    test_module!("type Foo\n ## Bar\n Baz",
        @r#"(BodyBlock #((TypeDef Foo #() #((ConstructorDefinition ((#((Section " Bar"))) #(())) #() () Baz #() #())))))"#);
}

#[test]
fn type_constructors_panic_14559() {
    test_module!([
            "type Missing",
            "    Error",
            "        (argument_name : Text) (function_name : Text | Nothing = Nothing) all_location : Source_Location | Nothing = Nothing)",
        ].join("\n"),
        @r#"Expected identifier or wildcard in argument binding: (BodyBlock #((TypeDef Missing #() #((ConstructorDefinition () #() () Error #() #(((() (Invalid) (":" (OprApp (Ident Source_Location) (Ok "|") (Ident Nothing))) ((Invalid))))))))))"#);
    test_module!([
            "type Missing",
            "    Error",
            "        (argument_name : Text) (function_name : Text) = Nothing",
        ].join("\n"),
        @"Expected identifier or wildcard in argument binding: (BodyBlock #((TypeDef Missing #() #((ConstructorDefinition () #() () Error #() #(((() (Invalid) () ((Call (Ident Nothing)))))))))))");
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
        @r#"(BodyBlock #((TypeDef Geo #() #((Function () #() () () (Ident number) #() () (BodyBlock #((ExpressionStatement () (Call (Ident x)))))) (Function () #() () () (Ident area) #((() (Ident self) () ())) () (OprApp (Call (Ident x)) (Ok "+") (Call (Ident x))))))))"#);
    test_module!([
            "type Problem_Builder",
            "    ## Returns a vector containing all reported problems, aggregated.",
            "    build_problemset : Vector",
            "    build_problemset self =",
            "        self",
        ].join("\n"),
        @r#"(BodyBlock #((TypeDef Problem_Builder #() #((Function ((#((Section " Returns a vector containing all reported problems, aggregated."))) #(())) #() ((Ident build_problemset) ":" (Ident Vector)) () (Ident build_problemset) #((() (Ident self) () ())) () (BodyBlock #((ExpressionStatement () (Call (Ident self))))))))))"#);
    test_module!("[foo., bar.]",
        @r#"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Array (Invalid) #(("," (Invalid)))))))"#);
}

#[test]
fn type_operator_methods() {
    test_module!("type Foo\n    + : Foo -> Foo -> Foo\n    + self b = b",
        @r#"(BodyBlock #((TypeDef Foo #() #((Function () #() ((Ident +) ":" (OprApp (Ident Foo) (Ok "->") (OprApp (Ident Foo) (Ok "->") (Ident Foo)))) () (Ident +) #((() (Ident self) () ()) (() (Ident b) () ())) () (Call (Ident b)))))))"#);
    test_module!("type Foo\n    Foo.+ : Foo\n    Foo.+ self b = b",
        @r#"(BodyBlock #((TypeDef Foo #() #((Function () #() ((PropertyAccess (Ident Foo) +) ":" (Ident Foo)) () (PropertyAccess (Ident Foo) +) #((() (Ident self) () ()) (() (Ident b) () ())) () (Call (Ident b)))))))"#);
    test_block!("Any.==",
        @"(BodyBlock #((ExpressionStatement () (Call (PropertyAccess (Call (Ident Any)) ==)))))");
    test_block!("x.-y",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("x.-1",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("x.+y",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("x.+1",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("x.+'a'",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    // Compile-time operators are never operator-identifiers.
    test_block!("x.~y",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("x.~1",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
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
        @r#"(BodyBlock #((TypeDef Geo #() #((ConstructorDefinition () #() () Circle #() #(((() (Ident radius) (":" (Ident float)) ())) ((() (Ident x) () ())))) (ConstructorDefinition () #() () Rectangle #((() (Ident width) () ()) (() (Ident height) () ())) #()) (ConstructorDefinition () #() () Point #() #()) () (Function () #() () () (Ident number) #() () (BodyBlock #((ExpressionStatement () (Call (Ident x)))))) (Function () #() () () (Ident area) #((() (Ident self) () ())) () (OprApp (Call (Ident x)) (Ok "+") (Call (Ident x))))))))"#);
}

#[test]
fn type_def_defaults() {
    test_module!("type Result error ok=Nothing\n    Ok value:ok=Nothing\n    Error (value:e = Nothing)",
        @r#"(BodyBlock #((TypeDef Result #((() (Ident error) () ()) (() (Ident ok) () ((Call (Ident Nothing))))) #((ConstructorDefinition () #() () Ok #((() (Ident value) (":" (Ident ok)) ((Call (Ident Nothing))))) #()) (ConstructorDefinition () #() () Error #((() (Ident value) (":" (Ident e)) ((Call (Ident Nothing))))) #())))))"#);
    test_module!("type Result\n    Ok value:ok = Nothing",
        @r#"Expected identifier or wildcard in argument binding: (BodyBlock #((TypeDef Result #() #((ConstructorDefinition () #() () Ok #((() (Ident value) (":" (Ident ok)) ()) (() (Invalid) () ((Invalid))) (() (Ident Nothing) () ())) #())))))"#);
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
        @"(BodyBlock #((Function () #() () () (Ident foo) #() () (Call (Ident x)))))");
    // In a body block, this is a variable binding.
    test_block!("main =\n    foo = x",
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((Assignment () (Ident foo) (Call (Ident x))))))))");
    test_module!("foo=x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #() () (Call (Ident x)))))");
    test_block!("foo=x",
        @"(BodyBlock #((Assignment () (Ident foo) (Call (Ident x)))))");
    test_module!("foo= x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #() () (Call (Ident x)))))");
    test_block!("foo= x",
        @"(BodyBlock #((Assignment () (Ident foo) (Call (Ident x)))))");
    test_module!("foo =x",
        @"Each operator on the left side of an assignment operator must be applied to two operands, with the same spacing on each side: (BodyBlock #((Invalid)))");
    test_block!("foo =x",
        @"Each operator on the left side of an assignment operator must be applied to two operands, with the same spacing on each side: (BodyBlock #((Invalid)))");
}

#[test]
fn assignment_documentation() {
    test_block!("## The Foo\nfoo = x",
        @r#"(BodyBlock #((Assignment ((#((Section " The Foo"))) #(())) (Ident foo) (Call (Ident x)))))"#);
}

// === Functions ===

#[test]
fn function_inline_simple_args() {
    test_module!("foo a = x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ())) () (Call (Ident x)))))");
    test_module!("foo a b = x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ())) () (Call (Ident x)))))");
    test_module!("foo a b c = x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ()) (() (Ident c) () ())) () (Call (Ident x)))))");
    test_module!("foo _ = x",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Wildcard) () ())) () (Call (Ident x)))))");
    test_module!("foo a =x",
        @"Each operator on the left side of an assignment operator must be applied to two operands, with the same spacing on each side: (BodyBlock #((Invalid)))");
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
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Wildcard) () ())) () ())))");
}

#[test]
fn function_block_body() {
    test_module!("foo a =\n    a",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ())) () (BodyBlock #((ExpressionStatement () (Call (Ident a))))))))");
    test_module!("foo a b =\n    a",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ())) () (BodyBlock #((ExpressionStatement () (Call (Ident a))))))))");
    test_module!("foo a b c =\n    a",
        @"(BodyBlock #((Function () #() () () (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ()) (() (Ident c) () ())) () (BodyBlock #((ExpressionStatement () (Call (Ident a))))))))");
}

#[test]
fn function_qualified() {
    test_module!("Id.id x = x",
        @"(BodyBlock #((Function () #() () () (PropertyAccess (Ident Id) id) #((() (Ident x) () ())) () (Call (Ident x)))))");
}

#[test]
fn function_type_declaration() {
    test_module!("foo : local.Proj.My_Module.My_Type\nfoo = My_Type.Cons",
        @r#"(BodyBlock #((Function () #() ((Ident foo) ":" (PropertyAccess (PropertyAccess (PropertyAccess (Ident local) Proj) My_Module) My_Type)) () (Ident foo) #() () (Call (PropertyAccess (Call (Ident My_Type)) Cons)))))"#);
}

#[test]
fn ignored_arguments() {
    test_module!("f _ = x",
        @"(BodyBlock #((Function () #() () () (Ident f) #((() (Wildcard) () ())) () (Call (Ident x)))))");
    test_module!("f ~_ = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #(("~" (Wildcard) () ())) () (Call (Ident x)))))"#);
}

#[test]
fn foreign_functions() {
    test_module!("foreign python my_method a b = \"42\"",
        @r#"(BodyBlock #((ForeignFunction python my_method #((() (Ident a) () ()) (() (Ident b) () ())) (TextLiteral #((Section "42"))))))"#);
    test_module!("foreign python my_method = \"42\"",
        @r#"(BodyBlock #((ForeignFunction python my_method #() (TextLiteral #((Section "42"))))))"#);
    test_module!("foreign = \"🥖\"",
        @"Expected language name in foreign function definition: (BodyBlock #((Invalid)))");
    test_module!("foreign python = \"🐍🥖\"",
        @"Expected function name in foreign function definition: (BodyBlock #((Invalid)))");
}

#[test]
fn function_inline_return_specification() {
    // Typical usage
    test_module!("id self that:Integer -> Integer = that",
        @r#"(BodyBlock #((Function () #() () () (Ident id) #((() (Ident self) () ()) (() (Ident that) (":" (Ident Integer)) ())) ("->" (Ident Integer)) (Call (Ident that)))))"#);
    // Edge case
    test_module!("number -> Integer = 23",
        @r#"(BodyBlock #((Function () #() () () (Ident number) #() ("->" (Ident Integer)) (Number () "23" ()))))"#);
    test_module!("f x : Integer -> Integer = 23",
        @r#"Expected identifier or wildcard in argument binding: (BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) () ()) (() (Invalid) (":" (Invalid)) ()) (() (Ident Integer) () ())) ("->" (Ident Integer)) (Number () "23" ()))))"#);
}

#[test]
fn ignored_argument_patterns() {
    test_block!("_ -> x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Wildcard) (Ok "->") (Call (Ident x))))))"#);
    test_block!("_-> x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Wildcard) (Ok "->") (Call (Ident x))))))"#);
    test_block!("_ = x", @"(BodyBlock #((Assignment () (Wildcard) (Call (Ident x)))))");
    test_block!("_= x", @"(BodyBlock #((Assignment () (Wildcard) (Call (Ident x)))))");
    test_block!("\\_ -> x",
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Wildcard) () ())) "->" (Call (Ident x))))))"#);
    test_block!("\\_-> x",
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Wildcard) () ())) "->" (Call (Ident x))))))"#);
}

// === Named arguments ===

#[test]
fn named_arguments() {
    test_block!("f x=y",
        @"(BodyBlock #((ExpressionStatement () (Call (NamedApp (Ident f) x (Call (Ident y)))))))");
    test_block!("f (x = y)",
        @"(BodyBlock #((ExpressionStatement () (Call (NamedApp (Ident f) x (Call (Ident y)))))))");
    test_block!("f (x = y",
        @"Unclosed parenthesis in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("f (x=y",
        @"Unclosed parenthesis in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("f x=)",
        @"Unmatched delimiter: (BodyBlock #((ExpressionStatement () (Call (NamedApp (Ident f) x (Invalid))))))");
    test_block!("f (x =)",
        @"Operator must be applied to two operands: (BodyBlock #((ExpressionStatement () (Call (App (Ident f) (Group (Invalid)))))))");
    test_block!("(x a=b)",
        @"(BodyBlock #((ExpressionStatement () (Group (Call (NamedApp (Ident x) a (Call (Ident b))))))))");
    test_block!("(x a=b.c)",
        @"(BodyBlock #((ExpressionStatement () (Group (Call (NamedApp (Ident x) a (Call (PropertyAccess (Call (Ident b)) c))))))))");
    test_block!("catch handler=exc->\n    throw",
        @r#"(BodyBlock #((ExpressionStatement () (Call (NamedApp (Ident catch) handler (OprApp (Ident exc) (Ok "->") (BodyBlock #((ExpressionStatement () (Call (Ident throw)))))))))))"#);
    test_block!("sort by=x-> y-> compare x y",
        @r#"(BodyBlock #((ExpressionStatement () (Call (NamedApp (Ident sort) by (OprApp (Ident x) (Ok "->") (OprApp (Ident y) (Ok "->") (Call (App (App (Ident compare) (Call (Ident x))) (Call (Ident y)))))))))))"#);
    test_block!("sort by=(<) xs",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (NamedApp (Ident sort) by (Group (OprApp () (Ok "<") ()))) (Call (Ident xs)))))))"#);
    test_block!("sort by=(x-> x) y-> compare x y",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (NamedApp (Ident sort) by (Group (OprApp (Ident x) (Ok "->") (Call (Ident x))))) (OprApp (Ident y) (Ok "->") (Call (App (App (Ident compare) (Call (Ident x))) (Call (Ident y))))))))))"#);
    test_block!("sort by=(x-> x) 1",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (NamedApp (Ident sort) by (Group (OprApp (Ident x) (Ok "->") (Call (Ident x))))) (Number () "1" ()))))))"#);
    test_block!("foo to=",
        @"Operator must be applied to two operands: (BodyBlock #((ExpressionStatement () (Call (App (Ident foo) (Invalid))))))");
    test_module!("foo to=",
        @"Operator must be applied to two operands: (BodyBlock #((ExpressionStatement () (Call (App (Ident foo) (Invalid))))))");
    test_block!("(foo to=)",
        @"Operator must be applied to two operands: (BodyBlock #((ExpressionStatement () (Group (Call (App (Ident foo) (Invalid)))))))");
    test_block!("filter (foo to=(1))",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (Ident filter) (Group (Call (NamedApp (Ident foo) to (Group (Number () "1" ()))))))))))"#);
    test_block!("foo . bar baz=quux",
        @"(BodyBlock #((ExpressionStatement () (Call (NamedApp (PropertyAccess (Call (Ident foo)) bar) baz (Call (Ident quux)))))))");
}

// === Default arguments ===

#[test]
fn default_app() {
    test_block!("f default",
        @"(BodyBlock #((ExpressionStatement () (Call (App (Ident f) (Call (Ident default)))))))");
}

#[test]
fn argument_named_default() {
    test_module!("f default x = x",
        @"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident default) () ()) (() (Ident x) () ())) () (Call (Ident x)))))");
    test_module!("f x default = x",
        @"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) () ()) (() (Ident default) () ())) () (Call (Ident x)))))");
}

#[test]
fn complex_arguments() {
    test_module!("f x=1 = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) () ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f (x : Number) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ())) () (Call (Ident x)))))"#);
    test_module!("f (x = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) () ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f ((x = 1) : Number) = x",
        @r#"Expected identifier or wildcard in argument binding: (BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) (":" (Ident Number)) ())) () (Call (Ident x)))))"#);
    test_module!("f (x=1 : Number) = x",
        @r#"Expected identifier or wildcard in argument binding: (BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) (":" (Ident Number)) ())) () (Call (Ident x)))))"#);
    test_module!("f (x : Number = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f (x y) = x",
        @"Expected identifier or wildcard in argument binding: (BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) () ())) () (Call (Ident x)))))");
    test_module!("f ((x : Number) = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f ((x : Array Number) = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (App (Ident Array) (Ident Number))) ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f (x):Number=1 = x",
        @r#"Expected identifier or wildcard in argument binding: (BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) (":" (Ident Number)) ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f ((x:Number=1)) = x",
        @"Expected identifier or wildcard in argument binding: (BodyBlock #((Function () #() () () (Ident f) #((() (Invalid) () ())) () (Call (Ident x)))))");
    test_module!("f (x : Number)=1 = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f (x:Number = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f (x:Number=1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f x:Number=1 = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #((() (Ident x) (":" (Ident Number)) ((Number () "1" ())))) () (Call (Ident x)))))"#);
    // Pattern in LHS:
    test_module!("f ~x=1 = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #(("~" (Ident x) () ((Number () "1" ())))) () (Call (Ident x)))))"#);
    test_module!("f (~x = 1) = x",
        @r#"(BodyBlock #((Function () #() () () (Ident f) #(("~" (Ident x) () ((Number () "1" ())))) () (Call (Ident x)))))"#);
}

// === Code Blocks ===

#[test]
fn code_block_body() {
    test_module!(["main =", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((ExpressionStatement () (Call (Ident x))))))))");
    test_module!(["main =", "      ", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #(() (ExpressionStatement () (Call (Ident x))))))))");
    test_module!(["main =", "    ", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #(() (ExpressionStatement () (Call (Ident x))))))))");
    test_module!(["main =", "  ", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #(() (ExpressionStatement () (Call (Ident x))))))))");
    test_module!(["main =", "", "    x"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #(() (ExpressionStatement () (Call (Ident x))))))))");
}

#[test]
fn operator_block() {
    test_block!(["value = nums", "    * each random", "    + constant"].join("\n"),
        @r#"(BodyBlock #((Assignment () (Ident value) (OperatorBlockApplication (Call (Ident nums)) #(((Ok "*") (Call (App (Ident each) (Call (Ident random))))) ((Ok "+") (Call (Ident constant)))) #()))))"#);
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
    test_block!(["rect1", "    . width * 7", "    . abs", "        + x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (Call (Ident rect1)) #(((Ok ".") (OprApp (Call (Ident width)) (Ok "*") (Number () "7" ()))) ((Ok ".") (OperatorBlockApplication (Call (Ident abs)) #(((Ok "+") (Call (Ident x)))) #()))) #()))))"#);
    test_block!("rect1\n    . width = 7",
        @r#"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (OperatorBlockApplication (Call (Ident rect1)) #(((Ok ".") (Invalid))) #()))))"#);
}

#[test]
fn argument_blocks() {
    test_block!("foo\n    bar",
        @"(BodyBlock #((ExpressionStatement () (ArgumentBlockApplication (Ident foo) #((Call (Ident bar)))))))");
    test_block!("value = foo\n    bar",
        @"(BodyBlock #((Assignment () (Ident value) (ArgumentBlockApplication (Ident foo) #((Call (Ident bar)))))))");
    test_block!(["value = foo", "    +x", "    bar"].join("\n"),
        @r#"(BodyBlock #((Assignment () (Ident value) (ArgumentBlockApplication (Ident foo) #((OprApp () (Ok "+") (Call (Ident x))) (Call (Ident bar)))))))"#);
}

#[test]
fn code_block_empty() {
    // The first line here should parse as a function with no body expression (which is an error).
    // No input would parse as an empty `ArgumentBlock` or `OperatorBlock`, because those types are
    // distinguished from a body continuation by the presence of non-empty indented lines.
    test_module!("foo =\nbar",
        @"(BodyBlock #((Function () #() () () (Ident foo) #() () ()) (ExpressionStatement () (Call (Ident bar)))))");
    // This parses similarly to above; a line with no non-whitespace content does not create a code
    // block.
    test_module!(["foo =", "    ", "bar"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident foo) #() () ()) () (ExpressionStatement () (Call (Ident bar)))))");
}

#[test]
fn code_block_bad_indents() {
    test_module!(["main =", "  foo", " bar", "  baz"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((ExpressionStatement () (Call (Ident foo))) (ExpressionStatement () (Call (Ident bar))) (ExpressionStatement () (Call (Ident baz))))))))");
    test_module!(["main =", "  foo", " bar", "baz"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((ExpressionStatement () (Call (Ident foo))) (ExpressionStatement () (Call (Ident bar)))))) (ExpressionStatement () (Call (Ident baz)))))");
}

#[test]
fn code_block_with_following_statement() {
    test_module!(["main =", "    foo", "bar"].join("\n"),
        @"(BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((ExpressionStatement () (Call (Ident foo)))))) (ExpressionStatement () (Call (Ident bar)))))");
}

#[test]
fn operator_block_nested() {
    test_block!(["foo", "    + bar", "        - baz"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (Call (Ident foo)) #(((Ok "+") (OperatorBlockApplication (Call (Ident bar)) #(((Ok "-") (Call (Ident baz)))) #()))) #()))))"#);
}

#[test]
fn operator_section_in_operator_block() {
    test_block!(["foo", "    + bar +"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (OperatorBlockApplication (Call (Ident foo)) #(((Ok "+") (OprApp (Call (Ident bar)) (Ok "+") ()))) #()))))"#);
}

#[test]
fn first_line_indented() {
    test_module!(" a",
        @"(BodyBlock #((BodyBlock #((ExpressionStatement () (Call (Ident a)))))))");
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
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Call (Ident x)) (Ok "*") (Call (Ident y))) (Ok "+") (Call (Ident z))))))"#);
    test_block!("x + y * z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident x)) (Ok "+") (OprApp (Call (Ident y)) (Ok "*") (Call (Ident z)))))))"#);
    test_block!("w + x + y * z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Call (Ident w)) (Ok "+") (Call (Ident x))) (Ok "+") (OprApp (Call (Ident y)) (Ok "*") (Call (Ident z)))))))"#);
    test_block!("x - 1 + 2",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Call (Ident x)) (Ok "-") (Number () "1" ())) (Ok "+") (Number () "2" ())))))"#);
    test_block!("x+y * z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident x)) (Ok "+") (OprApp (Call (Ident y)) (Ok "*") (Call (Ident z)))))))"#);
}

#[test]
fn dot_operator_precedence() {
    test_block!("x y . f v",
        @"(BodyBlock #((ExpressionStatement () (Call (App (PropertyAccess (Call (App (Ident x) (Call (Ident y)))) f) (Call (Ident v)))))))");
}

#[test]
fn right_associative_operators() {
    test_block!("x --> y ---> z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident x)) (Ok "-->") (OprApp (Call (Ident y)) (Ok "--->") (Call (Ident z)))))))"#);
    test_block!("x <| y <<| z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident x)) (Ok "<|") (OprApp (Call (Ident y)) (Ok "<<|") (Call (Ident z)))))))"#);
}

#[test]
fn left_associative_operators() {
    test_block!("x + y + z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Call (Ident x)) (Ok "+") (Call (Ident y))) (Ok "+") (Call (Ident z))))))"#);
}

#[test]
fn pipeline_operators() {
    test_block!("f <| a",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident f)) (Ok "<|") (Call (Ident a))))))"#);
    test_block!("a |> f",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident a)) (Ok "|>") (Call (Ident f))))))"#);
}

#[test]
fn operator_sections() {
    test_block!(".map (+2 * 3) *7",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (App (PropertyAccess () map) (Group (OprApp (OprApp () (Ok "+") (Number () "2" ())) (Ok "*") (Number () "3" ())))) (OprApp () (Ok "*") (Number () "7" ())))))))"#);
    test_block!(".sum 1",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (PropertyAccess () sum) (Number () "1" ()))))))"#);
    test_block!("+1 + x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp () (Ok "+") (Number () "1" ())) (Ok "+") (Call (Ident x))))))"#);
    test_block!("increment = 1 +",
        @r#"(BodyBlock #((Assignment () (Ident increment) (OprApp (Number () "1" ()) (Ok "+") ()))))"#);
    test_block!("1+ << 2*",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (Number () "1" ()) (Ok "+") ()) (Ok "<<") (OprApp (Number () "2" ()) (Ok "*") ())))))"#);
    test_block!("1+1+ << 2*2*",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (OprApp (Number () "1" ()) (Ok "+") (Number () "1" ())) (Ok "+") ()) (Ok "<<") (OprApp (OprApp (Number () "2" ()) (Ok "*") (Number () "2" ())) (Ok "*") ())))))"#);
    test_block!("+1 << *2",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp () (Ok "+") (Number () "1" ())) (Ok "<<") (OprApp () (Ok "*") (Number () "2" ()))))))"#);
    test_block!("+1+1 << *2*2",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (OprApp (OprApp () (Ok "+") (Number () "1" ())) (Ok "+") (Number () "1" ())) (Ok "<<") (OprApp (OprApp () (Ok "*") (Number () "2" ())) (Ok "*") (Number () "2" ()))))))"#);
}

#[test]
fn template_functions() {
    test_block!("_.map (_ + 2*3) _*7",
        @r#"(BodyBlock #((ExpressionStatement () (TemplateFunction (Call (App (App (PropertyAccess (Wildcard) map) (Group (TemplateFunction (OprApp (Wildcard) (Ok "+") (OprApp (Number () "2" ()) (Ok "*") (Number () "3" ())))))) (TemplateFunction (OprApp (Wildcard) (Ok "*") (Number () "7" ())))))))))"#);
    test_block!("_.sum 1",
        @r#"(BodyBlock #((ExpressionStatement () (TemplateFunction (Call (App (PropertyAccess (Wildcard) sum) (Number () "1" ())))))))"#);
    test_block!("_+1 + x",
        @r#"(BodyBlock #((ExpressionStatement () (TemplateFunction (OprApp (OprApp (Wildcard) (Ok "+") (Number () "1" ())) (Ok "+") (Call (Ident x)))))))"#);
}

// === Unary Operators ===

#[test]
fn unevaluated_argument() {
    test_module!("main ~foo = x",
        @r#"(BodyBlock #((Function () #() () () (Ident main) #(("~" (Ident foo) () ())) () (Call (Ident x)))))"#);
}

#[test]
fn unary_operator_missing_operand() {
    test_module!("main ~ = x",
        @r#"Expected identifier or wildcard in argument binding: (BodyBlock #((Function () #() () () (Ident main) #(("~" (Invalid) () ())) () (Call (Ident x)))))"#);
}

#[test]
fn unary_operator_at_end_of_expression() {
    test_block!("foo ~",
        @"Operator must be applied to an operand: (BodyBlock #((ExpressionStatement () (Call (App (Ident foo) (Invalid))))))");
}

#[test]
fn unspaced_operator_sequence() {
    // Add a negated value.
    test_block!("x = y+-z",
        @r#"(BodyBlock #((Assignment () (Ident x) (OprApp (Call (Ident y)) (Ok "+") (UnaryOprApp "-" (Call (Ident z)))))))"#);
    // Create an operator section that adds a negated value to its input.
    test_block!("x = +-z",
        @r#"(BodyBlock #((Assignment () (Ident x) (OprApp () (Ok "+") (UnaryOprApp "-" (Call (Ident z)))))))"#);
    // The `-` can only be lexed as a unary operator, and unary operators cannot form sections.
    test_module!("main =\n    x = y+-",
        @r#"Operator must be applied to an operand: (BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((Assignment () (Ident x) (OprApp (Call (Ident y)) (Ok "+") (Invalid))))))))"#);
    // Assign a negative number to x.
    test_block!("x=-1",
        @r#"(BodyBlock #((Assignment () (Ident x) (UnaryOprApp "-" (Number () "1" ())))))"#);
    // Assign a negated value to x.
    test_block!("x=-y",
        @r#"(BodyBlock #((Assignment () (Ident x) (UnaryOprApp "-" (Call (Ident y))))))"#);
}

#[test]
fn minus_binary() {
    test_block!("x - x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident x)) (Ok "-") (Call (Ident x))))))"#);
    test_block!("x-x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident x)) (Ok "-") (Call (Ident x))))))"#);
    test_block!("x-1",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Call (Ident x)) (Ok "-") (Number () "1" ())))))"#);
    test_block!("(x)-1",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Group (Call (Ident x))) (Ok "-") (Number () "1" ())))))"#);
}

#[test]
fn minus_section() {
    test_block!("- x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp () (Ok "-") (Call (Ident x))))))"#);
    test_block!("(- x)",
        @r#"(BodyBlock #((ExpressionStatement () (Group (OprApp () (Ok "-") (Call (Ident x)))))))"#);
    test_block!("- (x * x)",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp () (Ok "-") (Group (OprApp (Call (Ident x)) (Ok "*") (Call (Ident x))))))))"#);
}

#[test]
fn minus_unary() {
    test_block!("f -x",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (Ident f) (UnaryOprApp "-" (Call (Ident x))))))))"#);
    test_block!("-x",
        @r#"(BodyBlock #((ExpressionStatement () (UnaryOprApp "-" (Call (Ident x))))))"#);
    test_block!("(-x)",
        @r#"(BodyBlock #((ExpressionStatement () (Group (UnaryOprApp "-" (Call (Ident x)))))))"#);
    test_block!("-(x * x)",
        @r#"(BodyBlock #((ExpressionStatement () (UnaryOprApp "-" (Group (OprApp (Call (Ident x)) (Ok "*") (Call (Ident x))))))))"#);
    test_block!("x=-x",
        @r#"(BodyBlock #((Assignment () (Ident x) (UnaryOprApp "-" (Call (Ident x))))))"#);
    test_block!("-x+x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (UnaryOprApp "-" (Call (Ident x))) (Ok "+") (Call (Ident x))))))"#);
    test_block!("-x*x",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (UnaryOprApp "-" (Call (Ident x))) (Ok "*") (Call (Ident x))))))"#);
}

#[test]
fn minus_unary_decimal() {
    test_block!("-2.1",
        @r#"(BodyBlock #((ExpressionStatement () (UnaryOprApp "-" (Number () "2" ("1"))))))"#);
}

#[test]
fn minus_unary_in_method_app() {
    test_block!("-1.x",
        @r#"(BodyBlock #((ExpressionStatement () (Call (PropertyAccess (UnaryOprApp "-" (Number () "1" ())) x)))))"#);
    test_block!("-1.up_to 100",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (PropertyAccess (UnaryOprApp "-" (Number () "1" ())) up_to) (Number () "100" ()))))))"#);
}

#[test]
fn method_app_in_minus_unary() {
    test_block!("-Number.positive_infinity",
        @r#"(BodyBlock #((ExpressionStatement () (UnaryOprApp "-" (Call (PropertyAccess (Call (Ident Number)) positive_infinity))))))"#);
}

#[test]
fn autoscope_operator() {
    test_module!("x : ..True",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident x) ":" (AutoscopedIdentifier ".." True)))))"#);
    test_block!("x = ..True",
        @r#"(BodyBlock #((Assignment () (Ident x) (AutoscopedIdentifier ".." True))))"#);
    test_block!("x = f ..True",
        @r#"(BodyBlock #((Assignment () (Ident x) (Call (App (Ident f) (AutoscopedIdentifier ".." True))))))"#);
    test_block!("x = ..not_a_constructor",
        @"The auto-scope operator may only be applied to a capitalized identifier: (BodyBlock #((Assignment () (Ident x) (Invalid))))");
    test_block!("x = case a of ..True -> True",
        @r#"Expression invalid in a pattern: (BodyBlock #((Assignment () (Ident x) (CaseOf (Call (Ident a)) #(((() (Invalid) "->" (Call (Ident True)))))))))"#);
    test_block!("x = ..4",
        @"Space required between terms: (BodyBlock #((Assignment () (Ident x) (Invalid))))");
    test_block!("x = ..Foo.Bar",
        @r#"(BodyBlock #((Assignment () (Ident x) (Call (PropertyAccess (AutoscopedIdentifier ".." Foo) Bar)))))"#);
    test_block!("x = f .. True",
        @"The autoscope operator must be applied to an identifier: (BodyBlock #((Assignment () (Ident x) (Call (App (App (Ident f) (Invalid)) (Call (Ident True)))))))");
    test_block!("x = f (.. ..)",
        @"The autoscope operator must be applied to an identifier: (BodyBlock #((Assignment () (Ident x) (Call (App (Ident f) (Group (Call (App (Invalid) (Invalid)))))))))");
    test_block!("x = f (.. *)",
        @r#"The autoscope operator must be applied to an identifier: (BodyBlock #((Assignment () (Ident x) (Call (App (Ident f) (Group (OprApp (Invalid) (Ok "*") ())))))))"#);
    test_block!("x = f (.. True)",
        @"The autoscope operator must be applied to an identifier: (BodyBlock #((Assignment () (Ident x) (Call (App (Ident f) (Group (Call (App (Invalid) (Call (Ident True))))))))))");
    test_block!("x = True..",
        @"Space required between terms: (BodyBlock #((Assignment () (Ident x) (Invalid))))");
    test_block!("x = True..True",
        @"Space required between terms: (BodyBlock #((Assignment () (Ident x) (Invalid))))");
    test_block!("x = ..",
        @"The autoscope operator must be applied to an identifier: (BodyBlock #((Assignment () (Ident x) (Invalid))))");
    test_block!("x = .. True",
        @"The autoscope operator must be applied to an identifier: (BodyBlock #((Assignment () (Ident x) (Call (App (Invalid) (Call (Ident True)))))))");
    test_block!("x : .. True",
        @r#"The autoscope operator must be applied to an identifier: (BodyBlock #((ExpressionStatement () (TypeAnnotated (Call (Ident x)) ":" (App (Invalid) (Ident True))))))"#);
}

// === Import/Export ===

#[test]
fn import() {
    test_module!("import project.IO",
        @"(BodyBlock #((Import () () ((Ident import) (PropertyAccess (Ident project) IO)) () () ())))");
    test_module!("import Standard.Base as Enso_List",
        @"(BodyBlock #((Import () () ((Ident import) (PropertyAccess (Ident Standard) Base)) () ((Ident as) (Ident Enso_List)) ())))");
    test_module!("from Standard.Base import Foo",
        @"(BodyBlock #((Import () ((Ident from) (PropertyAccess (Ident Standard) Base)) ((Ident import) (Ident Foo)) () () ())))");
    test_module!("from Standard.Base import all",
        @"(BodyBlock #((Import () ((Ident from) (PropertyAccess (Ident Standard) Base)) ((Ident import) ()) all () ())))");
    test_module!("from Standard.Base import all hiding Number, Boolean",
        @r#"(BodyBlock #((Import () ((Ident from) (PropertyAccess (Ident Standard) Base)) ((Ident import) ()) all () ((Ident hiding) (OprApp (Ident Number) (Ok ",") (Ident Boolean))))))"#);
    test_module!("polyglot java import java.lang.Float",
        @"(BodyBlock #((Import ((Ident polyglot) (Ident java)) () ((Ident import) (PropertyAccess (PropertyAccess (Ident java) lang) Float)) () () ())))");
    test_module!("polyglot java import java.net.URI as Java_URI",
        @"(BodyBlock #((Import ((Ident polyglot) (Ident java)) () ((Ident import) (PropertyAccess (PropertyAccess (Ident java) net) URI)) () ((Ident as) (Ident Java_URI)) ())))");
    test_module!("from Standard.Base import Foo, Bar, Baz",
        @r#"(BodyBlock #((Import () ((Ident from) (PropertyAccess (Ident Standard) Base)) ((Ident import) (OprApp (OprApp (Ident Foo) (Ok ",") (Ident Bar)) (Ok ",") (Ident Baz))) () () ())))"#);
    test_module!("from Standard.Base.Data.Array import new as array_new",
        @"Expected identifier: (BodyBlock #((Import () ((Ident from) (PropertyAccess (PropertyAccess (PropertyAccess (Ident Standard) Base) Data) Array)) ((Ident import) (Invalid)) () () ())))");
}

#[test]
fn export() {
    test_module!("export prj.Data.Foo",
        @"(BodyBlock #((Export () ((Ident export) (PropertyAccess (PropertyAccess (Ident prj) Data) Foo)) ())))");
    test_module!("export Foo as Bar",
        @"(BodyBlock #((Export () ((Ident export) (Ident Foo)) ((Ident as) (Ident Bar)))))");
    test_module!("from Foo export Bar, Baz",
        @r#"(BodyBlock #((Export ((Ident from) (Ident Foo)) ((Ident export) (OprApp (Ident Bar) (Ok ",") (Ident Baz))) ())))"#);
    test_module!("from Foo export all hiding Bar, Baz",
        @r#""all" not allowed in export statement: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
    test_module!("from Foo export all",
        @r#""all" not allowed in export statement: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
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
    test_module!(code, @"(BodyBlock #((ExpressionStatement () (Call (Ident x))) ()))");
}

#[test]
fn metadata_parsing() {
    let code = metadata::ORDERS_WITH_METADATA;
    let (meta, code) = enso_parser::metadata::parse(code).unwrap();
    let _ast = parse_module(code);
    let _meta: enso_parser::metadata::Metadata = meta.unwrap();
}

// === Type annotations and signatures ===

#[test]
fn type_signatures() {
    test_module!("val : Bool",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident val) ":" (Ident Bool)))))"#);
    test_block!("val : Bool\nval",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident val) ":" (Ident Bool))) (ExpressionStatement () (Call (Ident val)))))"#);
    test_block!("val : Bool",
        @r#"(BodyBlock #((ExpressionStatement () (TypeAnnotated (Call (Ident val)) ":" (Ident Bool)))))"#);
    test_module!("val : Bool\nval = True",
        @r#"(BodyBlock #((Function () #() ((Ident val) ":" (Ident Bool)) () (Ident val) #() () (Call (Ident True)))))"#);
    test_module!("val : Bool\n\nval = True",
        @r#"(BodyBlock #((Function () #() ((Ident val) ":" (Ident Bool)) () (Ident val) #() () (Call (Ident True)))))"#);
    test_module!("val : Bool\n\n\nval = True",
        @r#"(BodyBlock #((Function () #() ((Ident val) ":" (Ident Bool)) () (Ident val) #() () (Call (Ident True)))))"#);
    test_module!("val : Bool\ndifferent_name = True",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident val) ":" (Ident Bool))) (Function () #() () () (Ident different_name) #() () (Call (Ident True)))))"#);
    test_module!("val : List Int",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident val) ":" (App (Ident List) (Ident Int))))))"#);
    test_module!("foo : [Integer | Text] -> (Integer | Text)",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident foo) ":" (OprApp (Array (OprApp (Ident Integer) (Ok "|") (Ident Text)) #()) (Ok "->") (Group (OprApp (Ident Integer) (Ok "|") (Ident Text))))))))"#);
    test_module!("f a (b : Int) : Double",
        @r#"(BodyBlock #((TypeAnnotated (App (App (Ident f) (Call (Ident a))) (Group (TypeAnnotated (Call (Ident b)) ":" (Ident Int)))) ":" (Ident Double))))"#);
    test_module!("f a (b = 1 : Int) : Double",
        @r#"(BodyBlock #((TypeAnnotated (NamedApp (App (Ident f) (Call (Ident a))) b (TypeAnnotated (Number () "1" ()) ":" (Ident Int))) ":" (Ident Double))))"#);
}

#[test]
fn type_annotations() {
    test_block!("val = x : Int",
        @r#"(BodyBlock #((Assignment () (Ident val) (TypeAnnotated (Call (Ident x)) ":" (Ident Int)))))"#);
    test_block!("val = x : A : B : C",
        @r#"(BodyBlock #((Assignment () (Ident val) (TypeAnnotated (TypeAnnotated (TypeAnnotated (Call (Ident x)) ":" (Ident A)) ":" (Ident B)) ":" (Ident C)))))"#);
    test_block!("val = foo (x : Int)",
        @r#"(BodyBlock #((Assignment () (Ident val) (Call (App (Ident foo) (Group (TypeAnnotated (Call (Ident x)) ":" (Ident Int))))))))"#);
    test_block!("(x : My_Type _)",
        @r#"(BodyBlock #((ExpressionStatement () (Group (TypeAnnotated (Call (Ident x)) ":" (App (Ident My_Type) (TemplateFunction (Wildcard))))))))"#);
    test_module!("x : List Int -> Int",
        @r#"(BodyBlock #((TypeSignatureDeclaration ((Ident x) ":" (OprApp (App (Ident List) (Ident Int)) (Ok "->") (Ident Int))))))"#);
    test_module!("p:Plus + m:Plus",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (TypeAnnotated (Call (Ident p)) ":" (Ident Plus)) (Ok "+") (TypeAnnotated (Call (Ident m)) ":" (Ident Plus))))))"#);
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
    test_block!(r#"`"#, @"Unexpected token: (BodyBlock #((ExpressionStatement () (Invalid))))");
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
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "part of the string") (Newline) (Section "   3-spaces indented line, part of the Text Block") (Newline) (Section "this does not end the string -> '''") (Newline) (Newline) (Section "`also` part of the string")))) () (ExpressionStatement () (Call (Ident x)))))"#);
    test_block!(r#""""
    multiline string that doesn't end in a newline
x"#,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "multiline string that doesn't end in a newline")))) (ExpressionStatement () (Call (Ident x)))))"#);
    test_block!("x = \"\"\"\n    Indented multiline\nx",
        @r#"(BodyBlock #((Assignment () (Ident x) (TextLiteral #((Section "Indented multiline")))) (ExpressionStatement () (Call (Ident x)))))"#);
    test_block!("'''\n    \\nEscape at start\n",
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Escape 10) (Section "Escape at start")))) ()))"#);
    test_module!("x =\n x = '''\n  x\nx",
        @r#"(BodyBlock #((Function () #() () () (Ident x) #() () (BodyBlock #((Assignment () (Ident x) (TextLiteral #((Section "x"))))))) (ExpressionStatement () (Call (Ident x)))))"#);
    test_block!("foo = bar '''\n baz",
        @r#"(BodyBlock #((Assignment () (Ident foo) (Call (App (Ident bar) (TextLiteral #((Section "baz"))))))))"#);
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
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "With a ") (Splice (Call (Ident splice))) (Section "."))))))"#);
    test_block!(r#"'` SpliceWithLeadingWhitespace`'"#,
        @"(BodyBlock #((ExpressionStatement () (TextLiteral #((Splice (Call (Ident SpliceWithLeadingWhitespace))))))))");
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
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Splice (Call (Ident splice))) (Section " at start"))))))"#);
    let code = r#"'''
    text with a `splice`
    and some \u000Aescapes\'"#;
    test_block!(code,
        @r#"(BodyBlock #((ExpressionStatement () (TextLiteral #((Section "text with a ") (Splice (Call (Ident splice))) (Newline) (Section "and some ") (Escape 10) (Section "escapes") (Escape 39))))))"#);
}

// === Lambdas ===

#[test]
fn new_lambdas() {
    test_block!(r#"\v-> v"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Call (Ident v))))))"#);
    test_block!(r#"\ v -> v"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Call (Ident v))))))"#);
    test_block!(r#"\v -> v"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Call (Ident v))))))"#);
    test_block!(r#"\ v-> v"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Call (Ident v))))))"#);
    test_block!(r#"\ x -> x + y"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident x) () ())) "->" (OprApp (Call (Ident x)) (Ok "+") (Call (Ident y)))))))"#);
    test_block!("\\v->\n    v",
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (BodyBlock #((ExpressionStatement () (Call (Ident v)))))))))"#);
    test_block!("\\ v ->\n    v",
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (BodyBlock #((ExpressionStatement () (Call (Ident v)))))))))"#);
    test_block!("f \\ v ->\n    v",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (Ident f) (Lambda "\\" #((() (Ident v) () ())) "->" (BodyBlock #((ExpressionStatement () (Call (Ident v)))))))))))"#);
    test_block!(r#"\a b -> x"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident a) () ()) (() (Ident b) () ())) "->" (Call (Ident x))))))"#);
    test_block!(r#"\~x -> x"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #(("~" (Ident x) () ())) "->" (Call (Ident x))))))"#);
    test_block!(r#"\a (b = f _ 1) -> f a"#,
        @r#"(BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident a) () ()) (() (Ident b) () ((Call (App (App (Ident f) (TemplateFunction (Wildcard))) (Number () "1" ())))))) "->" (Call (App (Ident f) (Call (Ident a))))))))"#);
    test_block!("\\",
        @"Invalid macro invocation: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("\\ v",
        @"Invalid macro invocation: (BodyBlock #((ExpressionStatement () (Call (App (Invalid) (Call (Ident v)))))))");
    test_block!("\\v",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("\\v->",
        @r#"Expected tokens: (BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Invalid)))))"#);
    test_block!("\\v->\n",
        @r#"Expected tokens: (BodyBlock #((ExpressionStatement () (Lambda "\\" #((() (Ident v) () ())) "->" (Invalid))) ()))"#);
    test_block!("\\v->\nv",
        @"This expression would define an unused function: (BodyBlock #((Invalid) (ExpressionStatement () (Call (Ident v)))))");
}

#[test]
fn old_lambdas() {
    test_block!("x -> y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (Call (Ident y))))))"#);
    test_block!("x->y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (Call (Ident y))))))"#);
    test_block!("x-> y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (Call (Ident y))))))"#);
    test_block!("x-> x + y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (OprApp (Call (Ident x)) (Ok "+") (Call (Ident y)))))))"#);
    test_block!("x->\n y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (BodyBlock #((ExpressionStatement () (Call (Ident y)))))))))"#);
    test_block!("x ->\n y",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (BodyBlock #((ExpressionStatement () (Call (Ident y)))))))))"#);
    test_block!("f x->\n y",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (Ident f) (OprApp (Ident x) (Ok "->") (BodyBlock #((ExpressionStatement () (Call (Ident y)))))))))))"#);
    test_block!("x->y-> z",
        @r#"(BodyBlock #((ExpressionStatement () (OprApp (Ident x) (Ok "->") (OprApp (Ident y) (Ok "->") (Call (Ident z)))))))"#);
    test_block!("foo = x -> (y = bar x) -> x + y",
        @r#"(BodyBlock #((Assignment () (Ident foo) (OprApp (Ident x) (Ok "->") (OprApp (Group (OprApp (Ident y) (Ok "=") (Call (App (Ident bar) (Call (Ident x)))))) (Ok "->") (OprApp (Call (Ident x)) (Ok "+") (Call (Ident y))))))))"#);
}

// === Pattern Matching ===

#[test]
fn pattern_irrefutable() {
    test_block!("Point x_val = my_point",
        @"(BodyBlock #((Assignment () (App (Ident Point) (Ident x_val)) (Call (Ident my_point)))))");
    test_block!("Vector _ = x",
        @"(BodyBlock #((Assignment () (App (Ident Vector) (Wildcard)) (Call (Ident x)))))");
    test_block!("X.y = z",
        @"(BodyBlock #((Function () #() () () (PropertyAccess (Ident X) y) #() () (Call (Ident z)))))");
}

#[test]
fn pattern_invalid() {
    test_block!("x + y = z",
        @"Expected identifier or wildcard in argument binding: (BodyBlock #((Function () #() () () (Ident x) #((() (Invalid) () ()) (() (Ident y) () ())) () (Call (Ident z)))))");
    test_module!("x + y = z",
        @"Expected identifier or wildcard in argument binding: (BodyBlock #((Function () #() () () (Ident x) #((() (Invalid) () ()) (() (Ident y) () ())) () (Call (Ident z)))))");
    test_block!("(x y) = z",
        @"Expression invalid in a pattern: (BodyBlock #((Assignment () (Group (Invalid)) (Call (Ident z)))))");
    test_module!("(x y) = z",
        @"Unexpected variable assignment in module statement: (BodyBlock #((Invalid)))");
}

#[test]
fn case_expression() {
    test_block!(["case a of", "    Some -> x", "    Int -> x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident a)) #(((() (Ident Some) "->" (Call (Ident x)))) ((() (Ident Int) "->" (Call (Ident x)))))))))"#);
    test_block!(["case a of", "    Vector_2d x y -> x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident a)) #(((() (App (App (Ident Vector_2d) (Ident x)) (Ident y)) "->" (Call (Ident x)))))))))"#);
    test_block!(["case self of", "    Vector_2d -> x", "    _ -> x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident self)) #(((() (Ident Vector_2d) "->" (Call (Ident x)))) ((() (Wildcard) "->" (Call (Ident x)))))))))"#);
    test_block!(["case foo of", "    v:My_Type -> x", "    v:(My_Type _ _) -> x"].join("\n"),
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident foo)) #(((() (TypeAnnotated (Ident v) ":" (Ident My_Type)) "->" (Call (Ident x)))) ((() (TypeAnnotated (Ident v) ":" (Group (App (App (Ident My_Type) (TemplateFunction (Wildcard))) (TemplateFunction (Wildcard))))) "->" (Call (Ident x)))))))))"#);
    test_block!("case a of\n    Platform.OS.Windows -> 42",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident a)) #(((() (PropertyAccess (PropertyAccess (Ident Platform) OS) Windows) "->" (Number () "42" ()))))))))"#);
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
    @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident a)) #(((((#((Section " The Some case"))) #()) () () ())) ((() (Ident Some) "->" (Call (Ident x)))) ((((#((Section " The Int case"))) #()) () () ())) ((() (Ident Int) "->" (Call (Ident x)))))))))"#);
}

#[test]
fn case_by_type() {
    macro_rules! test_case {
        ( $code:expr_2021, $($case:tt)* ) => {
            test_block!(&format!("case foo of\n {}", $code), $( $case )* );
        }
    }
    test_case!("f:A->B -> x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident foo)) #(((() (TypeAnnotated (Ident f) ":" (OprApp (Ident A) (Ok "->") (Ident B))) "->" (Call (Ident x)))))))))"#);
    test_case!("f : A->B -> x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident foo)) #(((() (TypeAnnotated (Ident f) ":" (OprApp (Ident A) (Ok "->") (Ident B))) "->" (Call (Ident x)))))))))"#);
    test_case!("v : A -> x->x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident foo)) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (OprApp (Ident x) (Ok "->") (Call (Ident x))))))))))"#);
    test_case!("v : A -> x -> x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident foo)) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (OprApp (Ident x) (Ok "->") (Call (Ident x))))))))))"#);
    test_case!("v:A->x->x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident foo)) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (OprApp (Ident x) (Ok "->") (Call (Ident x))))))))))"#);
    test_case!("v:A->x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident foo)) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (Call (Ident x)))))))))"#);
    test_case!("v : A -> _ + x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident foo)) #(((() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (TemplateFunction (OprApp (Wildcard) (Ok "+") (Call (Ident x)))))))))))"#);
}

#[test]
fn suspended_default_arguments_in_pattern() {
    test_block!("case self of\n    Vector_2d ... -> x",
        @r#"(BodyBlock #((ExpressionStatement () (CaseOf (Call (Ident self)) #(((() (App (Ident Vector_2d) (SuspendedDefaultArguments)) "->" (Call (Ident x)))))))))"#);
}

#[test]
fn suspended_default_arguments_in_expression() {
    test_block!("c = self.value ...",
        @"(BodyBlock #((Assignment () (Ident c) (Call (App (PropertyAccess (Call (Ident self)) value) (SuspendedDefaultArguments))))))");
    test_block!("c = self.value...",
        @"(BodyBlock #((Assignment () (Ident c) (Call (App (PropertyAccess (Call (Ident self)) value) (SuspendedDefaultArguments))))))");
}

// === Private (project-private) keyword ===

#[test]
fn private_keyword() {
    test_module!("private",
        @"(BodyBlock #((Private private)))");
    test_module!("private func",
        @r#"The "private" keyword cannot be applied to this type of symbol: (BodyBlock #((ExpressionStatement () (App (Invalid) (Call (Ident func))))))"#);
    test_module!("main =\n    private var = 42",
        @r#"The "private" keyword is not expected in this context: (BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((App (Invalid) (Assignment () (Ident var) (Number () "42" ()))))))))"#);
    test_module!("main =\n    private func x = 42",
        @r#"The "private" keyword is not expected in this context: (BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((App (Invalid) (Function () #() () () (Ident func) #((() (Ident x) () ())) () (Number () "42" ()))))))))"#);
    test_module!("private ConstructorOutsideType",
        @r#"The "private" keyword cannot be applied to this type of symbol: (BodyBlock #((ExpressionStatement () (App (Invalid) (Call (Ident ConstructorOutsideType))))))"#);
    test_module!("type My_Type\n    private",
        @r#"In a type definition, the "private" keyword can only be applied to a constructor or function definition: (BodyBlock #((TypeDef My_Type #() #((Invalid)))))"#);
    test_module!("private type My_Type\n    Ctor",
        @r#"The "private" keyword cannot be applied to this type of symbol: (BodyBlock #((App (Invalid) (TypeDef My_Type #() #((ConstructorDefinition () #() () Ctor #() #()))))))"#);
    test_module!("private type T",
        @r#"The "private" keyword cannot be applied to this type of symbol: (BodyBlock #((App (Invalid) (TypeDef T #() #()))))"#);
}

#[test]
fn private_methods() {
    test_module!("private method x = x",
        @"(BodyBlock #((Function () #() () private (Ident method) #((() (Ident x) () ())) () (Call (Ident x)))))");
    test_module!("private method =\n    42",
        @r#"(BodyBlock #((Function () #() () private (Ident method) #() () (BodyBlock #((ExpressionStatement () (Number () "42" ())))))))"#);
    test_module!("type T\n    private method x = x",
        @"(BodyBlock #((TypeDef T #() #((Function () #() () private (Ident method) #((() (Ident x) () ())) () (Call (Ident x)))))))");
}

// === Array/tuple literals ===

#[test]
fn array_literals() {
    test_block!("[]",
        @"(BodyBlock #((ExpressionStatement () (Array () #()))))");
    test_block!("[x]",
        @"(BodyBlock #((ExpressionStatement () (Array (Call (Ident x)) #()))))");
    test_block!("[x, y]",
        @r#"(BodyBlock #((ExpressionStatement () (Array (Call (Ident x)) #(("," (Call (Ident y))))))))"#);
    test_block!("[x, y, z]",
        @r#"(BodyBlock #((ExpressionStatement () (Array (Call (Ident x)) #(("," (Call (Ident y))) ("," (Call (Ident z))))))))"#);
    test_block!("[ x , y ]",
        @r#"(BodyBlock #((ExpressionStatement () (Array (Call (Ident x)) #(("," (Call (Ident y))))))))"#);
    test_block!("[ x , y , z ]",
        @r#"(BodyBlock #((ExpressionStatement () (Array (Call (Ident x)) #(("," (Call (Ident y))) ("," (Call (Ident z))))))))"#);
}

#[test]
fn tuple_literals() {
    test_block!("{}",
        @"(BodyBlock #((ExpressionStatement () (Tuple () #()))))");
    test_block!("{x}",
        @"(BodyBlock #((ExpressionStatement () (Tuple (Call (Ident x)) #()))))");
    test_block!("{x, y}",
        @r#"(BodyBlock #((ExpressionStatement () (Tuple (Call (Ident x)) #(("," (Call (Ident y))))))))"#);
}

// === Numeric literals ===

#[cfg(test)]
mod numbers {
    use super::*;

    #[test]
    fn with_decimal() {
        test_block!("pi = 3.14",
            @r#"(BodyBlock #((Assignment () (Ident pi) (Number () "3" ("14")))))"#);
    }

    #[test]
    fn digits_spaced_dot() {
        test_block!("1 . 0",
            @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
        test_block!("1 .0",
            @r#"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Call (App (Number () "1" ()) (Invalid))))))"#);
        test_block!("1. 0",
            @r#"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Call (App (Invalid) (Number () "0" ()))))))"#);
    }

    #[test]
    fn non_digits_dot_digits() {
        test_block!("x.0",
            @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
    }

    #[test]
    fn digits_dot_non_digits() {
        test_block!("0.0.x",
            @r#"(BodyBlock #((ExpressionStatement () (Call (PropertyAccess (Number () "0" ("0")) x)))))"#);
        test_block!("1.0.0",
            @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
        test_block!("1.0x",
            @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
        test_block!("876543.is_even.should_be_false",
            @r#"(BodyBlock #((ExpressionStatement () (Call (PropertyAccess (Call (PropertyAccess (Number () "876543" ()) is_even)) should_be_false)))))"#);
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
            @r#"(BodyBlock #((ExpressionStatement () (Number () "10_000" ("99")))))"#);
    }

    #[test]
    fn old_hex() {
        test_block!("16_17ffffffffffffffa",
            @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    }
}

// === Whitespace ===

#[test]
fn trailing_whitespace() {
    test_block!("a ", @"(BodyBlock #((ExpressionStatement () (Call (Ident a))) ()))");
    test_block!("a \n", @"(BodyBlock #((ExpressionStatement () (Call (Ident a))) ()))");
    test_module!("a = \n x",
        @"(BodyBlock #((Function () #() () () (Ident a) #() () (BodyBlock #((ExpressionStatement () (Call (Ident x))))))))");
}

// === Annotations ===

#[test]
fn at_operator() {
    test_block!("foo@bar",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_module!("foo@bar",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("foo @ bar",
        @"Operator must be applied to an operand: (BodyBlock #((ExpressionStatement () (Call (App (App (Ident foo) (Invalid)) (Call (Ident bar)))))))");
    test_module!("foo @ bar",
        @"Operator must be applied to an operand: (BodyBlock #((ExpressionStatement () (Call (App (App (Ident foo) (Invalid)) (Call (Ident bar)))))))");
}

#[test]
fn annotations() {
    test_module!("@on_problems P.g\nselect_columns : Text -> Table\nselect_columns text = to_table text",
        @r#"(BodyBlock #((Function () #(((on_problems (PropertyAccess (Call (Ident P)) g)) #(()))) ((Ident select_columns) ":" (OprApp (Ident Text) (Ok "->") (Ident Table))) () (Ident select_columns) #((() (Ident text) () ())) () (Call (App (Ident to_table) (Call (Ident text)))))))"#);
    test_module!("@a\n@b 1 + 1\nf x = x",
        @r#"(BodyBlock #((Function () #(((a ()) #(())) ((b (OprApp (Number () "1" ()) (Ok "+") (Number () "1" ()))) #(()))) () () (Ident f) #((() (Ident x) () ())) () (Call (Ident x)))))"#);
    test_module!("@x `\nid x = x",
        @"Unexpected token: (BodyBlock #((Function () #(((x (Invalid)) #(()))) () () (Ident id) #((() (Ident x) () ())) () (Call (Ident x)))))");
    test_module!("@` foo\nid x = x",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Call (App (Invalid) (Call (Ident foo))))) (Function () #() () () (Ident id) #((() (Ident x) () ())) () (Call (Ident x)))))");
}

#[test]
fn annotations_on_type_methods() {
    test_module!("type A\n @a z\n @b\n x y = x",
        @"(BodyBlock #((TypeDef A #() #((Function () #(((a (Ident z)) #(())) ((b ()) #(()))) () () (Ident x) #((() (Ident y) () ())) () (Call (Ident x)))))))");
}

#[test]
fn annotations_on_type_constructors() {
    test_module!("type A\n @a z\n @b\n Baz x",
        @"(BodyBlock #((TypeDef A #() #((ConstructorDefinition () #(((a (Ident z)) #(())) ((b ()) #(()))) () Baz #((() (Ident x) () ())) #())))))");
}

#[test]
fn inline_builtin_annotations() {
    test_block!("@Tail_Call go t",
        @"(BodyBlock #((ExpressionStatement () (AnnotatedBuiltin Tail_Call #() (Call (App (Ident go) (Call (Ident t))))))))");
    test_block!("@Tail_Call go (x = y)",
        @"(BodyBlock #((ExpressionStatement () (AnnotatedBuiltin Tail_Call #() (Call (NamedApp (Ident go) x (Call (Ident y))))))))");
    test_block!("@Tail_Call go\n a\n b",
        @"(BodyBlock #((ExpressionStatement () (AnnotatedBuiltin Tail_Call #() (ArgumentBlockApplication (Ident go) #((Call (Ident a)) (Call (Ident b))))))))");
    test_block!("map _-> @Tail_Call f",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (Ident map) (OprApp (Wildcard) (Ok "->") (AnnotatedBuiltin Tail_Call #() (Call (Ident f)))))))))"#);
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
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident FREEZE) (Call (Ident x))))))))");
    test_block!("FREEZE x + y",
        @r#"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident FREEZE) (OprApp (Call (Ident x)) (Ok "+") (Call (Ident y)))))))))"#);
    test_block!("FREEZE x.f",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident FREEZE) (Call (PropertyAccess (Call (Ident x)) f))))))))");
    test_block!("FREEZE x.f y",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident FREEZE) (Call (App (PropertyAccess (Call (Ident x)) f) (Call (Ident y))))))))))");
}

#[test]
fn skip() {
    test_block!("SKIP x",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident SKIP) (Call (Ident x))))))))");
    test_block!("SKIP x + y",
        @r#"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident SKIP) (OprApp (Call (Ident x)) (Ok "+") (Call (Ident y)))))))))"#);
    test_block!("SKIP x.f",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident SKIP) (Call (PropertyAccess (Call (Ident x)) f))))))))");
    test_block!("SKIP x.f y",
        @"(BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident SKIP) (Call (App (PropertyAccess (Call (Ident x)) f) (Call (Ident y))))))))))");
}

// === Context errors ===

#[test]
fn statement_in_expression_context() {
    test_block!("x = y = z",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((Assignment () (Ident x) (Invalid))))");
    test_block!("(y = z)",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Group (Invalid)))))");
    test_block!("(y = z) x",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Call (App (Group (Invalid)) (Call (Ident x)))))))");
    test_block!("(f x = x)",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Group (Invalid)))))");
    test_block!("y = f x = x",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((Assignment () (Ident y) (Invalid))))");
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
    test_block!("foo = if cond.x else.y",
        @"Invalid macro invocation: (BodyBlock #((Assignment () (Ident foo) (Call (App (App (Invalid) (Call (PropertyAccess (Call (Ident cond)) x))) (MultiSegmentApp #(((Ident else) (Call (PropertyAccess () y))))))))))");
}

#[test]
fn incomplete_type_definition() {
    test_module!("type", @"Expected type identifier in type declaration: (BodyBlock #((Invalid)))");
    test_block!("type", @"Expected type identifier in type declaration: (BodyBlock #((Invalid)))");
}

#[test]
fn bad_case() {
    test_block!("foo = case x of\n 4",
        @"Invalid case expression: (BodyBlock #((Assignment () (Ident foo) (Invalid))))");
    test_block!("foo = case x of\n 4 ->",
        @"Invalid case expression: (BodyBlock #((Assignment () (Ident foo) (Invalid))))");
    test_block!("foo = case x of\n 4->",
        @"Invalid case expression: (BodyBlock #((Assignment () (Ident foo) (Invalid))))");
}

#[test]
fn malformed_sequence() {
    test_block!("(1, )",
        @"Operator must be applied to two operands: (BodyBlock #((ExpressionStatement () (Group (Invalid)))))");
    test_block!("foo = (1, )",
        @"Operator must be applied to two operands: (BodyBlock #((Assignment () (Ident foo) (Group (Invalid)))))");
}

#[test]
fn unmatched_delimiter() {
    test_block!("(",
        @"Unclosed parenthesis in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!(")", @"Unmatched delimiter: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("[",
        @"Invalid macro invocation: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("]", @"Unmatched delimiter: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("foo = (",
        @"Unclosed parenthesis in expression: (BodyBlock #((Assignment () (Ident foo) (Invalid))))");
    test_module!("foo = (",
        @"Unclosed parenthesis in expression: (BodyBlock #((Function () #() () () (Ident foo) #() () (Invalid))))");
    test_block!("foo = )",
        @"Unmatched delimiter: (BodyBlock #((Assignment () (Ident foo) (Invalid))))");
    test_module!("foo = )",
        @"Unmatched delimiter: (BodyBlock #((Function () #() () () (Ident foo) #() () (Invalid))))");
    test_block!("foo = [",
        @"Invalid macro invocation: (BodyBlock #((Assignment () (Ident foo) (Invalid))))");
    test_module!("foo = [",
        @"Invalid macro invocation: (BodyBlock #((Function () #() () () (Ident foo) #() () (Invalid))))");
    test_block!("foo = ]",
        @"Unmatched delimiter: (BodyBlock #((Assignment () (Ident foo) (Invalid))))");
    test_module!("foo = ]",
        @"Unmatched delimiter: (BodyBlock #((Function () #() () () (Ident foo) #() () (Invalid))))");
}

#[test]
fn unexpected_special_operator() {
    test_block!("foo = 1, 2",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((Assignment () (Ident foo) (Invalid))))");
    test_module!("foo = 1, 2",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((Function () #() () () (Ident foo) #() () (Invalid))))");
    //test_block!("f <| x=2", @r#"Invalid use of syntactic operator in expression: "#);
}

#[test]
fn malformed_import() {
    test_module!("import",
        @r#"Expected name or "all" keyword following "import" keyword: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
    test_module!("import as Foo",
        @r#"Expected name or "all" keyword following "import" keyword: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
    test_module!("import Foo as Foo, Bar",
        @"Expected identifier: (BodyBlock #((Import () () ((Ident import) (Ident Foo)) () ((Ident as) (Invalid)) ())))");
    test_module!("import Foo as Foo.Bar",
        @"Expected identifier: (BodyBlock #((Import () () ((Ident import) (Ident Foo)) () ((Ident as) (Invalid)) ())))");
    test_module!("import Foo as",
        @"Expected tokens: (BodyBlock #((Import () () ((Ident import) (Ident Foo)) () ((Ident as) (Invalid)) ())))");
    test_module!("import Foo as Bar.Baz",
        @"Expected identifier: (BodyBlock #((Import () () ((Ident import) (Ident Foo)) () ((Ident as) (Invalid)) ())))");
    test_module!("import Foo hiding",
        @"Expected qualified name: (BodyBlock #((Import () () ((Ident import) (Invalid)) () () ())))");
    test_module!("import Foo hiding X,",
        @"Malformed comma-delimited sequence: (BodyBlock #((Import () () ((Ident import) (Invalid)) () () ())))");
    test_module!("polyglot import Foo",
        @"Expected tokens: (BodyBlock #((Import ((Ident polyglot) (Invalid)) () ((Ident import) (Ident Foo)) () () ())))");
    test_module!("polyglot java import",
        @r#"Expected name or "all" keyword following "import" keyword: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
    test_module!("from import all",
        @"Expected tokens: (BodyBlock #((Import () ((Ident from) (Invalid)) ((Ident import) ()) all () ())))");
    test_module!("from Foo import all hiding",
        @"Expected tokens: (BodyBlock #((Import () ((Ident from) (Ident Foo)) ((Ident import) ()) all () ((Ident hiding) (Invalid)))))");
    test_module!("from Foo import all hiding X.Y",
        @"Expected identifier: (BodyBlock #((Import () ((Ident from) (Ident Foo)) ((Ident import) ()) all () ((Ident hiding) (Invalid)))))");
    test_module!("export",
        @r#"Expected name following "export" keyword: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
    test_module!("export as Foo",
        @r#"Expected name following "export" keyword: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
    test_module!("export Foo as Foo, Bar",
        @"Expected identifier: (BodyBlock #((Export () ((Ident export) (Ident Foo)) ((Ident as) (Invalid)))))");
    test_module!("export Foo as Foo.Bar",
        @"Expected identifier: (BodyBlock #((Export () ((Ident export) (Ident Foo)) ((Ident as) (Invalid)))))");
    test_module!("export Foo as",
        @"Expected tokens: (BodyBlock #((Export () ((Ident export) (Ident Foo)) ((Ident as) (Invalid)))))");
    test_module!("export Foo as Bar.Baz",
        @"Expected identifier: (BodyBlock #((Export () ((Ident export) (Ident Foo)) ((Ident as) (Invalid)))))");
    test_module!("export Foo hiding",
        @"Expected qualified name: (BodyBlock #((Export () ((Ident export) (Invalid)) ())))");
    test_module!("export Foo hiding X,",
        @"Malformed comma-delimited sequence: (BodyBlock #((Export () ((Ident export) (Invalid)) ())))");
    test_module!("from export all",
        @r#""all" not allowed in export statement: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
    test_module!("from Foo export all hiding",
        @r#""all" not allowed in export statement: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
    test_module!("from Foo export all hiding X.Y",
        @r#""all" not allowed in export statement: (BodyBlock #((ExpressionStatement () (Invalid))))"#);
}

#[test]
fn invalid_token() {
    test_block!("`", @"Unexpected token: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("splice_outside_text = `",
        @"Unexpected token: (BodyBlock #((Assignment () (Ident splice_outside_text) (Invalid))))");
}

#[test]
fn illegal_foreign_body() {
    // Foreign is only a keyword on the LHS of an assignment operator.
    test_module!("foreign 4",
        @r#"(BodyBlock #((ExpressionStatement () (Call (App (Ident foreign) (Number () "4" ()))))))"#);
    test_module!("foreign foo = \"4\"",
        @"Expected function name in foreign function definition: (BodyBlock #((Invalid)))");
    test_module!("foreign js foo = 4",
        @"The body of a foreign function must be a text literal: (BodyBlock #((ForeignFunction js foo #() (Invalid))))");
}

#[test]
fn unexpected_tokens_in_inner_macro_segment() {
    test_module!("from Foo import all What_Is_This_Doing_Here hiding Bar",
        @"Unexpected tokens in macro invocation: (BodyBlock #((ExpressionStatement () (MultiSegmentApp #(((Ident from) (Call (Ident Foo))) ((Ident import) ()) ((Ident all) (Invalid)) ((Ident hiding) (Call (Ident Bar))))))))");
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
    test_block!("x = y +- z",
        @r#"Operator must be applied to an operand: (BodyBlock #((Assignment () (Ident x) (Call (App (App (Ident y) (OprApp () (Ok "+") (Invalid))) (Call (Ident z)))))))"#);
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
    test_block!("x = y +-",
        @r#"Operator must be applied to an operand: (BodyBlock #((Assignment () (Ident x) (Call (App (Ident y) (OprApp () (Ok "+") (Invalid)))))))"#);
    test_block!("x = +- z",
        @r#"Operator must be applied to an operand: (BodyBlock #((Assignment () (Ident x) (Call (App (OprApp () (Ok "+") (Invalid)) (Call (Ident z)))))))"#);
    expect_multiple_operator_error("x =-");
    expect_multiple_operator_error("=- y");
    expect_multiple_operator_error("=-");
}

#[test]
fn function_expression_in_statement_context() {
    test_module!("main =\n    +x\n    x",
        @"This expression would define an unused function; if you would like to create an operator block, each indented line must begin with an operator followed by a space: (BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((Invalid) (ExpressionStatement () (Call (Ident x))))))))");
    test_module!("main =\n    \\x -> x\n    x",
        @"This expression would define an unused function: (BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((Invalid) (ExpressionStatement () (Call (Ident x))))))))");
    test_module!("main =\n    _ x\n    x",
        @"This expression would define an unused function: (BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((Invalid) (ExpressionStatement () (Call (Ident x))))))))");
    // Catch a common error; See: https://github.com/enso-org/enso/issues/11203
    test_module!("main =\n    x +\n        1 +\n        2",
        @r#"This expression would define an unused function; if you would like to create an operator block, each indented line must begin with an operator followed by a space: (BodyBlock #((Function () #() () () (Ident main) #() () (BodyBlock #((ExpressionStatement () (OprApp (Call (Ident x)) (Ok "+") (BodyBlock #((Invalid) (ExpressionStatement () (Number () "2" ())))))))))))"#);
}

#[test]
fn property_access_simple() {
    test_block!("foo.bar", @"(BodyBlock #((ExpressionStatement () (Call (PropertyAccess (Call (Ident foo)) bar)))))");
    test_block!("foo . bar", @"(BodyBlock #((ExpressionStatement () (Call (PropertyAccess (Call (Ident foo)) bar)))))");
}

#[test]
fn property_access_lambdas() {
    test_block!(".log",
        @"(BodyBlock #((ExpressionStatement () (Call (PropertyAccess () log)))))");
    test_block!(". log",
        @"(BodyBlock #((ExpressionStatement () (Call (PropertyAccess () log)))))");
    test_block!("_.foo",
        @"(BodyBlock #((ExpressionStatement () (TemplateFunction (Call (PropertyAccess (Wildcard) foo))))))");
    test_block!("_ . foo",
        @"(BodyBlock #((ExpressionStatement () (TemplateFunction (Call (PropertyAccess (Wildcard) foo))))))");
}

#[test]
fn property_access_invalid() {
    test_block!("op._",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (TemplateFunction (Invalid)))))");
    test_block!("op ._",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Call (App (Ident op) (TemplateFunction (Invalid)))))))");
    test_block!("op._.something",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (TemplateFunction (Call (PropertyAccess (Invalid) something))))))");
    test_block!("x. length",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Call (App (Invalid) (Call (Ident length)))))))");
    test_block!("x.('p')",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!("Console.",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_block!(".",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
}

#[test]
#[ignore]
fn proposed_invalid_cases() {
    // FIXME: Type operators must be fully-applied
    test_module!("f : Text -> | Nothing -> Nothing\nf x = Nothing",
        @r#"(BodyBlock #((Function () #() ((Ident f) ":" (OprApp (Ident Text) (Err (#("->" "|"))) (OprApp (Ident Nothing) (Ok "->") (Ident Nothing)))) () (Ident f) #((() (Ident x) () ())) () (Ident Nothing))))"#);
    // FIXME: Annotation argument should be syntactically required
    test_module!("@anno\nfn = 10",
        @r#"(BodyBlock #((Function () #(((anno ()) #(()))) () () (Ident fn) #() () (Number () "10" ()))))"#);

    // I think it may be better to treat this as a semantic error than a syntax error. The
    // inter-line state we'd have to maintain to recognize this error would interfere with
    // incremental parsing (which in the GUI we've discussed maybe wanting some day).
    test_module!("private\nprivate", @"(BodyBlock #((Private private) (Private private)))");
    test_module!("type T\nprivate", @"(BodyBlock #((TypeDef T #() #()) (Private private)))");
    // Comments and empty lines are allowed before "private".
    test_module!("# Some comment\n    # Other comment\n\n    \nprivate",
        @"(BodyBlock #((BodyBlock #(() ())) () () () (Private private)))");
}

#[test]
fn nonsense_inputs() {
    test_module!("`a (b = 1).`",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Invalid))))");
    test_module!("type M = B<d f<'a> F(M<'a>) -> S>;",
        @"Expected identifier or wildcard in argument binding: (BodyBlock #((TypeDef M #((() (Invalid) () ((Invalid))) (() (Invalid) () ()) (() (Invalid) () ()) (() (Invalid) () ()) (() (Invalid) () ())) #())))");
    test_module!("'`'\nx `y`\nz",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (Invalid)) (ExpressionStatement () (Call (App (Ident x) (Invalid)))) (ExpressionStatement () (Call (Ident z)))))");
    test_module!("if (asGuestValue\n  a",
        @"Invalid macro invocation: (BodyBlock #((ExpressionStatement () (ArgumentBlockApplication (App (Invalid) (Invalid)) #((Call (Ident a)))))))");
    test_module!("foo(\n  a",
        @"Space required between terms: (BodyBlock #((ExpressionStatement () (ArgumentBlockApplication (Invalid) #((Call (Ident a)))))))");
    test_module!("(Vector(), true)",
        @"Invalid use of syntactic operator in expression: (BodyBlock #((ExpressionStatement () (Group (Invalid)))))");
    test_module!("x @Builtin_Method \"a\"",
        @"Unexpected expression annotation: (BodyBlock #((ExpressionStatement () (Invalid))))");
}

// FIXME: These cases are currently unparseable
#[test]
#[ignore]
fn nonsense_inputs_broken() {
    test_module!("'`\n", @"");
    test_module!(".'\\\n", @"");
}
