//! Parse expressions and compare their results to expected values.

// === Features ===
#![feature(cell_update)]
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

use enso_parser_debug::to_s_expr;



// ===========================
// === Test support macros ===
// ===========================

/// Parses input as a sequence of S-expressions, and wraps it in a `BodyBlock`.
macro_rules! block {
    ( $($statements:tt)* ) => {
        lexpr::sexp![(BodyBlock #( $( $statements )* ) )]
    }
}

macro_rules! test {
    ( $code:expr, $($statements:tt)* ) => {
        test($code, block![$( $statements )*]);
    }
}



// ================================
// === Language Construct Tests ===
// ================================

#[test]
fn nothing() {
    test("", block![()]);
}

#[test]
fn application() {
    test("a b c", block![(App (App (Ident a) (Ident b)) (Ident c))]);
}

#[test]
fn parentheses() {
    test("(a b)", block![(Group (App (Ident a) (Ident b)))]);
    test("x)", block![(App (Ident x) (Invalid))]);
    test("(x", block![(App (Invalid) (Ident x))]);
    test("(a) (b)", block![(App (Group (Ident a)) (Group (Ident b)))]);
    #[rustfmt::skip]
    test("((a b) c)", block![
        (Group
         (App (Group (App (Ident a) (Ident b)))
              (Ident c)))]);
}

#[test]
fn section_simple() {
    let expected_lhs = block![(OprSectionBoundary 1 (OprApp () (Ok "+") (Ident a)))];
    test("+ a", expected_lhs);
    let expected_rhs = block![(OprSectionBoundary 1 (OprApp (Ident a) (Ok "+") ()))];
    test("a +", expected_rhs);
}

#[test]
fn inline_if() {
    #[rustfmt::skip]
    test("if True then True else False", block![
       (MultiSegmentApp #(((Ident if) (Ident True))
                          ((Ident then) (Ident True))
                          ((Ident else) (Ident False))))]);
}

#[test]
fn then_block() {
    #[rustfmt::skip]
    test("if True then\n True", block![
       (MultiSegmentApp #(((Ident if) (Ident True)) ((Ident then) (BodyBlock #((Ident True))))))]);
}

#[test]
fn else_block() {
    #[rustfmt::skip]
    test("if True then True else\n False", block![
       (MultiSegmentApp #(((Ident if) (Ident True))
                          ((Ident then) (Ident True))
                          ((Ident else) (BodyBlock #((Ident False))))))]);
}


// === Comments ===

#[test]
fn plain_comments() {
    test!("# a b c", ()());
    test!("main = # define main\n 4",
        (Function (Ident main) #() "=" (BodyBlock #(() (Number () "4" ())))));
}

#[test]
fn doc_comments() {
    #[rustfmt::skip]
    let lines = vec![
        "## The Identity Function",
        "",
        "   Arguments:",
        "   - x: value to do nothing to",
        "id x = x",
    ];
    #[rustfmt::skip]
    test(&lines.join("\n"), block![
        (Documented
         (#((Section " The Identity Function") (Newline)
           (Newline)
           (Section "Arguments:") (Newline)
           (Section "- x: value to do nothing to"))
         #(()))
         (Function (Ident id) #((() (Ident x) () ())) "=" (Ident x)))]);
    #[rustfmt::skip]
    let lines = vec![
        "type Foo",
        " ## Test indent handling",
        "  ",
        " foo",
    ];
    #[rustfmt::skip]
    test!(&lines.join("\n"),
        (TypeDef type Foo #() #(
         (Documented
          (#((Section " Test indent handling")) #(() ()))
          (Ident foo)))));
}


// === Type Definitions ===

#[test]
fn type_definition_no_body() {
    test!("type Bool", (TypeDef type Bool #() #()));
    test!("type Option a", (TypeDef type Option #((() (Ident a) () ())) #()));
    test!("type Option (a)", (TypeDef type Option #((() (Ident a) () ())) #()));
    test!("type Foo (a : Int)", (TypeDef type Foo #((() (Ident a) (":" (Ident Int)) ())) #()));
    test!("type A a=0", (TypeDef type A #((() (Ident a) () ("=" (Number () "0" ())))) #()));
    test!("type Existing_Headers (column_names : Vector Text)",
        (TypeDef type Existing_Headers #(
         (() (Ident column_names) (":" (App (Ident Vector) (Ident Text))) ())) #()));
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
        (TypeDef type Geo #()
         #((ConstructorDefinition
             Circle #() #(((() (Ident radius) () ())) ((() (Ident x) () ()))))
           (ConstructorDefinition
             Rectangle #((() (Ident width) () ()) (() (Ident height) () ())) #())
           (ConstructorDefinition Point #() #())))];
    test(&code.join("\n"), expected);
    let code = "type Foo\n Bar (a : B = C.D)";
    #[rustfmt::skip]
    let expected = block![
        (TypeDef type Foo #() #((ConstructorDefinition
         Bar
         #((() (Ident a) (":" (Ident B)) ("=" (OprApp (Ident C) (Ok ".") (Ident D)))))
         #())))];
    test(code, expected);
    let code = "type Foo\n ## Bar\n Baz";
    let expected = block![(TypeDef type Foo #() #(
        (Documented (#((Section " Bar")) #(())) (ConstructorDefinition Baz #() #()))))];
    test(code, expected);
    let code = ["type A", "    Foo (a : Integer, b : Integer)"];
    #[rustfmt::skip]
    let expected = block![(TypeDef type A #() #(
        (ConstructorDefinition Foo #((() (Invalid) () ())) #())))];
    test(&code.join("\n"), expected);
}

#[test]
fn type_methods() {
    let code = ["type Geo", "    number =", "        x", "    area self = x + x"];
    #[rustfmt::skip]
    let expected = block![
        (TypeDef type Geo #()
         #((Function (Ident number) #() "=" (BodyBlock #((Ident x))))
           (Function (Ident area) #((() (Ident self) () ()))
                     "=" (OprApp (Ident x) (Ok "+") (Ident x)))))];
    test(&code.join("\n"), expected);
    let code = [
        "type Problem_Builder",
        "    ## Returns a vector containing all reported problems, aggregated.",
        "    build_problemset : Vector",
        "    build_problemset self =",
        "        self",
    ];
    #[rustfmt::skip]
    let expected = block![
        (TypeDef type Problem_Builder #() #(
         (Documented
          (#((Section " Returns a vector containing all reported problems, aggregated.")) #(()))
          (TypeSignature (Ident build_problemset) ":" (Ident Vector)))
         (Function (Ident build_problemset) #((() (Ident self) () ()))
                   "=" (BodyBlock #((Ident self))))))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn type_operator_methods() {
    #[rustfmt::skip]
    let code = [
        "type Foo",
        "    + : Foo -> Foo -> Foo",
        "    + self b = b",
        "    Foo.+ : Foo",
        "    Foo.+ self b = b",
    ];
    #[rustfmt::skip]
    let expected = block![
        (TypeDef type Foo #()
         #((TypeSignature (Ident #"+") ":"
            (OprApp (Ident Foo) (Ok "->") (OprApp (Ident Foo) (Ok "->") (Ident Foo))))
           (Function (Ident #"+") #((() (Ident self) () ()) (() (Ident b) () ())) "=" (Ident b))
           (TypeSignature (OprApp (Ident Foo) (Ok ".") (Ident #"+")) ":" (Ident Foo))
           (Function (OprApp (Ident Foo) (Ok ".") (Ident #"+"))
                     #((() (Ident self) () ()) (() (Ident b) () ())) "=" (Ident b))))];
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
        (TypeDef type Geo #()
         #((ConstructorDefinition Circle #() #(
             ((() (Ident radius) (":" (Ident float)) ()))
             ((() (Ident x) () ()))))
           (ConstructorDefinition
             Rectangle #((() (Ident width) () ()) (() (Ident height) () ())) #())
           (ConstructorDefinition Point #() #())
           ()
           (Function (Ident number) #() "=" (BodyBlock #((Ident x))))
           (Function (Ident area) #((() (Ident self) () ()))
                      "=" (OprApp (Ident x) (Ok "+") (Ident x)))))];
    test(&code.join("\n"), expected);
}

#[test]
fn type_def_defaults() {
    let code = ["type Result error ok=Nothing", "    Ok value:ok = Nothing"];
    #[rustfmt::skip]
    let expected = block![
        (TypeDef type Result #((() (Ident error) () ())
                               (() (Ident ok) () ("=" (Ident Nothing))))
         #((ConstructorDefinition Ok
            #((() (Ident value) (":" (Ident ok)) ("=" (Ident Nothing)))) #())))];
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
        (TypeDef type Foo #()
         #((TypeDef type Bar #() #())
           (TypeDef type Baz #() #())))
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
    test("foo a = x", block![(Function (Ident foo) #((() (Ident a) () ())) "=" (Ident x))]);
    #[rustfmt::skip]
    test("foo a b = x",
         block![(Function (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ())) "=" (Ident x))]);
    #[rustfmt::skip]
    test(
        "foo a b c = x", block![
            (Function (Ident foo)
             #((() (Ident a) () ()) (() (Ident b) () ()) (() (Ident c) () ()))
             "=" (Ident x))],
    );
    test("foo _ = x", block![(Function (Ident foo) #((() (Wildcard -1) () ())) "=" (Ident x))]);
}

#[test]
fn function_block_noargs() {
    test("foo =", block![(Function (Ident foo) #() "=" ())]);
}

#[test]
fn function_block_simple_args() {
    test("foo a =", block![(Function (Ident foo) #((() (Ident a) () ())) "=" ())]);
    #[rustfmt::skip]
    test("foo a b =", block![(Function (Ident foo) #((() (Ident a) () ())
                                                     (() (Ident b) () ())) "=" ())]);
    #[rustfmt::skip]
    test("foo a b c =", block![
        (Function (Ident foo) #((() (Ident a) () ()) (() (Ident b) () ()) (() (Ident c) () ())) "="
         ())]);
}

#[test]
fn function_qualified() {
    test("Id.id x = x", block![
        (Function (OprApp (Ident Id) (Ok ".") (Ident id)) #((() (Ident x) () ())) "=" (Ident x))]);
}

#[test]
fn ignored_arguments() {
    test!("f ~_ = x", (Function (Ident f) #(("~" (Wildcard -1) () ())) "=" (Ident x)));
}

#[test]
fn foreign_functions() {
    test!("foreign python my_method a b = \"42\"",
        (ForeignFunction foreign python my_method
            #((() (Ident a) () ()) (() (Ident b) () ()))
            "="
            (TextLiteral #((Section "42")))));
}


// === Named arguments ===

#[test]
fn named_arguments() {
    let cases = [
        ("f x=y", block![(NamedApp (Ident f) x "=" (Ident y))]),
        ("f (x = y)", block![(NamedApp (Ident f) x "=" (Ident y))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Default arguments ===

#[test]
fn default_app() {
    test!("f default", (DefaultApp (Ident f) default));
}

#[test]
fn argument_named_default() {
    test!("f default x = x",
        (Function (Ident f) #((() (Ident default) () ()) (() (Ident x) () ())) "=" (Ident x)));
    test!("f x default = x",
        (Function (Ident f) #((() (Ident x) () ()) (() (Ident default) () ())) "=" (Ident x)));
}

#[test]
fn default_arguments() {
    #[rustfmt::skip]
    let cases = [
        ("f x=1 = x", block![
            (Function (Ident f) #((() (Ident x) () ("=" (Number () "1" ())))) "=" (Ident x))]),
        ("f (x = 1) = x", block![
            (Function (Ident f) #((() (Ident x) () ("=" (Number () "1" ())))) "=" (Ident x))]),
        // Pattern in LHS:
        ("f ~x=1 = x", block![
            (Function (Ident f)
             #(("~" (Ident x) () ("=" (Number () "1" ()))))
             "=" (Ident x))]),
        ("f (~x = 1) = x", block![
            (Function (Ident f)
             #(("~" (Ident x) () ("=" (Number () "1" ()))))
             "=" (Ident x))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Code Blocks ===

#[test]
fn code_block_body() {
    let code = ["main =", "    x"];
    test(&code.join("\n"), block![(Function (Ident main) #() "=" (BodyBlock #((Ident x))))]);
    let code = ["main =", "      ", "    x"];
    test(&code.join("\n"), block![(Function (Ident main) #() "=" (BodyBlock #(() (Ident x))))]);
    let code = ["main =", "    ", "    x"];
    test(&code.join("\n"), block![(Function (Ident main) #() "=" (BodyBlock #(() (Ident x))))]);
    let code = ["main =", "  ", "    x"];
    test(&code.join("\n"), block![(Function (Ident main) #() "=" (BodyBlock #(() (Ident x))))]);
    let code = ["main =", "", "    x"];
    test(&code.join("\n"), block![(Function (Ident main) #() "=" (BodyBlock #(() (Ident x))))]);

    #[rustfmt::skip]
    let code = [
        "main =",
        "    +x",
        "    print x",
    ];
    #[rustfmt::skip]
    let expect = block![
        (Function (Ident main) #() "=" (BodyBlock #(
         (OprSectionBoundary 1 (OprApp () (Ok "+") (Ident x)))
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
fn dot_operator_blocks() {
    let code = ["rect1", "    . width = 7", "    . center", "        + x"];
    #[rustfmt::skip]
    let expected = block![
        (OperatorBlockApplication (Ident rect1)
         #(((Ok ".") (OprApp (Ident width) (Ok "=") (Number () "7" ())))
           ((Ok ".") (OperatorBlockApplication (Ident center)
                     #(((Ok "+") (Ident x))) #()))) #())];
    test(&code.join("\n"), expected);
}

#[test]
fn code_block_argument_list() {
    #[rustfmt::skip]
    let code = [
        "foo",
        "    bar",
    ];
    test!(&code.join("\n"), (ArgumentBlockApplication (Ident foo) #((Ident bar))));

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
          (OprSectionBoundary 1 (OprApp () (Ok "+") (Ident x)))
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
    test(&code.join("\n"), block![(Function (Ident foo) #() "=" ()) (Ident bar)]);
    // This parses similarly to above; a line with no non-whitespace content does not create a code
    // block.
    let code = ["foo =", "    ", "bar"];
    test(&code.join("\n"), block![(Function (Ident foo) #() "=" ()) () (Ident bar)]);
}

#[test]
fn code_block_bad_indents1() {
    let code = ["main =", "  foo", " bar", "  baz"];
    let expected = block![
        (Function (Ident main) #() "=" (BodyBlock #((Ident foo) (Ident bar) (Ident baz))))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn code_block_bad_indents2() {
    let code = ["main =", "  foo", " bar", "baz"];
    let expected = block![
        (Function (Ident main) #() "=" (BodyBlock #((Ident foo) (Ident bar))))
        (Ident baz)
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn code_block_with_following_statement() {
    let code = ["main =", "    foo", "bar"];
    let expected = block![
        (Function (Ident main) #() "=" (BodyBlock #((Ident foo))))
        (Ident bar)
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn operator_block_nested() {
    let code = ["foo", "    + bar", "        - baz"];
    #[rustfmt::skip]
    let expected = block![
        (OperatorBlockApplication (Ident foo)
         #(((Ok "+") (OperatorBlockApplication (Ident bar) #(((Ok "-") (Ident baz))) #())))
         #())];
    test(&code.join("\n"), expected);
}

#[test]
fn operator_section_in_operator_block() {
    let code = ["foo", "    + bar +"];
    #[rustfmt::skip]
    let expected = block![
        (OperatorBlockApplication (Ident foo)
         #(((Ok "+") (OprSectionBoundary 1 (OprApp (Ident bar) (Ok "+") ()))))
         #())];
    test(&code.join("\n"), expected);
}

#[test]
fn first_line_indented() {
    expect_invalid_node(" a");
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
    #[rustfmt::skip]
    let cases = [
        ("x * y + z", block![(OprApp (OprApp (Ident x) (Ok "*") (Ident y)) (Ok "+") (Ident z))]),
        ("x + y * z", block![(OprApp (Ident x) (Ok "+") (OprApp (Ident y) (Ok "*") (Ident z)))]),
        ("w + x + y * z", block![
            (OprApp (OprApp (Ident w) (Ok "+") (Ident x)) (Ok "+")
                    (OprApp (Ident y) (Ok "*") (Ident z)))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
    test!("x - 1 + 2",
        (OprApp (OprApp (Ident x) (Ok "-") (Number () "1" ())) (Ok "+") (Number () "2" ())));
}

#[test]
fn dot_operator_precedence() {
    test!("x y . f v", (App (OprApp (App (Ident x) (Ident y)) (Ok ".") (Ident f)) (Ident v)));
}

#[test]
fn right_associative_operators() {
    test!("x --> y ---> z", (OprApp (Ident x) (Ok "-->") (OprApp (Ident y) (Ok "--->") (Ident z))));
    test!("x <| y <<| z", (OprApp (Ident x) (Ok "<|") (OprApp (Ident y) (Ok "<<|") (Ident z))));
}

#[test]
fn left_associative_operators() {
    test!("x + y + z", (OprApp (OprApp (Ident x) (Ok "+") (Ident y)) (Ok "+") (Ident z)));
}

#[test]
fn pipeline_operators() {
    test("f <| a", block![(OprApp (Ident f) (Ok "<|") (Ident a))]);
    test("a |> f", block![(OprApp (Ident a) (Ok "|>") (Ident f))]);
}

#[test]
fn accessor_operator() {
    // Test that the accessor operator `.` is treated like any other operator.
    let cases = [
        ("Console.", block![(OprSectionBoundary 1 (OprApp (Ident Console) (Ok ".") ()))]),
        (".", block![(OprSectionBoundary 2 (OprApp () (Ok ".") ()))]),
        (".log", block![(OprSectionBoundary 1 (OprApp () (Ok ".") (Ident log)))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn operator_sections() {
    #[rustfmt::skip]
    test(".map (+2 * 3) *7", block![
        (OprSectionBoundary 1
         (App (App (OprApp () (Ok ".") (Ident map))
                   (Group
                    (OprSectionBoundary 1 (OprApp (OprApp () (Ok "+") (Number () "2" ()))
                    (Ok "*") (Number () "3" ())))))
              (OprSectionBoundary 1 (OprApp () (Ok "*") (Number () "7" ())))))]);
    #[rustfmt::skip]
    test(".sum 1", block![
        (OprSectionBoundary 1 (App (OprApp () (Ok ".") (Ident sum)) (Number () "1" ())))]);
    #[rustfmt::skip]
    test("+1 + x", block![
        (OprSectionBoundary 1 (OprApp (OprApp () (Ok "+") (Number () "1" ()))
                                (Ok "+") (Ident x)))]);
    #[rustfmt::skip]
    test("increment = 1 +", block![
        (Assignment (Ident increment) "="
         (OprSectionBoundary 1 (OprApp (Number () "1" ()) (Ok "+") ())))]);
}

#[test]
fn template_functions() {
    #[rustfmt::skip]
    test("_.map (_+2 * 3) _*7", block![
        (TemplateFunction 1
         (App (App (OprApp (Wildcard 0) (Ok ".") (Ident map))
                   (Group
                    (TemplateFunction 1
                     (OprApp (OprApp (Wildcard 0) (Ok "+") (Number () "2" ()))
                    (Ok "*") (Number () "3" ())))))
              (TemplateFunction 1 (OprApp (Wildcard 0) (Ok "*") (Number () "7" ())))))]);
    #[rustfmt::skip]
    test("_.sum 1", block![
        (TemplateFunction 1 (App (OprApp (Wildcard 0) (Ok ".") (Ident sum)) (Number () "1" ())))]);
    #[rustfmt::skip]
    test("_+1 + x", block![
        (TemplateFunction 1 (OprApp (OprApp (Wildcard 0) (Ok "+") (Number () "1" ()))
                           (Ok "+") (Ident x)))]);
}


// === Unary Operators ===

#[test]
fn unevaluated_argument() {
    let code = ["main ~foo = x"];
    let expected = block![
        (Function (Ident main) #(("~" (Ident foo) () ())) "=" (Ident x))
    ];
    test(&code.join("\n"), expected);
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
    test!("x = y+-z",
        (Assignment (Ident x) "=" (OprApp (Ident y) (Ok "+") (UnaryOprApp "-" (Ident z)))));
    // Create an operator section that adds a negated value to its input.
    test!("x = +-z",
        (Assignment (Ident x) "=" (OprSectionBoundary 1
            (OprApp () (Ok "+") (UnaryOprApp "-" (Ident z))))));
    // Create an operator section that adds its input, negated, to a value.
    test!("x = y+-",
        (Assignment (Ident x) "=" (OprSectionBoundary 1
            (OprApp (Ident y) (Ok "+") (UnaryOprApp "-" ())))));
    // Assign a negative number to x.
    test!("x=-1", (Assignment (Ident x) "=" (UnaryOprApp "-" (Number () "1" ()))));
    // Assign a negated value to x.
    test!("x=-y", (Assignment (Ident x) "=" (UnaryOprApp "-" (Ident y))));
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
        ("- x", block![(OprSectionBoundary 1 (OprApp () (Ok "-") (Ident x)))]),
        ("(- x)", block![(Group (OprSectionBoundary 1 (OprApp () (Ok "-") (Ident x))))]),
        ("- (x * x)", block![
            (OprSectionBoundary 1 (OprApp () (Ok "-")
             (Group (OprApp (Ident x) (Ok "*") (Ident x)))))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn minus_unary() {
    test!("f -x", (App (Ident f) (UnaryOprApp "-" (Ident x))));
    test!("-x", (UnaryOprApp "-" (Ident x)));
    test!("(-x)", (Group (UnaryOprApp "-" (Ident x))));
    test!("-(x * x)", (UnaryOprApp "-" (Group (OprApp (Ident x) (Ok "*") (Ident x)))));
    test!("x=-x", (Assignment (Ident x) "=" (UnaryOprApp "-" (Ident x))));
    test!("-x+x", (OprApp (UnaryOprApp "-" (Ident x)) (Ok "+") (Ident x)));
    test!("-x*x", (OprApp (UnaryOprApp "-" (Ident x)) (Ok "*") (Ident x)));
}

#[test]
fn minus_unary_decimal() {
    test!("-2.1", (UnaryOprApp "-" (Number () "2" ("." "1"))));
}

#[test]
fn minus_unary_in_method_app() {
    test!("-1.x", (OprApp (UnaryOprApp "-" (Number () "1" ())) (Ok ".") (Ident x)));
    test!("-1.up_to 100",
        (App (OprApp (UnaryOprApp "-" (Number () "1" ())) (Ok ".") (Ident up_to))
             (Number () "100" ())));
}

#[test]
fn method_app_in_minus_unary() {
    test!("-Number.positive_infinity",
        (UnaryOprApp "-" (OprApp (Ident Number) (Ok ".") (Ident positive_infinity))));
}


// === Import/Export ===

#[test]
fn import() {
    #[rustfmt::skip]
    let cases = [
        ("import project.IO", block![
            (Import () () ((Ident import) (OprApp (Ident project) (Ok ".") (Ident IO))) () () ())]),
        ("import Standard.Base as Enso_List", block![
            (Import () ()
             ((Ident import) (OprApp (Ident Standard) (Ok ".") (Ident Base)))
             ()
             ((Ident as) (Ident Enso_List))
             ())]),
        ("from Standard.Base import all", block![
            (Import ()
             ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Base)))
             ((Ident import) ())
             all () ())]),
        ("from Standard.Base import all hiding Number, Boolean", block![
            (Import ()
             ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Base)))
             ((Ident import) ())
             all
             ()
             ((Ident hiding) (OprApp (Ident Number) (Ok ",") (Ident Boolean))))]),
        ("polyglot java import java.lang.Float", block![
            (Import
             ((Ident polyglot) (Ident java))
             ()
             ((Ident import)
              (OprApp (OprApp (Ident java) (Ok ".") (Ident lang)) (Ok ".") (Ident Float)))
             () () ())]),
        ("polyglot java import java.net.URI as Java_URI", block![
            (Import
             ((Ident polyglot) (Ident java))
             ()
             ((Ident import)
              (OprApp (OprApp (Ident java) (Ok ".") (Ident net)) (Ok ".") (Ident URI)))
             ()
             ((Ident as) (Ident Java_URI))
             ())]),
        ("from Standard.Base import Foo, Bar, Baz", block![
            (Import ()
             ((Ident from) (OprApp (Ident Standard) (Ok ".") (Ident Base)))
             ((Ident import) (OprApp (OprApp (Ident Foo) (Ok ",") (Ident Bar)) (Ok ",") (Ident Baz)))
             () () ())]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
    expect_invalid_node("from Standard.Base.Data.Array import new as array_new");
}

#[test]
fn export() {
    #[rustfmt::skip]
    let cases = [
        ("export prj.Data.Foo", block![
            (Export ()
             ((Ident export)
              (OprApp (OprApp (Ident prj) (Ok ".") (Ident Data)) (Ok ".") (Ident Foo)))
             () () ())]),
        ("export Foo as Bar", block![
            (Export () ((Ident export) (Ident Foo)) () ((Ident as) (Ident Bar)) ())]),
        ("from Foo export Bar, Baz", block![
            (Export
             ((Ident from) (Ident Foo))
             ((Ident export) (OprApp (Ident Bar) (Ok ",") (Ident Baz)))
             () () ())]),
        ("from Foo export all hiding Bar, Baz", block![
            (Export
             ((Ident from) (Ident Foo))
             ((Ident export) ())
             all
             ()
             ((Ident hiding) (OprApp (Ident Bar) (Ok ",") (Ident Baz))))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
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
    let _ast = parse(code);
    let _meta: enso_parser::metadata::Metadata = meta.unwrap();
}


// === Type annotations and signatures ===

#[test]
fn type_signatures() {
    #[rustfmt::skip]
    let cases = [
        ("val : Bool", block![(TypeSignature (Ident val) ":" (Ident Bool))]),
        ("val : List Int", block![(TypeSignature (Ident val) ":" (App (Ident List) (Ident Int)))]),
        ("foo : [Integer | Text] -> (Integer | Text)", block![
            (TypeSignature (Ident foo) ":"
             (OprApp (Array (OprApp (Ident Integer) (Ok "|") (Ident Text)) #())
                     (Ok "->")
                     (Group (OprApp (Ident Integer) (Ok "|") (Ident Text)))))]),
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
              (Group (TypeAnnotated (Ident x) ":" (Ident Int)))))]),
        ("(x : My_Type _)", block![
            (Group (TypeAnnotated (Ident x) ":" (App (Ident My_Type) (Wildcard -1))))]),
        ("x : List Int -> Int", block![
            (TypeSignature (Ident x) ":"
             (OprApp (App (Ident List) (Ident Int)) (Ok "->") (Ident Int)))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Text Literals ===

#[test]
fn inline_text_literals() {
    test!(r#""I'm an inline raw text!""#, (TextLiteral #((Section "I'm an inline raw text!"))));
    test!(r#"zero_length = """#, (Assignment (Ident zero_length) "=" (TextLiteral #())));
    test!(r#""type""#, (TextLiteral #((Section "type"))));
    test!(r#"unclosed = ""#, (Assignment (Ident unclosed) "=" (TextLiteral #())));
    test!(r#"unclosed = "a"#, (Assignment (Ident unclosed) "=" (TextLiteral #((Section "a")))));
    test!(r#"'Other quote type'"#, (TextLiteral #((Section "Other quote type"))));
    test!(r#""Non-escape: \n""#, (TextLiteral #((Section "Non-escape: \\n"))));
    test!(r#""Non-escape: \""#, (TextLiteral #((Section "Non-escape: \\"))));
    test!(r#"'String with \' escape'"#,
        (TextLiteral #((Section "String with ") (Escape '\'') (Section " escape"))));
    test!(r#"'\u0915\u094D\u0937\u093F'"#, (TextLiteral
        #((Escape '\u{0915}') (Escape '\u{094D}') (Escape '\u{0937}') (Escape '\u{093F}'))));
    test!(r#"('\n')"#, (Group (TextLiteral #((Escape '\n')))));
    test!(r#"`"#, (Invalid));
    test!(r#"(")")"#, (Group (TextLiteral #((Section ")")))));
    test!(r#"'\x'"#, (TextLiteral #((Escape ()))));
    test!(r#"'\u'"#, (TextLiteral #((Escape ()))));
    test!(r#"'\U'"#, (TextLiteral #((Escape ()))));
}

#[test]
fn multiline_text_literals() {
    test("'''", block![(TextLiteral #())]);
    let code = r#""""
    part of the string
       3-spaces indented line, part of the Text Block
    this does not end the string -> '''

    `also` part of the string

x"#;
    #[rustfmt::skip]
    let expected = block![
        (TextLiteral
         #((Section "part of the string") (Newline)
           (Section "   3-spaces indented line, part of the Text Block") (Newline)
           (Section "this does not end the string -> '''") (Newline)
           (Newline)
           (Section "`also` part of the string")))
        ()
        (Ident x)
    ];
    test(code, expected);
    let code = r#""""
    multiline string that doesn't end in a newline
x"#;
    #[rustfmt::skip]
    let expected = block![
        (TextLiteral #((Section "multiline string that doesn't end in a newline")))
        (Ident x)
    ];
    test(code, expected);
    let code = "x = \"\"\"\n    Indented multiline\nx";
    #[rustfmt::skip]
    let expected = block![
        (Assignment (Ident x) "=" (TextLiteral #((Section "Indented multiline"))))
        (Ident x)
    ];
    test(code, expected);
    let code = "'''\n    \\nEscape at start\n";
    test!(code, (TextLiteral #((Escape '\n') (Section "Escape at start"))) ());
    let code = "x =\n x = '''\n  x\nx";
    #[rustfmt::skip]
    let expected = block![
        (Function (Ident x) #() "="
         (BodyBlock #((Assignment (Ident x) "=" (TextLiteral #((Section "x")))))))
        (Ident x)
    ];
    test(code, expected);
    test!("foo = bar '''\n baz",
        (Assignment (Ident foo) "=" (App (Ident bar) (TextLiteral #((Section "baz"))))));
    test!("'''\n \\t'", (TextLiteral #((Escape '\t') (Section "'"))));
    test!("'''\n x\n \\t'",
        (TextLiteral #((Section "x") (Newline) (Escape '\t') (Section "'"))));
}

#[test]
fn interpolated_literals_in_inline_text() {
    test!(r#"'Simple case.'"#, (TextLiteral #((Section "Simple case."))));
    test!(r#"'With a `splice`.'"#, (TextLiteral
        #((Section "With a ")
          (Splice (Ident splice))
          (Section "."))));
    test!(r#"'` SpliceWithLeadingWhitespace`'"#,
        (TextLiteral #((Splice (Ident SpliceWithLeadingWhitespace)))));
    test!(r#"'String with \n escape'"#,
        (TextLiteral #((Section "String with ") (Escape '\n') (Section " escape"))));
    test!(r#"'\x0Aescape'"#, (TextLiteral #((Escape '\n') (Section "escape"))));
    test!(r#"'\u000Aescape'"#, (TextLiteral #((Escape '\n') (Section "escape"))));
    test!(r#"'\u{0000A}escape'"#, (TextLiteral #((Escape '\n') (Section "escape"))));
    test!(r#"'\U0000000Aescape'"#, (TextLiteral #((Escape '\n') (Section "escape"))));
}

#[test]
fn interpolated_literals_in_multiline_text() {
    let code = r#"'''
    `splice` at start"#;
    #[rustfmt::skip]
    let expected = block![
        (TextLiteral #((Splice (Ident splice)) (Section " at start")))];
    test(code, expected);

    let code = r#"'''
    text with a `splice`
    and some \u000Aescapes\'"#;
    #[rustfmt::skip]
    let expected = block![
        (TextLiteral
         #((Section "text with a ") (Splice (Ident splice)) (Newline)
           (Section "and some ") (Escape '\n') (Section "escapes") (Escape '\'')))];
    test(code, expected);
}


// === Lambdas ===

#[test]
fn new_lambdas() {
    let cases = [
        (r#"\v -> v"#, block![(Lambda "\\" (OprApp (Ident v) (Ok "->") (Ident v)))]),
        (r#"\a b -> x"#, block![
            (Lambda "\\" (OprApp (App (Ident a) (Ident b)) (Ok "->") (Ident x)))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn old_lambdas() {
    test("x -> y", block![(OprApp (Ident x) (Ok "->") (Ident y))]);
    test("x->y", block![(OprApp (Ident x) (Ok "->") (Ident y))]);
    test("x-> y", block![(OprApp (Ident x) (Ok "->") (Ident y))]);
    test("x->\n y", block![(OprApp (Ident x) (Ok "->") (BodyBlock #((Ident y))))]);
    test("x ->\n y", block![(OprApp (Ident x) (Ok "->") (BodyBlock #((Ident y))))]);
    test("f x->\n y", block![
        (App (Ident f) (OprApp (Ident x) (Ok "->") (BodyBlock #((Ident y)))))]);
    test("x->y-> z", block![(OprApp (Ident x) (Ok "->") (OprApp (Ident y) (Ok "->") (Ident z)))]);
}


// === Pattern Matching ===

#[test]
fn pattern_irrefutable() {
    let code = "Point x_val = my_point";
    let expected = block![(Assignment (App (Ident Point) (Ident x_val)) "=" (Ident my_point))];
    test(code, expected);

    test("Vector _ = x", block![(Assignment (App (Ident Vector) (Wildcard -1)) "=" (Ident x))]);
}

#[test]
fn case_expression() {
    #[rustfmt::skip]
    let code = [
        "case a of",
        "    Some -> x",
        "    Int -> x",
    ];
    #[rustfmt::skip]
    let expected = block![
        (CaseOf (Ident a) #(
         ((() (Ident Some) "->" (Ident x)))
         ((() (Ident Int) "->" (Ident x)))))
    ];
    test(&code.join("\n"), expected);

    #[rustfmt::skip]
    let code = [
        "case a of",
        "    Vector_2d x y -> x",
    ];
    #[rustfmt::skip]
    let expected = block![
        (CaseOf (Ident a) #(
         ((() (App (App (Ident Vector_2d) (Ident x)) (Ident y)) "->" (Ident x)))))];
    test(&code.join("\n"), expected);

    #[rustfmt::skip]
    let code = [
        "case self of",
        "    Vector_2d -> x",
        "    _ -> x",
    ];
    #[rustfmt::skip]
    let expected = block![
        (CaseOf (Ident self) #(
         ((() (Ident Vector_2d) "->" (Ident x)))
         ((() (Wildcard -1) "->" (Ident x)))))];
    test(&code.join("\n"), expected);

    #[rustfmt::skip]
    let code = [
        "case foo of",
        "    v:My_Type -> x",
        "    v:(My_Type _ _) -> x",
    ];
    #[rustfmt::skip]
    let expected = block![
        (CaseOf (Ident foo) #(
         ((() (TypeAnnotated (Ident v) ":" (Ident My_Type)) "->" (Ident x)))
         ((() (TypeAnnotated (Ident v) ":"
            (Group (App (App (Ident My_Type) (Wildcard -1)) (Wildcard -1))))
           "->" (Ident x)))))];
    test(&code.join("\n"), expected);
}

#[test]
fn case_documentation() {
    #[rustfmt::skip]
    let code = [
        "case a of",
        "    ## The Some case",
        "    Some -> x",
        "    ## The Int case",
        "    Int -> x",
    ];
    #[rustfmt::skip]
    let expected = block![
        (CaseOf (Ident a) #(
            (((#((Section " The Some case")) #()) () () ()))
            ((() (Ident Some) "->" (Ident x)))
            (((#((Section " The Int case")) #()) () () ()))
            ((() (Ident Int) "->" (Ident x)))))
    ];
    test(&code.join("\n"), expected);
}

#[test]
fn case_by_type() {
    macro_rules! test_case {
        ( $code:expr, $case:tt ) => {
            test(&format!("case foo of\n {}", $code), block![(CaseOf (Ident foo) #(($case)))]);
        }
    }
    test_case!("f:A->B -> x",
        (() (TypeAnnotated (Ident f) ":" (OprApp (Ident A) (Ok "->") (Ident B))) "->" (Ident x)));
    test_case!("f : A->B -> x",
        (() (TypeAnnotated (Ident f) ":" (OprApp (Ident A) (Ok "->") (Ident B))) "->" (Ident x)));
    test_case!("v : A -> x->x",
        (() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (OprApp (Ident x) (Ok "->") (Ident x))));
    test_case!("v : A -> x -> x",
        (() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (OprApp (Ident x) (Ok "->") (Ident x))));
    test_case!("v:A->x->x",
        (() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (OprApp (Ident x) (Ok "->") (Ident x))));
    test_case!("v:A->x", (() (TypeAnnotated (Ident v) ":" (Ident A)) "->" (Ident x)));
    test_case!("v : A -> _ + x",
        (() (TypeAnnotated (Ident v) ":" (Ident A)) "->"
         (TemplateFunction 1 (OprApp (Wildcard 0) (Ok "+") (Ident x)))));
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
        (CaseOf (Ident self) #(((() (App (Ident Vector_2d) (AutoScope)) "->" (Ident x)))))];
    test(&code.join("\n"), expected);
}

// === Private (project-private) keyword ===
#[test]
fn private_keyword() {
    test("private", block![(Private())]);
    test("private func", block![(Private (Ident func))]);
}


#[test]
#[ignore]
fn private_is_first_statement() {
    // Comments and empty lines are allowed before `private`.
    #[rustfmt::skip]
    let lines = vec![
        "# Some comment",
        "# Other comment",
        "",
        "private"
    ];
    test(&lines.join("\n"), block![()()()(Private)]);

    #[rustfmt::skip]
    let lines = vec![
        "type T",
        "",
        "private"
    ];
    expect_invalid_node(&lines.join("\n"));
}

// === Array/tuple literals ===

#[test]
fn array_literals() {
    let cases = [
        ("[]", block![(Array () #())]),
        ("[x]", block![(Array (Ident x) #())]),
        ("[x, y]", block![(Array (Ident x) #(("," (Ident y))))]),
        ("[x, y, z]", block![(Array (Ident x) #(("," (Ident y)) ("," (Ident z))))]),
        ("[ x , y ]", block![(Array (Ident x) #(("," (Ident y))))]),
        ("[ x , y , z ]", block![(Array (Ident x) #(("," (Ident y)) ("," (Ident z))))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}

#[test]
fn tuple_literals() {
    let cases = [
        ("{}", block![(Tuple () #())]),
        ("{x}", block![(Tuple (Ident x) #())]),
        ("{x, y}", block![(Tuple (Ident x) #(("," (Ident y))))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Numeric literals ===

#[cfg(test)]
mod numbers {
    use super::*;

    #[test]
    fn with_decimal() {
        test!("1 . 0", (OprApp (Number () "1" ()) (Ok ".") (Number () "0" ())));
        test!("1 .0",
            (App (Number () "1" ()) (OprSectionBoundary 1 (OprApp () (Ok ".") (Number () "0" ())))));
        test!("1. 0",
            (OprSectionBoundary 1 (App (OprApp (Number () "1" ()) (Ok ".") ()) (Number () "0" ()))));
        test!("pi = 3.14", (Assignment (Ident pi) "=" (Number () "3" ("." "14"))));
        test!("0.0.x", (OprApp (Number () "0" ("." "0")) (Ok ".") (Ident x)));
    }

    #[test]
    fn with_base() {
        test!("0b10101010", (Number "0b" "10101010" ()));
        test!("0o122137", (Number "0o" "122137" ()));
        test!("0xAE2F14", (Number "0x" "AE2F14" ()));
    }

    #[test]
    fn base_only() {
        test!("0x", (Number "0x" () ()));
        test!("0b", (Number "0b" () ()));
        test!("0o", (Number "0o" () ()));
    }

    #[test]
    // This syntax cannot be used until we remove old-nondecimal number support, which is
    // needed for compatibility until the old parser is fully replaced.
    #[ignore]
    fn new_delimited() {
        test!("100_000", (Number () "100_000" ()));
        test!("10_000.99", (Number () "10_000" ("." "99")));
    }

    #[test]
    fn old_nondecimal() {
        test!("2_01101101", (Number "2_" "01101101" ()));
        test!("-2_01101101", (UnaryOprApp "-" (Number "2_" "01101101" ())));
        test!("16_17ffffffffffffffa", (Number "16_" "17ffffffffffffffa" ()));
    }
}


// === Whitespace ===

#[test]
fn trailing_whitespace() {
    let cases = [
        ("a ", block![(Ident a) ()]),
        ("a \n", block![(Ident a) ()]),
        ("a = \n x", block![(Function (Ident a) #() "=" (BodyBlock #((Ident x))))]),
    ];
    cases.into_iter().for_each(|(code, expected)| test(code, expected));
}


// === Annotations ===

#[test]
fn at_operator() {
    expect_invalid_node("foo@bar");
    expect_invalid_node("foo @ bar");
}

#[test]
fn attributes() {
    test!("@on_problems P.g\nTable.select_columns : Text -> Table",
        (Annotated "@" on_problems
         (OprApp (Ident P) (Ok ".") (Ident g))
         #(())
         (TypeSignature (OprApp (Ident Table) (Ok ".") (Ident select_columns))
                        ":"
                        (OprApp (Ident Text) (Ok "->") (Ident Table)))));
    test!("@a z\n@b\nx", (Annotated "@" a (Ident z) #(()) (Annotated "@" b () #(()) (Ident x))));
    test!("@a\n@b\nx", (Annotated "@" a () #(()) (Annotated "@" b () #(()) (Ident x))));
}

#[test]
fn attributes_in_types() {
    test!("type A\n @a z\n @b\n x",
        (TypeDef type A #() #(
         (Annotated "@" a (Ident z) #(()) (Annotated "@" b () #(()) (Ident x))))));
}

#[test]
fn inline_builtin_annotations() {
    test!("@Tail_Call go t", (AnnotatedBuiltin "@" Tail_Call #() (App (Ident go) (Ident t))));
    test!("@Tail_Call go\n a\n b",
        (AnnotatedBuiltin "@" Tail_Call #()
         (ArgumentBlockApplication (Ident go) #((Ident a) (Ident b)))));
}

#[test]
fn multiline_builtin_annotations() {
    test!("@Builtin_Type\ntype Date",
        (AnnotatedBuiltin "@" Builtin_Type #(()) (TypeDef type Date #() #())));
}


// === SKIP and FREEZE ===

#[test]
fn freeze() {
    test!("FREEZE x", (MultiSegmentApp #(((Ident FREEZE) (Ident x)))));
    test!("FREEZE x + y", (MultiSegmentApp
                           #(((Ident FREEZE) (OprApp (Ident x) (Ok "+") (Ident y))))));
    test!("FREEZE x.f", (MultiSegmentApp
                         #(((Ident FREEZE) (OprApp (Ident x) (Ok ".") (Ident f))))));
    test!("FREEZE x.f y", (MultiSegmentApp #(((Ident FREEZE)
                           (App (OprApp (Ident x) (Ok ".") (Ident f)) (Ident y))))));
}

#[test]
fn skip() {
    test!("SKIP x", (MultiSegmentApp #(((Ident SKIP) (Ident x)))));
    test!("SKIP x + y", (MultiSegmentApp #(((Ident SKIP) (OprApp (Ident x) (Ok "+") (Ident y))))));
    test!("SKIP x.f", (MultiSegmentApp #(((Ident SKIP) (OprApp (Ident x) (Ok ".") (Ident f))))));
    test!("SKIP x.f y", (MultiSegmentApp #(((Ident SKIP)
                         (App (OprApp (Ident x) (Ok ".") (Ident f)) (Ident y))))));
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
    expect_invalid_node("foreign 4");
    expect_invalid_node("foreign 4 * 4");
    expect_invalid_node("foreign foo = \"4\"");
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
    expect_multiple_operator_error("x = y +- z");
    expect_multiple_operator_error("x =- y");
    //
    // Treating the `-` as a unary operator applied to `z` would be confusing, as it would be in
    // contradiction to the associativity implied by the whitespace rules.
    //
    // However, it would also be confusing to lex a sequence of characters like `+-` as a single
    // operator in spaced expressions, but as two operators in unspaced expressions.
    //
    // Lacking any reasonable valid interpretation, we treat this case as a multiple-operator error.
    // This is the only case in which we yield a multiple-operator error when there are no spaces
    // between the operators.
    //
    // Similar expressions with missing operands should be treated likewise:
    expect_multiple_operator_error("x = y +-");
    expect_multiple_operator_error("x = +- z");
    expect_multiple_operator_error("x =-");
    expect_multiple_operator_error("=- y");
    expect_multiple_operator_error("=-");
}



// ====================
// === Test Support ===
// ====================


// === Testing helpers ===

/// Check that the given [`Tree`] is a valid representation of the given source code:
/// - Assert that the given [`Tree`] is composed of tokens that concatenate back to the given source
/// code.
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
fn test(code: &str, expect: lexpr::Value) {
    let ast = parse(code);
    let ast_s_expr = to_s_expr(&ast, code);
    assert_eq!(ast_s_expr.to_string(), expect.to_string(), "{:?}", &ast);
    expect_tree_representing_code(code, &ast);
}

fn parse(code: &str) -> enso_parser::syntax::tree::Tree {
    let ast = enso_parser::Parser::new().run(code);
    let expected_span = 0..(code.encode_utf16().count() as u32);
    let mut locations = enso_parser::source::code::debug::LocationCheck::new();
    enso_parser_debug::validate_spans(&ast, expected_span, &mut locations);
    locations.check(code);
    ast
}


// === Testing inputs containing syntax errors ===

#[derive(Debug, Eq, PartialEq, Default, Copy, Clone)]
struct Errors {
    invalid_node:      bool,
    multiple_operator: bool,
}

impl Errors {
    fn collect(code: &str) -> Self {
        let ast = parse(code);
        expect_tree_representing_code(code, &ast);
        let errors = core::cell::Cell::new(Errors::default());
        ast.map(|tree| match &*tree.variant {
            enso_parser::syntax::tree::Variant::Invalid(_) => {
                errors.update(|e| Self { invalid_node: true, ..e });
            }
            enso_parser::syntax::tree::Variant::OprApp(opr_app) if opr_app.opr.is_err() => {
                errors.update(|e| Self { multiple_operator: true, ..e });
            }
            _ => (),
        });
        errors.into_inner()
    }
}

/// Checks that an input contains an `Invalid` node somewhere.
fn expect_invalid_node(code: &str) {
    let errors = Errors::collect(code);
    assert!(errors.invalid_node, "{:?}", enso_parser::Parser::new().run(code));
}

/// Checks that an input contains a multiple-operator error somewhere.
fn expect_multiple_operator_error(code: &str) {
    let errors = Errors::collect(code);
    assert!(errors.multiple_operator, "{:?}", enso_parser::Parser::new().run(code));
}

/// Check that the input can be parsed, and doesn't yield any `Invalid` nodes.
fn expect_valid(code: &str) {
    let errors = Errors::collect(code);
    assert!(!errors.invalid_node);
}
