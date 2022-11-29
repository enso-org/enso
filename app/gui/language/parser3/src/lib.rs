#![feature(box_patterns)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use ast::Ast;
use ast::Shape;
use enso_parser::syntax::tree;
use wasm_bindgen_test::wasm_bindgen_test;



fn test(source_code: &str) {
    // Run Rust parser.
    let rust_parser = enso_parser::Parser::new();
    let rust_output = rust_parser.run(source_code);
    let box tree::Variant::BodyBlock(rust_block) = rust_output.variant else {
        panic!()
    };
    let rust_output = enso_parser_bridge::translate_module(&rust_block);
    // Run Scala parser.
    let scala_parser = parser_scala::Parser::new_or_panic();
    let scala_output = scala_parser.parse(source_code.to_owned(), Default::default()).unwrap();
    // Check equivalence.
    check(&rust_output, &scala_output);
}

fn check(left: &Ast, right: &Ast) {
    match (&left.wrapped.wrapped, &right.wrapped.wrapped) {
        //(Shape::Unexpected(_), Shape::Unexpected(_)) => {}
        //(Shape::InvalidQuote(_), Shape::InvalidQuote(_)) => {}
        //(Shape::InlineBlock(_), Shape::InlineBlock(_)) => {}
        //(Shape::InvalidSuffix(_), Shape::InvalidSuffix(_)) => {}
        (Shape::Prefix(a), Shape::Prefix(b)) => {
            check(&a.func, &b.func);
            check(&a.arg, &b.arg);
            assert_eq!(a.off, b.off);
        }
        (Shape::Infix(a), Shape::Infix(b)) => {
            let ast::Infix { larg: larg0, loff: loff0, opr: opr0, roff: roff0, rarg: rarg0 } = a;
            let ast::Infix { larg: larg1, loff: loff1, opr: opr1, roff: roff1, rarg: rarg1 } = b;
            check(opr0, opr1);
            check(larg0, larg1);
            check(rarg0, rarg1);
            assert_eq!(loff0, loff1);
            assert_eq!(roff0, roff1);
        }
        (Shape::SectionLeft(a), Shape::SectionLeft(b)) => {
            check(&a.arg, &b.arg);
            check(&a.opr, &b.opr);
            assert_eq!(a.off, b.off);
        }
        (Shape::SectionRight(a), Shape::SectionRight(b)) => {
            check(&a.opr, &b.opr);
            check(&a.arg, &b.arg);
            assert_eq!(a.off, b.off);
        }
        (Shape::SectionSides(a), Shape::SectionSides(b)) => check(&a.opr, &b.opr),
        (Shape::Module(a), Shape::Module(b)) => {
            for (a, b) in a.lines.iter().zip(&b.lines) {
                match (&a.elem, &b.elem) {
                    (Some(a), Some(b)) => check(a, b),
                    _ => assert_eq!(a, b),
                }
            }
            assert_eq!(a.lines.len(), b.lines.len());
        }
        (Shape::Block(lblock), Shape::Block(rblock)) => {
            assert_eq!(lblock.indent, rblock.indent);
            assert_eq!(&lblock.empty_lines, &rblock.empty_lines);
            check(&lblock.first_line.elem, &rblock.first_line.elem);
            for (a, b) in lblock.lines.iter().zip(&rblock.lines) {
                match (&a.elem, &b.elem) {
                    (Some(a), Some(b)) => check(a, b),
                    _ => assert_eq!(a, b),
                }
            }
            assert_eq!(lblock.lines.len(), rblock.lines.len());
            assert_eq!(lblock.is_orphan, rblock.is_orphan);
        }
        (Shape::Number(a), Shape::Number(b)) => {
            assert_eq!(&a.int, &b.int);
            assert_eq!(&a.base, &b.base);
        }
        //(Shape::Match(_), Shape::Match(_)) => {}
        //(Shape::Ambiguous(_), Shape::Ambiguous(_)) => {}
        (a, b) => assert_eq!(a, b),
        // Spaceless AST:
        //(Shape::Comment(_), Shape::Comment(_)) => {}
        //(Shape::Documented(_), Shape::Documented(_)) => {}
        //(Shape::Import(_), Shape::Import(_)) => {}
        //(Shape::Export(_), Shape::Export(_)) => {}
        //(Shape::JavaImport(_), Shape::JavaImport(_)) => {}
        //(Shape::Mixfix(_), Shape::Mixfix(_)) => {}
        //(Shape::Group(_), Shape::Group(_)) => {}
        //(Shape::SequenceLiteral(_), Shape::SequenceLiteral(_)) => {}
        //(Shape::TypesetLiteral(_), Shape::TypesetLiteral(_)) => {}
        //(Shape::Def(_), Shape::Def(_)) => {}
        //(Shape::Foreign(_), Shape::Foreign(_)) => {}
        //(Shape::Modified(_), Shape::Modified(_)) => {}
    }
}

wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);




#[wasm_bindgen_test]
fn test_numeric_literal() {
    test("main =\n    4");
}

/*
#[wasm_bindgen_test]
fn test_parse_main_7_foo() {
    test("main = 7.foo");
}

#[wasm_bindgen_test]
fn test_simple_arithmetic_expression() {
    test("main = 2 + 45 * 20");
}

#[wasm_bindgen_test]
fn test_applications_and_method_calls() {
    test("main = (2-2 == 0).if_then_else (List.Cons 5 6) 0");
}

#[wasm_bindgen_test]
fn test_assignment_of_variable_reads() {
    test(r#"
    main =
        x = 2 + 2 * 2
        y = x * x
        IO.println y
    "#);
}

#[wasm_bindgen_test]
fn test_method_with_complex_body() {
    test(r#"
    foo a b =
        x : Number
        x = a + 1
        y = b - 2
        x * y
    "#);
}

#[wasm_bindgen_test]
fn test_build_function_simple() {
    test(r#"
    main =
        foo a = a + 1
        foo 42
    "#);
}
*/

/*
  @Test
  public void testLocationsDeeplyNestedFunctions() throws Exception {
    parseTest("""
        foo = a -> b ->
            IO.println a
        """, true, true, true);
  }

  @Test
  public void testLocationsDeeplyNestedFunctionsNoBlock() throws Exception {
    parseTest("""
    Nothing.method =
        add = a -> b -> a + b

    main = Nothing.method
    """, true, true, true);
  }

  @Test
  public void testSpacesAtTheEndOfFile() throws Exception {
    var fourSpaces = "    ";
    parseTest("""
    main = add_ten 5
    """ + fourSpaces);
  }

  @Test
  public void testCase() throws Exception {
    parseTest("""
    type Msg
        Ahoj
        Ciao

    c x = case x of
        Ahoj -> 0
        Ciao -> 1
        Msg.Ciao -> 2
    """
    );
  }

  @Test
  public void testTypeMethodWithSignature() throws Exception {
    parseTest("""
    @Builtin_Type
    type Fuzzy
        == : Correct -> Wrong
        == self right = @Builtin_Method "Fuzzy.=="
    """
    );
  }

  @Test
  public void testImport() throws Exception {
    parseTest("""
    import Standard.Base.Any.Any
    import project.IO
    import Standard.Base as Enso_List
    from Standard.Base import all hiding Number, Boolean, Decimal, Any
    polyglot java import java.lang.Float
    polyglot java import java.net.URI as Java_URI

    main = 3
    """);
  }

  @Test
  public void testImportAll() throws Exception {
    parseTest("""
    ## TODO Dubious constructor export
    from project.Network.Http.Version.Version import all
    from project.Network.Http.Version.Version export all
    """);
  }

  @Test
  public void testImportTrue() throws Exception {
    parseTest("""
    from Standard.Base import True
    """);
  }

  @Test
  public void testMeaningOfWorld() throws Exception {
    parseTest("""
    import Standard.Base.IO

    main = IO.println 42
    """);
  }

  @Test
  public void testMinusOne() throws Exception {
    parseTest("""
    minus n = n-1
    """);
  }

  @Test
  public void testIfNothingSelf() throws Exception {
    parseTest("""
    if_nothing self ~_ = self
    """);
  }

  @Test
  public void testIfSomethingSelf() throws Exception {
    parseTest("""
    if_nothing self ~ignore = self
    """);
  }

  @Test
  public void testMinusRec() throws Exception {
    parseTest("""
    minus n = minus n-1
    """);
  }

  @Test
  public void testFactorial() throws Exception {
    parseTest("""
    fac n = if n == 1 then 1 else n * fac n-1
    """);
  }

  @Test
  public void testIsDigitWithSpaces() throws Exception {
    parseTest("""
    compare =
        is_digit = character -> 42
    """);
  }

  @Test
  public void testComments() throws Exception {
    parseTest("""
    # a b c
    """);
  }

  @Test
  public void testCaseTypeOf() throws Exception {
    parseTest("""
    cmp self = case self of
        v:Vector_2d -> x
        _ -> x
    """);
  }

  @Test
  public void testCaseTypeOf2() throws Exception {
    parseTest("""
    cmp self = case self of
        v:My_Type -> x
    """);
  }

  @Test
  public void testCaseTypeOfWithSpace() throws Exception {
    parseTest("""
    filter self filter = case filter of
        _ : Filter -> 42
    """);
  }

  @Test
  public void testAnnotation0() throws Exception {
    parseTest("""
    dont_stop = @Tail_Call dont_stop
    """);
  }

  @Test
  public void testAnnotation1() throws Exception {
    parseTest("""
    go t = @Tail_Call go t-1
    """);
  }

  @Test
  public void testAnnotation2() throws Exception {
    parseTest("""
    go t x = @Tail_Call go t-1 x
    """);
  }

  @Test
  public void testAnnotationBlock() throws Exception {
    parseTest("""
    go a b = @Tail_Call go
        a
        b
    """);
  }

  @Test
  public void testBuiltinTypeAnnotation() throws Exception {
    parseTest("""
    @Builtin_Type
    type Date
    """);
  }

  @Test
  public void testBoolean() throws Exception {
    parseTest("""
    @Builtin_Type
    type Boolean
        True
        False

        == : Boolean -> Boolean
        == self that = @Builtin_Method "Boolean.=="

        && : Boolean -> Boolean
        && self ~that = @Builtin_Method "Boolean.&&"

        not : Boolean
        not self = @Builtin_Method "Boolean.not"

        compare_to : Boolean -> Ordering
        compare_to self that = @Builtin_Method "Boolean.compare_to"

        if_then_else : Any -> Any -> Any
        if_then_else self ~on_true ~on_false = @Builtin_Method "Boolean.if_then_else"

        if_then : Any -> Any | Nothing
        if_then self ~on_true = @Builtin_Method "Boolean.if_then"
    """);
  }

  @Test
  public void testBuiltinMethodAnnotation() throws Exception {
    parseTest("""
    normalize x = @Builtin_Method "File.normalize"
    """);
  }

  @Test
  public void testTextOrNothing() throws Exception {
    parseTest("""
    type Locale
        language : Text | Nothing
    """);
  }

  @Test
  public void testInterval() throws Exception {
    parseTest("""
    type Interval
        Interval_Data (start : Bound.Bound)
    """);
  }

  @Test
  public void testAtEq() throws Exception {
    parseTest("""
    type Array
        == : Array -> Boolean
        == self that =
            if False then True that else
                eq_at i = self.at i == that.at i
                eq_at 0
    """);
  }

  @Test
  public void testNestedBlocks() throws Exception {
    parseTest("""
    type Array
        meaning =
            catch_primitive handler
                42
    """);
  }

  @Test
  public void testSelf1() throws Exception {
    parseTest("""
    contains self elem = self.contains Nothing
    """);
  }

  @Test
  public void testPanic_184119084() throws Exception {
    parseTest("""
    type Foo
        type Bar

    main = 42
    """);
  }

  @Test
  public void testMetadataRaw() throws Exception {
    parseTest("""
    main =
        foo = 42


    #### METADATA ####
    [[{"index": {"value": 17}, "size": {"value": 2}}, "0270bcdf-26b8-4b99-8745-85b3600c7359"]]
    []
    """);
  }

  @Test
  public void testDocumentationComment() throws Exception {
    parseTest("""
    ## A type representing computations that may fail.
    type Maybe
    """);
  }

  @Test
  public void testColumnSelector() throws Exception {
    parseTest("""
    ## Specifies a selection of columns from the table on which an operation is
       going to be performed.
    type Column_Selector
        By_Index (indexes : Vector Integer)
        By_Column (columns : Vector Column)
    """);
  }

  @Test
  public void testAssignments() throws Exception {
    parseTest("""
      from_java_set java_set =
        owner = Vector.new_builder
        group = Vector.new_builder
        others = Vector.new_builder
        """);
  }

  @Test
  public void testNumberTimes() throws Exception {
    parseTest("""
    Standard.Base.Number.times : List Any
    Standard.Base.Number.times self act =
        act
    """);
  }

  @Test
  public void testIfThenBlock() throws Exception {
    parseTest("""
      from_java_set java_set =
        if java_set.contains PosixFilePermission.OWNER_READ then
            owner.append Read
        if java_set.contains PosixFilePermission.OWNER_WRITE then
            owner.append Write
        """);
  }

  @Test
  public void testInvokeFilePermissions() throws Exception {
    parseTest("""
      from_java_set java_set =
        File_Permissions owner.to_vector group.to_vector others.to_vector
        """);
  }

  @Test
  public void testSignature1() throws Exception {
    parseTest("val : Bool");
  }

  @Test
  public void testSignature2() throws Exception {
    parseTest("val : List Int");
  }

  @Test
  public void testSignature3() throws Exception {
    parseTest("val = 123 : Int");
  }

  @Test
  public void testSignature4() throws Exception {
    parseTest("val = foo (123 : Int)");
  }

  @Test
  public void testSignature5() throws Exception {
    parseTest("val : List Int -> Int");
  }

  @Test
  public void testExport1() throws Exception {
    parseTest("export prj.Data.Foo");
  }

  @Test
  public void testExportAs() throws Exception {
    parseTest("export prj.Data.Foo as Bar");
  }

  @Test
  public void testExportFrom() throws Exception {
    parseTest("from prj.Data.Foo export Bar, Baz");
  }

  @Test
  public void testExportFromTen() throws Exception {
    parseTest("from prj.Data.Foo export One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten");
  }

  @Test
  public void testExportFromAllHiding() throws Exception {
    parseTest("from prj.Data.Foo export all hiding Bar, Baz");
  }

  @Test
  public void testTextLiteral() throws Exception {
    parseTest("""
    main = "I'm an inline raw text!"
    """);
  }

  @Test
  public void testTextLiteralWithEscape() throws Exception {
    parseTest("""
    wrap_junit_testsuites = '<?xml version="1.0"\\tencoding="UTF-8"?>\\n'
    """);
  }

  @Test
  public void testRawBlockLiteral() throws Exception {
    // mimics TextTest
    parseTest("""
    x = \"\"\"
        Foo
        Bar
          Baz
    """);
  }

  @Test
  public void testVariousKindsOfUnicodeWhitespace() throws Exception {
    parseTest("""
    t = '\\v\\f\\u{200a}\\u{202f}\\u{205F}\\u{3000}'.trim
    """);
  }

  @Test
  public void testLambda() throws Exception {
    parseTest("""
    f = map _->alphabet
    """);
  }

  @Test
  public void testMultiParameterFunction() throws Exception {
    parseTest("""
    from Standard.Base import all
    import Standard.Base.System

    measure : Any -> Text -> Integer -> Integer -> Nothing
    measure = ~act -> label -> iter_size -> num_iters ->
        42
    """);
  }

  @Test
  public void testTestGroup() throws Exception {
    parseTest("""
    type Test
        ## Creates a new test group, describing properties of the object
           described by `self`.

           Arguments:
           - name: The name of the test group.
           - behaviors: An action containing a set of specs for the group.
           - pending: A reason for why the test is pending, or `Nothing` when it is not
             pending.

           > Example
             Adding a test group.

                 from Standard.Test import Test, Test_Suite

                 example_group = Test_Suite.run <|
                     Test.group "Number" <| Nothing
        group : Text -> Any -> (Text | Nothing) -> Nothing
        """);
  }

  @Test
  public void testEmptyGroup() throws Exception {
    parseTest("""
    main =
        x = Panic.catch Any () .convert_to_dataflow_error
        x.catch_primitive err->
            case err of
                Syntax_Error_Data msg -> "Oopsie, it's a syntax error: " + msg
    """);
  }

  @Test
  public void testEmptyGroup2AndAtSymbol() throws Exception {
    parseTest("""
    main =
        x = ()
        x = 5
        y = @
    """);
  }

  @Test
  public void testTestGroupSimple() throws Exception {
    parseTest("""
    group1 : Text -> Any -> (Text | Nothing) -> Nothing

    type Test
        group2 : Text -> Any -> (Text | Nothing) -> Nothing
    """);
  }

  @Test
  public void testNotAnOperator() throws Exception {
    parseTest("""
    main =
        x = Panic.catch_primitive @ caught_panic-> caught_panic.payload
        x.to_text
    """);
  }

  @Test
  public void testWildcardLeftHandSide() throws Exception {
    parseTest("""
    Any.should_succeed self frames_to_skip=0 =
        _ = frames_to_skip
        """);
  }

  @Test
  public void testReverseListType() throws Exception {
    parseTest("""
    reverse_list : List Any -> List
    reverse_list list = List.Nil
    """);
  }

  @Test
  public void testReverseList() throws Exception {
    parseTest("""
    reverse_list list =
        go = list -> acc -> case list of
            List.Cons h t -> go t (List.Cons h acc)
            List.Cons h _ -> acc
            ListNil -> acc
            _ -> acc
        res = go list List.Nil
        res
    """);
  }

  @Test
  public void testProblemHandling() throws Exception {
    parseTest("""
    test_problem_handling : (Problem_Behavior -> Any) -> Vector Any -> (Any -> Nothing) -> Nothing
    test_problem_handling action expected_problems result_checker =
        result_checker result_ignoring
    """);
  }

  @Test
  public void testProblemHandling2() throws Exception {
    parseTest("""
    test_problem_handling action expected_problems result_checker =
        error_result . should_fail_with first_problem_type frames_to_skip=3
        warnings_checker warnings =
            ## TODO [RW] we are not checking if there are no duplicate warnings, because the warnings are in fact duplicated - we should figure out how to handle that and then possibly modify the test
            warnings . should_contain_the_same_elements_as expected_problems frames_to_skip=3
    """);
  }

  @Test
  public void testNamedArgument() throws Exception {
    parseTest("""
    fn = get_all frames_to_skip=1
    """);
  }

  @Test
  public void testVectorLiteralEmpty() throws Exception {
    parseTest("""
    fn = []
    """);
  }

  @Test
  public void testVectorLiteralOne() throws Exception {
    parseTest("""
    fn = [ 1 ]
    """);
  }

  @Test
  public void testVectorLiteralMany() throws Exception {
    parseTest("""
    fn = [ 1, 2, 3 ]
    """);
  }

  @Test
  public void testInvokeMethod() throws Exception {
    parseTest("""
    fn = result_ignoring . should_equal
    """);
  }

  @Test
  public void testTableDataArgumentInCase() throws Exception {
    parseTest("""
    process_to_json_text value =
        json = case value of
            Table.Table_Data _ -> json_from_table value
            _ -> value.to_json
        """);
  }

  @Test
  public void testVisualizationCaseOf() throws Exception {
    parseTest("""
    prepare_visualization : Any -> Integer -> Json
    prepare_visualization x max_rows=1000 = case x of
        Array ->
            prepare_visualization (Vector.from_polyglot_array x) max_rows

        # Anything else will be visualized with the JSON or matrix visualization
        _ ->
            Json.from_pairs [["json", x]] . to_text
        """);
  }

  @Test
  public void testAggregateColumnGroupByTrue() throws Exception {
    parseTest("""
    prepare_aggregate_columns : [Aggregate_Column] -> Table -> Problem_Behavior -> Resolved_Aggregate_Columns
    prepare_aggregate_columns aggregates table =
        # Grouping Key
        is_a_key c = case c of
            Aggregate_Column.Group_By _ _ -> True
            _ -> False
      """);
  }

  @Test
  public void testUnaryDot() throws Exception {
    parseTest("""
    write_file = ExcelWriter.setEnsoToTextCallbackIfUnset (.to_text)
      """);
  }

  @Test
  public void testUnaryMinus() throws Exception {
    parseTest("""
    meaning = -42
      """);
  }

  @Test
  public void testExtensionMethod() throws Exception {
    parseTest("""
    Any.meaning = -42
      """);
  }

  @Test
  public void testExtensionOperator() throws Exception {
    parseTest("""
    Text.* : Integer -> Text
    Text.* self = 42
    """);
  }

  @Test
  public void testTypeSignature() throws Exception {
    parseTest("""
    resolve_aggregate table problem_builder aggregate_column =
        table_columns = table.columns

        resolve : (Integer|Text|Column) -> Column ! Internal_Missing_Column_Error
        resolve c = 42
    """);
  }

  @Test
  public void testTypeSignature2() throws Exception {
    parseTest("""
    type Baz
        resolve : Integer -> Column
    """);
  }

  @Test
  public void testTypeSignatureQualified() throws Exception {
    parseTest("""
    type Baz
        Foo.resolve : Integer -> Column
    """);
  }

  @Test
  public void testMethodDef() throws Exception {
    parseTest("""
    type Foo
        id x = x
    """);
  }

  @Test
  public void testSelfTypeKeyword() throws Exception {
    parseTest("""
    type My_Type
        Cons_A x
        Cons_B y=(Self.Cons_A 10)

        static = 123

        static_use = Self.static + (Self.Cons_A 5).x
        instance_use self = Self.static + self.x + (Self.Cons_A 5).x

        static_match x = case x of
            Self -> "it matched"
            _ -> "it didn't match"

        matching_method self = case self of
            Self.Cons_A y -> y + 2

        match_by_type x = case x of
            _ : Self -> "it's a Self"
            _ -> "it's a something else"
    """);
  }



  @Test
  public void testCaseOnTextLiteral() throws Exception {
    parseTest("""
    choose ch = case ch of
        "yes" -> True
        "no" -> False
      """);
  }

  @Test
  public void testCaseWithUnderscore() throws Exception {
    parseTest("""
    choose ch = case ch of
        0 -> _.name
        _ -> Nothing
    """);
  }

  @Test
  public void testCaseWithLowerCaseA() throws Exception {
    parseTest("""
    map_nothing self = case self of
        a -> f a
    """);
  }

  @Test
  public void testBalanceUpperCase() throws Exception {
    parseTest("""
    balance_left k x l r = case r of
        Bin _ lk lx Tip -> 42
      """);
  }

  @Test
  public void testBalanceLowerCase() throws Exception {
    parseTest("""
    balance_left k x l r = case r of
        Bin _ lk lx tip -> 42
      """);
  }

  @Test
  public void testVectorVectorSimple() throws Exception {
    parseTest("""
    type Vector
        build : Matrix Any Decimal
    """);
  }

  @Test
  public void testVectorVectorAny() throws Exception {
    parseTest("""
    type Vector
        build : Standard.Base.Vector.Matrix Standard.Base.Any Standard.Base.Decimal
    """);
  }

  @Test
  public void testCaseOfVector() throws Exception {
    parseTest("""
        m other = case other of
            _:Vector.Vector -> 0
        """);
  }

  @Test
  public void testOperatorSectionRight() throws Exception {
    parseTest("""
    type Filter_Condition
        to_predicate self = case self of
            Less value -> <value
    """);
  }

  @Test
  public void testAutoScope() throws Exception {
    parseTest("""
    fn that_meta =
        c_2 = that_meta.constructor ...
        """);
  }

  @Test
  public void testAutoScope2() throws Exception {
    parseTest("""
    fn1 = fn ...
    fn2 = fn 1 ...
    """);
  }

  @Test
  public void testForcedTerms() throws Exception {
    parseTest("""
    ifTest = c -> (~ifT) -> ~ifF -> if c == 0 then ifT else ifF
    """);
  }

  @Test
  public void testTextArrayType() throws Exception {
    parseTest("""
    type Connection
        table_types : [Text]
    """);
  }

  @Test
  public void testListBody() throws Exception {
    parseTest("""
          list directory name_filter=Nothing recursive=False =
              new directory . list name_filter=name_filter recursive=recursive
                  """);
  }

  @Test
  public void testLambdaBody() throws Exception {
    parseTest("""
    list =
        all_files.filter file->
            all_files
    """);
  }

  @Test
  public void testCaseWithComment() throws Exception {
    parseTest("""
    ansi_bold : Boolean -> Text -> Text
    ansi_bold enabled txt =
        case Platform.os of
            ## Output formatting for Windows is not currently supported.
            Platform.OS.Windows -> txt
            _ -> if enabled then Nothing
    """);
  }

  @Test
  @Ignore // Crashes old parser
  public void testAlternationTypes() throws Exception {
    parseTest("""
    foo : [Integer | Text] -> (Integer | Text)
    foo v = v.at 0
    """);
  }

  @Test
  public void testGroupOfPatterns() throws Exception {
    parseTest("""
    sum self = case self of
        Group (A _) (B _ _) (C _ e _) (D _ f _ g) -> e + f + g
    """);
  }

  @Test
  public void testNameAsMethodApp() throws Exception {
    parseTest("""
    f = foo x=A.B
    """);
  }

  @Test
  public void testIsMethodWithSpaces() throws Exception {
    parseTest("""
    f = 0.up_to . all f
    """);
  }

  @Test
  public void testIsMethodWithoutSpaces() throws Exception {
    parseTest("""
    f = 0.up_to.all f
    """);
  }

  @Test
  public void testHandleRequestError() throws Exception {
    parseTest("""
    request self req =
        handle_request_error =
            42
    """);
  }

  @Test
  public void testWriteFlag() throws Exception {
    parseTest("""
    type Write_Flag
        JPEG_Quality val:Integer=95
    """);
  }

  @Test
  public void testHasDefaultsSuspended() throws Exception {
    parseTest("""
    Atom.constructor self = get_atom_constructor self.value ...
    """);
  }

  @Test
  public void testVectorVector() throws Exception {
    parseTest("""
    get_stack_trace : Vector.Vector Stack_Trace_Element
    """);
  }

  @Test
  public void testSidesPlus() throws Exception {
    parseTest("""
    result = reduce (+)
    """);
  }

  @Test
  public void testConstructorMultipleNamedArgs1() throws Exception {
    parseTest("""
    x = Regex_Matcher.Regex_Matcher_Data case_sensitivity=Case_Sensitivity.Sensitive dot_matches_newline=True
    """);
  }

  @Test
  public void testDocAtEndOfBlock() throws Exception {
    parseTest("""
    x =
      23
      ## end of block
    """);
  }

  @Test
  public void testMethodSections() throws Exception {
    parseTest("""
    x = .from self=Foo
    """);
  }

  @Test
  public void testGroupArgument() throws Exception {
    parseTest("""
    foo = x -> (y = bar x) -> x + y
    """);
  }

  @Test
  public void testRuntimeServerTestCode() throws Exception {
    parseTest("""
    from Standard.Base.Data.Numbers import Number

    main =
        x = 6
        y = x.foo 5
        z = y + 5
        z

    Number.foo self = x ->
        y = self + 3
        z = y * x
        z
    """, true, true, true);
  }

  @Test
  public void testResolveExecutionContext() throws Exception {
    parseTest("""
    foo : A -> B -> C in Input
    """);
  }

  @Test
  public void testSugaredFunctionDefinition() throws Exception {
    parseTest("""
    main =
        f a b = a - b
        f 10 20
    """);
  }

  @Test
  public void testAtomBenchmarks1() throws Exception {
    parseTest("""
    import Standard.Base.Data.List.List

    main =
        generator fn acc i end = if i == end then acc else @Tail_Call generator fn (fn acc i) i+1 end
        res = generator (acc -> x -> List.Cons x acc) List.Nil 1 1000000
        res
    """);
  }

  @Test
  public void testAtomBenchmarks3() throws Exception {
    parseTest("""
    import Standard.Base.Data.List.List

    List.mapReverse self f acc = case self of
        List.Cons h t -> @Tail_Call t.mapReverse f (List.Cons (f h) acc)
        _ -> acc

    main = list ->
        res = list.mapReverse (x -> x + 1) List.Nil
        res
    """, true, true, false);
  }

  @Test
  public void testShouldQuoteValuesContainingTheCommentSymbol() throws Exception {
    parseTest("""
    suite =
        Test.specify "should quote values containing the comment symbol if comments are enabled" <|
            format = Delimited ',' . with_comments
            table.write file format on_problems=Report_Error . should_succeed
            expected_text_2 = normalize_lines <| \"""
                "#",B
                b,
                x,"#"
                "#",abc
            text_2 = File.read_text file
            text_2.should_equal expected_text_2
            file.delete
    """, true, true, false);
  }

  @Test
  public void testEmptyValueBetweenComments() throws Exception {
    parseTest("""
    expected_text = normalize_lines <| \"""
                A,B
                1,
                ,""
                3,abc
    """, true, true, false);
  }

  @Test
  public void testQuotedValues() throws Exception {
    parseTest("""
    expected_text = normalize_lines <| \"""
        "one, two, three",-1.5,42,"4\"000",
    """, true, true, false);
  }

  @Test
  public void testSimpleTripleQuote() throws Exception {
    parseTest("""
    expected_response = Json.parse <| '''
        {
          "headers": {
            "Content-Length": "13",
            "Content-Type": "application/json",
            "User-Agent": "Java-http-client/11.0.13"
          },
          "origin": "127.0.0.1",
          "url": "",
          "args": {},
          "data": "{\\"key\\":\\"val\\"}",
          "files": null,
          "form": null,
          "json": {
            "key": "val"
          }
        }
    json = Json.parse <| '''
        {"key":"val"}
    res = Http.new.post_json url_post json
        """, true, true, false);
  }

  @Test
  public void testInThePresenceOfComments() throws Exception {
    parseTest("""
    # this is a comment
    #this too
    ## But this is a doc.
    main = # define main
        y = 1 # assign one to `y`
        x = 2 # assign two to #x
        # perform the addition
        x + y # the addition is performed here
    """);
  }

  @Test
  public void testNPE183892665() throws Exception {
    parseTest("""
    foo : Integer ->
    """);
  }

  @Test
  public void testNamedDefaultedArguments183953473() throws Exception {
    parseTest("""
    main = @Tail_Call summator (current = 0) (acc = 1)
    """);
  }
 */

/////////////////

#[wasm_bindgen_test]
fn nothing() {
    test("");
}

#[wasm_bindgen_test]
fn application() {
    test("a b c");
}

/*
#[wasm_bindgen_test]
fn parentheses() {
    test("(a b)");
    test("(a) (b)");
    test("((a b) c)");
}

#[wasm_bindgen_test]
fn section_simple() {
    test("+ a");
    test("a +");
}

#[wasm_bindgen_test]
fn inline_if() {
    test("if True then True else False");
}

#[wasm_bindgen_test]
fn then_block() {
    test("if True then\n True");
}

#[wasm_bindgen_test]
fn else_block() {
    test("if True then True else\n False");
}
 */


// === Comments ===

/*
#[wasm_bindgen_test]
fn plain_comments() {
    test("# a b c");
    test("main = # define main\n 4");
}

#[wasm_bindgen_test]
fn doc_comments() {
    #[rustfmt::skip]
    let lines = vec![
        "## The Identity Function",
        "",
        "   Arguments:",
        "   - x: value to do nothing to",
        "id x = x",
    ];
    test(&lines.join("\n"));
    #[rustfmt::skip]
    let lines = vec![
        " ## Test indent handling",
        " foo",
    ];
    test(&lines.join("\n"));
}
 */


// === Type Definitions ===

/*
#[wasm_bindgen_test]
fn type_definition_no_body() {
    test("type Bool");
    test("type Option a");
    test("type Option (a)");
    test("type Foo (a : Int)");
    test("type A a=0");
    test("type Existing_Headers (column_names : Vector Text)");
}

#[wasm_bindgen_test]
fn type_constructors() {
    let code = [
        "type Geo",
        "    Circle",
        "        radius",
        "        x",
        "    Rectangle width height",
        "    Point",
    ];
    test(&code.join("\n"));
    test("type Foo\n Bar (a : B = C.D)");
    test("type Foo\n ## Bar\n Baz");
    let code = ["type A", "    Foo (a : Integer, b : Integer)"];
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
fn type_methods() {
    let code = ["type Geo", "    number =", "        x", "    area self = x + x"];
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
fn type_operator_methods() {
    #[rustfmt::skip]
    let code = [
        "type Foo",
        "    + : Foo -> Foo -> Foo",
        "    + self b = b",
        "    Foo.+ : Foo",
        "    Foo.+ self b = b",
    ];
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
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
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
fn type_def_defaults() {
    let code = ["type Result error ok=Nothing", "    Ok value:ok = Nothing"];
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
fn type_def_nested() {
    #[rustfmt::skip]
        let code = [
        "type Foo",
        "    type Bar",
        "    type Baz",
    ];
    test(&code.join("\n"));
}
 */


// === Variable Assignment ===

#[wasm_bindgen_test]
fn assignment_simple() {
    test("foo = x");
}


// === Functions ===

#[wasm_bindgen_test]
fn function_inline_simple_args() {
    test("foo a = x"); // Different representation of initial indent in old Ast
    test("foo a b = x");
    test("foo a b c = x");
    test("foo _ = x");
    //test(" foo a = x"); // Different representation of initial indent in old Ast
    //test(" foo _ = x"); // Different representation of initial indent in old Ast
}

#[wasm_bindgen_test]
fn function_block_noargs() {
    test("foo =");
}

#[wasm_bindgen_test]
fn function_block_simple_args() {
    test("foo a =");
    test("foo a b =");
    test("foo a b c =");
}

#[wasm_bindgen_test]
fn function_qualified() {
    test("Id.id x = x");
}

/*
#[wasm_bindgen_test]
fn ignored_arguments() {
    test("f ~_ = x");
}
 */

/* TODO: Text
#[wasm_bindgen_test]
fn foreign_functions() {
    test("foreign python my_method a b = \"42\"");
}
 */


// === Named arguments ===

#[wasm_bindgen_test]
fn named_arguments() {
    test("f x=y");
    //test("f (x = y)");
}


// === Default arguments ===

#[wasm_bindgen_test]
fn default_app() {
    test("f default");
}

#[wasm_bindgen_test]
fn argument_named_default() {
    test("f default x = x");
    test("f x default = x");
}

#[wasm_bindgen_test]
fn default_arguments() {
    test("f x=1 = x");
    //test("f (x = 1) = x");
    //test("f ~x=1 = x");
    //test("f (~x = 1) = x");
}


// === Code Blocks ===

#[wasm_bindgen_test]
fn code_block_body() {
    let code = ["main =", "    x"];
    test(&code.join("\n"));
    /* Trailing whitespace representation not implemented yet
    let code = ["main =", "      ", "    x"];
    test(&code.join("\n"));
    let code = ["main =", "    ", "    x"];
    test(&code.join("\n"));
    let code = ["main =", "  ", "    x"];
    test(&code.join("\n"));
     */
    let code = ["main =", "", "    x"];
    test(&code.join("\n"));
    #[rustfmt::skip]
    let code = [
        "main =",
        "    +x",
        "    print x",
    ];
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
fn code_block_operator() {
    let code = ["value = nums", "    * each random", "    + constant"];
    test(&code.join("\n"));
}

/* Old parser doesn't handle precedence of operator-block leading operator correctly
#[wasm_bindgen_test]
fn dot_operator_blocks() {
    let code = ["rect1", "    . width = 7", "    . center", "        + x"];
    test(&code.join("\n"));
}
 */

#[wasm_bindgen_test]
fn code_block_argument_list() {
    #[rustfmt::skip]
    let code = [
        "value = foo",
        "    bar",
    ];
    test(&code.join("\n"));


    #[rustfmt::skip]
        let code = [
        "value = foo",
        "    +x",
        "    bar",
    ];
    test(&code.join("\n"));
}

/* Old parser handling is different from language spec
#[wasm_bindgen_test]
fn code_block_bad_indents1() {
    let code = ["main =", "  foo", " bar", "  baz"];
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
fn code_block_bad_indents2() {
    let code = ["main =", "  foo", " bar", "baz"];
    test(&code.join("\n"));
}
 */

#[wasm_bindgen_test]
fn code_block_with_following_statement() {
    let code = ["main =", "    foo", "bar"];
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
fn operator_block_nested() {
    let code = ["foo", "    + bar", "        - baz"];
    test(&code.join("\n"));
}

/* Old parser precedence wrong?
#[wasm_bindgen_test]
fn operator_section_in_operator_block() {
    let code = ["foo", "    + bar +"];
    test(&code.join("\n"));
}
 */


// === Binary Operators ===

#[wasm_bindgen_test]
fn precedence() {
    test("x * y + z");
    test("x + y * z");
    test("w + x + y * z");
    test("x - 1 + 2");
}

/* Old parser is wrong
#[wasm_bindgen_test]
fn dot_operator_precedence() {
    test("x y . f v");
}
 */

#[wasm_bindgen_test]
fn right_associative_operators() {
    test("x --> y ---> z");
    test("x <| y <<| z");
}

#[wasm_bindgen_test]
fn left_associative_operators() {
    test("x + y + z");
}

#[wasm_bindgen_test]
fn pipeline_operators() {
    test("f <| a");
    test("a |> f");
}

#[wasm_bindgen_test]
fn accessor_operator() {
    // Test that the accessor operator `.` is treated like any other operator.
    test("Console.");
    test(".");
    test(".log");
}

#[wasm_bindgen_test]
fn operator_sections() {
    //test(".map (+2 * 3) *7");
    test(".sum 1");
    test("+1 + x");
    test("increment = 1 +");
}

#[wasm_bindgen_test]
fn template_functions() {
    //test("_.map (_+2 * 3) _*7");
    test("_.sum 1");
    test("_+1 + x");
}


// === Unary Operators ===

#[wasm_bindgen_test]
fn unevaluated_argument() {
    let code = ["main ~foo = x"];
    test(&code.join("\n"));
}

/*
#[wasm_bindgen_test]
fn unary_operator_missing_operand() {
    let code = ["main ~ = x"];
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
fn unary_operator_at_end_of_expression() {
    let code = ["foo ~"];
    test(&code.join("\n"));
}
 */

/* Old parser is wrong
#[wasm_bindgen_test]
fn plus_negative() {
    test("x = x+-x");
}
 */

#[wasm_bindgen_test]
fn minus_binary() {
    test("x - x");
    test("x-x");
    // test("x.-y"); // Old parser is wrong
    // test("x.~y"); // Old parser uses a non-operator representation of `~`
}

#[wasm_bindgen_test]
fn minus_section() {
    test("- x");
    //test("(- x)");
    //test("- (x * x)");
}

#[wasm_bindgen_test]
fn minus_unary() {
    test("f -x");
    test("-x");
    //test("(-x)");
    //test("-(x * x)");
    //test("x=-x"); // Old parser is completely wrong
    test("-x+x");
    //test("-x*x"); // Old parser gets precedence wrong
    //test("-2.1");
}


// === Import/Export ===

/*
#[wasm_bindgen_test]
fn import() {
    test("import project.IO");
    test("import Standard.Base as Enso_List");
    test("from Standard.Base import all");
    test("from Standard.Base import all hiding Number, Boolean");
    test("polyglot java import java.lang.Float");
    test("polyglot java import java.net.URI as Java_URI");
    test("from Standard.Base import Foo, Bar, Baz");
}

#[wasm_bindgen_test]
fn export() {
    test("export prj.Data.Foo");
    test("export Foo as Bar");
    test("from Foo export Bar, Baz");
    test("from Foo export all hiding Bar, Baz");
}
 */


// === Type annotations and signatures ===

#[wasm_bindgen_test]
fn type_signatures() {
    test("val : Bool");
    test("val : List Int");
    //test("foo : [Integer | Text] -> (Integer | Text)");
}

#[wasm_bindgen_test]
fn type_annotations() {
    test("val = x : Int");
    //test("val = foo (x : Int)");
    //test("(x : My_Type _)");
    //test("x : List Int -> Int");
}


// === Text Literals ===

/*
#[wasm_bindgen_test]
fn inline_text_literals() {
    test(r#""I'm an inline raw text!""#);
    test(r#"zero_length = """#);
    test(r#""type""#);
    test(r#"unclosed = ""#);
    test(r#"unclosed = "a"#);
    test(r#"'Other quote type'"#);
    test(r#""Non-escape: \n""#);
    test(r#""Non-escape: \""#);
    test(r#"'String with \' escape'"#);
    test(r#"'\u0915\u094D\u0937\u093F'"#);
    test(r#"('\n')"#);
    test(r#"`"#);
    test(r#"(")")"#);
    test(r#"'\x'"#);
    test(r#"'\u'"#);
    test(r#"'\U'"#);
}

#[wasm_bindgen_test]
fn multiline_text_literals() {
    test("'''");
    let code = r#""""
        part of the string
           3-spaces indented line, part of the Text Block
        this does not end the string -> '''

        `also` part of the string

    x"#;
    test(code);
    let code = r#""""
        multiline string that doesn't end in a newline
    x"#;
    test(code);
    let code = "  x = \"\"\"\n    Indented multiline\n  x";
    test(code);
    let code = "'''\n    \\nEscape at start\n";
    test(code);
    let code = "x =\n x = '''\n  x\nx";
    test(code);
    test("foo = bar '''\n baz");
    test("'''\n \\t'");
    test("'''\n x\n \\t'");
}

#[wasm_bindgen_test]
fn interpolated_literals_in_inline_text() {
    test(r#"'Simple case.'"#);
    test(r#"'String with \n escape'"#);
    test(r#"'\x0Aescape'"#);
    test(r#"'\u000Aescape'"#);
    test(r#"'\u{0000A}escape'"#);
    test(r#"'\U0000000Aescape'"#);
    // test(r#"'With a `splice`.'"#); // Old parsser doesn't support splices.
    // test(r#"'` SpliceWithLeadingWhitespace`'"#); // Old parsser doesn't support splices.
}


// === Lambdas ===

#[wasm_bindgen_test]
fn new_lambdas() {
    test(r#"\v -> v"#);
    test(r#"\a b -> x"#);
}
 */

/* MACRO
#[wasm_bindgen_test]
fn old_lambdas() {
    test("x -> y");
    test("x->y");
    test("x-> y");
    test("x->\n y");
    test("x ->\n y");
    test("f x->\n y");
    test("x->y-> z");
}
 */


// === Pattern Matching ===

/* Not supported by backend yet.
#[wasm_bindgen_test]
fn pattern_irrefutable() {
    test("Point x_val = my_point");
    test("Vector _ = x");
}
 */

/*
#[wasm_bindgen_test]
fn case_expression() {
    #[rustfmt::skip]
    let code = [
        "case a of",
        "    Some -> x",
        "    Int -> x",
    ];
    test(&code.join("\n"));

    #[rustfmt::skip]
    let code = [
        "case a of",
        "    Vector_2d x y -> x",
    ];
    test(&code.join("\n"));

    #[rustfmt::skip]
    let code = [
        "case self of",
        "    Vector_2d -> x",
        "    _ -> x",
    ];
    test(&code.join("\n"));
    #[rustfmt::skip]
    let code = [
        "case foo of",
        "    v:My_Type -> x",
        "    v:(My_Type _ _) -> x",
    ];
    test(&code.join("\n"));
}

#[wasm_bindgen_test]
fn case_by_type() {
    macro_rules! test_case {
        ( $code:expr ) => {
            test(&format!("case foo of\n {}", $code));
        }
    }
    test_case!("f:A->B -> x");
    test_case!("f : A->B -> x");
    test_case!("v : A -> x->x");
    test_case!("v : A -> x -> x");
    test_case!("v:A->x->x");
    test_case!("v:A->x");
    test_case!("v : A -> _ + x");
}

#[wasm_bindgen_test]
fn pattern_match_auto_scope() {
    #[rustfmt::skip]
    let code = [
        "case self of",
        "    Vector_2d ... -> x",
    ];
    test(&code.join("\n"));
}


// === Array/tuple literals ===

#[wasm_bindgen_test]
fn array_literals() {
    test("[]");
    test("[x]");
    test("[x, y]");
    test("[x, y, z]");
    test("[ x , y ]");
    test("[ x , y , z ]");
}

#[wasm_bindgen_test]
fn tuple_literals() {
    test("{}");
    test("{x}");
    test("{x, y}");
}
 */


// === Numeric literals ===

#[wasm_bindgen_test]
fn numbers() {
    test("1 . 0");
    test("1 .0");
    test("1. 0");
    // test("pi = 3.14"); // ?
}

/* Not supported by old parser.
#[wasm_bindgen_test]
fn new_nondecimal_numbers() {
    test("0b10101010");
    test("0o122137");
    test("0xAE2F14");
}
 */

/*
#[wasm_bindgen_test]
// This syntax cannot be used until we remove old-nondecimal number support, which is
// needed for compatibility until the old parser is fully replaced.
#[ignore]
fn new_delimited_numbers() {
    test!("100_000", (Number () "100_000" ()));
    test!("10_000.99", (Number () "10_000" ("." "99")));
}
 */

/*
#[wasm_bindgen_test]
fn old_nondecimal_numbers() {
    test("2_01101101");
    test("16_17ffffffffffffffa");
}


// === Whitespace ===

#[wasm_bindgen_test]
fn trailing_whitespace() {
    test("a ");
    test("a \n");
    test("a = \n x");
}
*/


// === Annotations ===

/* Old parser representation of this is bizarre.
#[wasm_bindgen_test]
fn at_operator() {
    test("foo@bar");
    test("foo @ bar");
}
 */


/*
#[wasm_bindgen_test]
fn attributes() {
    test("@on_problems P.g\nTable.select_columns : Text -> Table");
    test("@a z\n@b\nx");
    test("@a\n@b\nx");
}
*/

#[wasm_bindgen_test]
fn inline_builtin_annotations() {
    test("@Tail_Call go");
    // FIXME: In the old parser, annotations bind tighter than application...
    // test("@Tail_Call go t");
    // test("@Tail_Call go\n a\n b");
}

/*
#[wasm_bindgen_test]
fn multiline_builtin_annotations() {
    test("@Builtin_Type\ntype Date");
}


// === SKIP and FREEZE ===

#[wasm_bindgen_test]
fn freeze() {
    test("FREEZE x");
    test("FREEZE x + y");
    test("FREEZE x.f");
    test("FREEZE x.f y");
}

#[wasm_bindgen_test]
fn skip() {
    test("SKIP x");
    test("SKIP x + y");
    test("SKIP x.f");
    test("SKIP x.f y");
}
 */
