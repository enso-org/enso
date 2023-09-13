package org.enso.compiler.core;

import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.function.Function;
import org.enso.compiler.core.ir.Module;
import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.BeforeClass;
import org.junit.Test;

public class EnsoParserTest {
  private static EnsoParser ensoCompiler;

  @BeforeClass
  public static void initEnsoParser() {
    ensoCompiler = new EnsoParser();
  }

  @AfterClass
  public static void closeEnsoParser() throws Exception {
    if (ensoCompiler != null)
      ensoCompiler.close();
  }

  @Test
  public void testParseMain7Foo() throws Exception {
    parseTest("""
    main = 7.foo
    """);
  }

  @Test
  public void testLocationsSimpleArithmeticExpression() throws Exception {
    parseTest("""
    main = 2 + 45 * 20
    """, true, true, true);
  }

  @Test
  public void testLocationsApplicationsAndMethodCalls() throws Exception {
    parseTest("""
    main = (2-2 == 0).if_then_else (List.Cons 5 6) 0
    """, true, true, true);
  }

  @Test
  public void testLocationsCorrectAssignmentOfVariableReads() throws Exception {
    parseTest("""
    main =
        x = 2 + 2 * 2
        y = x * x
        IO.println y
    """, true, true, true);
  }

  @Test
  public void testLocationsMethodWithComplexBody() throws Exception {
    parseTest("""
    foo a b =
        x : Number
        x = a + 1
        y = b - 2
        x * y
    """, true, true, true);
  }

  @Test
  public void testLocationsBuildFunctionSimple() throws Exception {
    parseTest("""
    main =
        foo a = a + 1
        foo 42
    """, true, true, true);
  }

  @Test
  public void testListWithATrailingComma() throws Exception {
    parseTest("""
    main = ["a", ]
    """, true, true, true);
  }

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
    from Standard.Base import all hiding Number, Boolean, Float, Any
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
  public void testEmptyGroup2() throws Exception {
    parseTest("""
    main =
        x = ()
        x = 5
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
        build : Matrix Any Float
    """);
  }

  @Test
  public void testVectorVectorAny() throws Exception {
    parseTest("""
    type Vector
        build : Standard.Base.Vector.Matrix Standard.Base.Any Standard.Base.Float
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

  @Test
  public void testDotPrecedence() throws Exception {
    equivalenceTest("x = -1.up_to 100", "x = (-1).up_to 100");
  }

  @Test
  public void testFreeze() throws Exception {
    equivalenceTest("a = x", "a = FREEZE x");
    equivalenceTest("a = x+1", "a = FREEZE x+1");
    equivalenceTest("a = x + 1", "a = FREEZE x + 1");
    equivalenceTest("a = x.f 1", "a = FREEZE x.f 1");
  }

  @Test
  public void testSkip() throws Exception {
    equivalenceTest("a = x", "a = SKIP x");
    equivalenceTest("a = x", "a = SKIP x+1");
    equivalenceTest("a = x", "a = SKIP x + 1");
    equivalenceTest("a = x", "a = SKIP x");
    equivalenceTest("a = x", "a = SKIP x+y");
    equivalenceTest("a = x", "a = SKIP x + y");
    equivalenceTest("a = x", "a = SKIP x.f y");
    equivalenceTest("a = x", "a = SKIP Std.foo x");
    equivalenceTest("a = x", "a = SKIP Std.foo x.f");
    equivalenceTest("a = (Std.bar x)", "a = SKIP Std.foo (Std.bar x)");
    equivalenceTest("a = x", "a = SKIP FREEZE x");
    equivalenceTest("a = x", "a = SKIP FREEZE x + y");
    equivalenceTest("a = x", "a = SKIP FREEZE x.f");
    equivalenceTest("a = x", "a = SKIP FREEZE x.f y");
  }

  @Test
  public void ise_184219679() throws IOException {
    parseTest("""
    from Standard.Base import all

    main =
        x = 42
        y = if x == 42 then 10 else
        20
        IO.println y
    """);
  }

  private static void parseTest(String code) throws IOException {
      parseTest(code, true, true, true);
  }

  @SuppressWarnings("unchecked")
  private static void parseTest(String code, boolean noIds, boolean noLocations, boolean lessDocs) throws IOException {
    var ir = compile(code);
    assertNotNull(ir);
  }

  private static void equivalenceTest(String code1, String code2) throws IOException {
    var old = compile(code1);
    var now = compile(code2);
    var msg = "IR for " + code1 + " shall be equal to IR for " + code2;
    assertIR(msg, old, now);
  }

  private static Module compile(String code) {
    return compile(ensoCompiler, code);
  }

  public static Module compile(EnsoParser c, String code) {
    var src = Source.newBuilder("enso", code, "test-" + Integer.toHexString(code.hashCode()) + ".enso").build();
    var ir = c.compile(src.getCharacters());
    assertNotNull("IR was generated", ir);
    return ir;
  }

  static void assertIR(String msg, Module old, Module now) throws IOException {
    Function<IR, String> filter = f -> simplifyIR(f, true, true, false);
    String ir1 = filter.apply(old);
    String ir2 = filter.apply(now);
    if (!ir1.equals(ir2)) {
      String name = findTestMethodName();
      var home = new File(System.getProperty("java.io.tmpdir")).toPath();
      var file1 = home.resolve(name + ".1");
      var file2 = home.resolve(name + ".2");
      Files.writeString(file1, ir1, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
      Files.writeString(file2, ir2, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
      assertEquals(msg, file1, file2);
    }
  }

  private static String findTestMethodName() {
    for (var e : new Exception().getStackTrace()) {
      if (e.getMethodName().startsWith("test")) {
        return e.getMethodName();
      }
    }
    throw new IllegalStateException();
  }

  /** Takes an {@link IR} and converts it to text representation suitable for
   * "diffing" while "simplifying" it.
   *
   * @param ir the intermediate representation
   * @param noIds remove all UUIDs or keep them? Multiple runs usually assign
   *   random/different UUIDs to various IR elements. Removing them is a best
   *   way to make the converted text comparable
   * @param noLocations locations may slightly differ. Usually off-by-one.
   *   Especially when running old and new parser in parallel - removing them
   *   may be useful
   * @param lessDocs documentation often isn't an essential part of the IR
   *   one can easily remove it by specifying {@code false}
   * @return string representation of the IR
   */
  private static String simplifyIR(IR ir, boolean noIds, boolean noLocations, boolean lessDocs) {
    String txt = ir.pretty();
    if (noIds) {
      txt = txt.replaceAll("[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]", "_");
    }
    if (noLocations) {
      for (;;) {
        final String pref = " Location(";
        int at = txt.indexOf(pref);
        if (at == -1) {
          break;
        }
        int to = at + pref.length();
        int depth = 1;
        while (depth > 0) {
          switch (txt.charAt(to)) {
            case '(' -> depth++;
            case ')' -> depth--;
          }
          to++;
        }
        txt = txt.substring(0, at) + "Location[_]" + txt.substring(to);
      }
    }
    if (lessDocs) {
      for (;;) {
        final String pref = "Comment.Documentation(";
        int at = txt.indexOf(pref);
        if (at == -1) {
          break;
        }
        int to = txt.indexOf("location =", at + pref.length());
        txt = txt.substring(0, at) + "Comment.Doc(" + txt.substring(to);
      }
      for (;;) {
        final String pref = "Case.Pattern.Doc(";
        int at = txt.indexOf(pref);
        if (at == -1) {
          break;
        }
        int to = txt.indexOf("location =", at + pref.length());
        txt = txt.substring(0, at) + "Comment.CaseDoc(" + txt.substring(to);
      }
    }
    for (;;) {
      final String pref = "errors.Syntax(";
      int at = txt.indexOf(pref);
      if (at == -1) {
        break;
      }
      int to = txt.indexOf("reason =", at + pref.length());
      txt = txt.substring(0, at) + "errors.Syntax (" + txt.substring(to);
    }
    return txt;
  }

}
