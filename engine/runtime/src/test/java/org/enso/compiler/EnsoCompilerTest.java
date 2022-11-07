package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.function.Function;
import org.enso.compiler.codegen.AstToIr;
import org.enso.compiler.core.IR;
import org.enso.syntax.text.AST.ASTOf;
import org.enso.syntax.text.Parser;
import org.enso.syntax.text.Shape;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Ignore;

public class EnsoCompilerTest {
  private static EnsoCompiler ensoCompiler;

  @BeforeClass
  public static void initEnsoCompiler() {
    ensoCompiler = new EnsoCompiler();
  }

  @AfterClass
  public static void closeEnsoCompiler() throws Exception {
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
    """, true, false, true);
  }

  @Test
  public void testLocationsApplicationsAndMethodCalls() throws Exception {
    parseTest("""
    main = (2-2 == 0).if_then_else (Cons 5 6) 0
    """, true, false, true);
  }

  @Test
  public void testLocationsCorrectAssignmentOfVariableReads() throws Exception {
    parseTest("""
    main =
        x = 2 + 2 * 2
        y = x * x
        IO.println y
    """, true, false, true);
  }

  @Test
  public void testLocationsMethodWithComplexBody() throws Exception {
    parseTest("""
    foo a b =
        x : Number
        x = a + 1
        y = b - 2
        x * y
    """, true, false, true);
  }

  @Test
  public void testLocationsBuildFunctionSimple() throws Exception {
    parseTest("""
    main =
        foo a = a + 1
        foo 42
    """, true, false, true);
  }

  @Test
  public void testLocationsDeeplyNestedFunctions() throws Exception {
    parseTest("""
        foo = a -> b ->
            IO.println a
        """, true, false, true);
  }

  @Test
  public void testLocationsDeeplyNestedFunctionsNoBlock() throws Exception {
    parseTest("""
    Nothing.method =
        add = a -> b -> a + b

    main = Nothing.method
    """, true, false, true);
  }

  @Test
  @Ignore
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
    from Standard.Base.Data.Any import all
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
  @Ignore
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
  @Ignore
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
  @Ignore
  public void testVariousKindsOfUnicodeWhitespace() throws Exception {
    // mimics Text_Spec.enso:1049
    parseTest("""
    '\\v\\f\\u{200a}\\u{202f}\\u{205F}\\u{3000}\\u{feff}'.trim
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
        x = Panic.catch_primitive () .convert_to_dataflow_error
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
    reverse_list list = Nil
    """);
  }

  @Test
  public void testReverseList() throws Exception {
    parseTest("""
    reverse_list list =
        go = list -> acc -> case list of
            Cons h t -> go t (Cons h acc)
            Cons h _ -> acc
            Nil -> acc
            _ -> acc
        res = go list Nil
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
            Platform.Windows -> txt
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
  @Ignore // Old parser's representation of this is inconsistent with normal treatment of names.
  public void testConstructorMultipleNamedArgs2() throws Exception {
    parseTest("""
    x = (Regex_Matcher.Regex_Matcher_Data case_sensitivity=Case_Sensitivity.Sensitive) dot_matches_newline=True
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
  @Ignore
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
  @Ignore
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

  static String simplifyIR(IR i, boolean noIds, boolean noLocations, boolean lessDocs) {
    var txt = i.pretty();
    if (noIds) {
      txt = txt.replaceAll("id = [0-9a-f\\-]*", "id = _");
    }
    if (noLocations) {
        for (;;) {
          final String pref = "IdentifiedLocation(";
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
          txt = txt.substring(0, at) + "IdentifiedLocation[_]" + txt.substring(to);
        }
    }
    if (lessDocs) {
        for (;;) {
          final String pref = "IR.Comment.Documentation(";
          int at = txt.indexOf(pref);
          if (at == -1) {
            break;
          }
          int to = txt.indexOf("location =", at + pref.length());
          txt = txt.substring(0, at) + "IR.Comment.Doc(" + txt.substring(to);
        }
        for (;;) {
          final String pref = "IR.Case.Pattern.Doc(";
          int at = txt.indexOf(pref);
          if (at == -1) {
            break;
          }
          int to = txt.indexOf("location =", at + pref.length());
          txt = txt.substring(0, at) + "IR.Comment.CaseDoc(" + txt.substring(to);
        }
    }
    for (;;) {
      final String pref = "IR.Error.Syntax(";
      int at = txt.indexOf(pref);
      if (at == -1) {
        break;
      }
      int to = txt.indexOf("reason =", at + pref.length());
      txt = txt.substring(0, at) + "IR.Error.Syntax (" + txt.substring(to);
    }
    return txt;
  }

  private static void parseTest(String code) throws IOException {
      parseTest(code, true, true, true);
  }

  @SuppressWarnings("unchecked")
  private static void parseTest(String code, boolean noIds, boolean noLocations, boolean lessDocs) throws IOException {
    var src = Source.newBuilder("enso", code, "test-" + Integer.toHexString(code.hashCode()) + ".enso").build();
    var ir = ensoCompiler.compile(src);
    assertNotNull("IR was generated", ir);

    var oldAst = new Parser().runWithIds(src.getCharacters().toString());
    var oldIr = AstToIr.translate((ASTOf<Shape>)(Object)oldAst);

    Function<IR, String> filter = (f) -> simplifyIR(f, noIds, noLocations, lessDocs);

    var old = filter.apply(oldIr);
    var now = filter.apply(ir);
    if (!old.equals(now)) {
      var name = findTestMethodName();
      var home = new File(System.getProperty("user.home")).toPath();
      Files.writeString(home.resolve(name + ".old") , old, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
      Files.writeString(home.resolve(name + ".now") , now, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
      assertEquals("IR for " + code + " shall be equal", old, now);
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
}
