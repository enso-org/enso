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
    from Standard.Base import all hiding Number, Boolean
    polyglot java import java.lang.Float
    polyglot java import java.net.URI as Java_URI

    main = 3
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
  public void testComments() throws Exception {
    parseTest("""
    # a b c
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
  public void testSelf1() throws Exception {
    parseTest("""
    contains self elem = self.contains Nothing
    """);
  }

  @Test
  @Ignore
  public void testMetadataRaw() throws Exception {
    parseTest("""
    main = 4
    #### METADATA ####
    [[{"index":{"value":7},"size":{"value":8}},"5bad897e-099b-4b00-9348-64092636746d"]]
    """);
  }

  @Test
  @Ignore // because of https://github.com/enso-org/enso/pull/3653#issuecomment-1221841342
  public void testDocumentationComment() throws Exception {
    parseTest("""
    ## A type representing computations that may fail.
    type Maybe
    """);
  }

  @Test
  @Ignore
  public void testColumnSelector() throws Exception {
    parseTest("""
    type Column_Selector

        ## Selects columns based on their names.

           The `matcher` can be used to specify if the names should be matched
           exactly or should be treated as regular expressions. It also allows to
           specify if the matching should be case-sensitive.
        type By_Name (names : Vector Text) (matcher : Matcher = Text_Matcher)

        ## Selects columns by their index.

           The index of the first column in the table is 0. If the provided index is
           negative, it counts from the end of the table (e.g. -1 refers to the last
           column in the table).
        type By_Index (indexes : Vector Integer)

        ## Selects columns having exactly the same names as the columns provided in
           the input.

           The input columns do not necessarily have to come from the same table, so
           this approach can be used to match columns with the same names as a set
           of columns of some other table, for example, when preparing for a join.
        type By_Column (columns : Vector Column)
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
  public void testTypeSignature() throws Exception {
    parseTest("""
    resolve_aggregate table problem_builder aggregate_column =
        table_columns = table.columns

        resolve : (Integer|Text|Column) -> Column ! Internal_Missing_Column_Error
        resolve c = 42
    """);
  }

  @Test
  public void testTypeSignatureQualified() throws Exception {
    parseTest("""
    type Baz
        resolve : Integer -> Column
    """);
  }

  @Test
  public void testMethodDefQualified() throws Exception {
    parseTest("""
    type Foo
        id x = x
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

  @SuppressWarnings("unchecked")
  private void parseTest(String code) throws IOException {
    var src = Source.newBuilder("enso", code, "test-" + Integer.toHexString(code.hashCode()) + ".enso").build();
    var ir = ensoCompiler.compile(src);
    assertNotNull("IR was generated", ir);

    var oldAst = new Parser().runWithIds(src.getCharacters().toString());
    var oldIr = AstToIr.translate((ASTOf<Shape>)(Object)oldAst);

    Function<IR, String> filter = (i) -> {
      var txt = i.pretty().replaceAll("id = [0-9a-f\\-]*", "id = _");
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
      return txt;
    };

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
