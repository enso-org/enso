package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.function.Function;
import org.enso.compiler.codegen.AstToIr;
import org.enso.compiler.core.IR;
import org.enso.syntax.text.AST.ASTOf;
import org.enso.syntax.text.Parser;
import org.enso.syntax.text.Shape;
import org.enso.syntax2.UnsupportedSyntaxException;
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
  @Ignore
  public void testCase() throws Exception {
    parseTest("""
    type Msg
        Ahoj
        Ciao
        """

//    c x = case x of
//        Ahoj -> 0
//        Ciao -> 1
//    """);
    );
  }

  @Test
  public void testImport() throws Exception {
    parseTest("""
    from Standard.Base.Data.Any import all
    import project.IO
    import Standard.Base as Enso_List
    from Standard.Base import all hiding Number, Boolean
    from Standard.Table as Column_Module import Column
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
  @Ignore
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
  @Ignore
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
  @Ignore
  public void testSignature3() throws Exception {
    parseTest("val = 123 : Int");
  }

  @Test
  @Ignore
  public void testSignature4() throws Exception {
    parseTest("val = foo (123 : Int)");
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
  public void testExportFromAsExport() throws Exception {
    parseTest("from prj.Data.Foo as Bar export Baz, Quux");
  }

  @SuppressWarnings("unchecked")
  private void parseTest(String code) throws UnsupportedSyntaxException, IOException {
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
