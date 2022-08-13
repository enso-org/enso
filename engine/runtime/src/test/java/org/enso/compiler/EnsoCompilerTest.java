package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
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
        type Ahoj
        type Ciao
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
  public void testFactorial() throws Exception {
    parseTest("""
    fac n = if n == 1 then 1 else n * fac n-1
    """);
  }

  @Test
  public void testComment() throws Exception {
    parseTest("""
    ## A type representing computations that may fail.
    type Maybe
    """);
  }

  @Test
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

  @SuppressWarnings("unchecked")
  private void parseTest(String code) throws UnsupportedSyntaxException {
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
    assertEquals("IR for " + code + " shall be equal", old, now);
  }
}
