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
