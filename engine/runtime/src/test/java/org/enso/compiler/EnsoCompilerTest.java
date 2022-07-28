package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import java.util.function.Function;
import org.enso.compiler.codegen.AstToIr;
import org.enso.compiler.core.IR;
import org.enso.compiler.data.CompilerConfig;
import org.enso.interpreter.runtime.Context;
import org.enso.syntax.text.AST;
import org.enso.syntax.text.AST.ASTOf;
import org.enso.syntax.text.Parser;
import org.enso.syntax.text.Shape;
import org.enso.syntax2.Tree;
import org.enso.syntax2.UnsupportedSyntaxException;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class EnsoCompilerTest {
  private static EnsoCompiler ensoCompiler;
  private static Compiler oldCompiler;

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

  private void parseTest(String code) throws UnsupportedSyntaxException {
    var src = Source.newBuilder("enso", code, "test-" + Integer.toHexString(code.hashCode()) + ".enso").build();
    var ir = ensoCompiler.compile(src);
    assertNotNull("IR was generated", ir);

    var oldAst = new Parser().runWithIds(src.getCharacters().toString());
    var oldIr = AstToIr.translate((ASTOf<Shape>)(Object)oldAst);

    Function<IR, String> filter = (i) -> i.pretty().replaceAll("id = [0-9a-f\\-]*", "id = _");

    var old = filter.apply(oldIr);
    var now = filter.apply(ir);
    assertEquals("IR for " + code + " shall be equal", old, now);
  }
}
