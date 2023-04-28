package org.enso.interpreter.test;

import java.net.URI;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import static org.junit.Assert.assertTrue;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class ArrayTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Test
  public void dontExposeNullsOutOfArray() throws Exception {
    final URI facUri = new URI("memory://choose.enso");
    final Source facSrc = Source.newBuilder("enso", """
    from Standard.Base import all

    null =
        a = Array.new 1
        b = a.at 0
        b
    """, "nulls.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(facSrc);
    var res = module.invokeMember("eval_expression", "null");
    assertTrue("i null", res.isNull());
  }
}
