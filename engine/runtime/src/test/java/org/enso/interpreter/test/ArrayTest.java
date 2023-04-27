package org.enso.interpreter.test;

import java.net.URI;
import java.util.Map;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ArrayTest extends TestBase {
  private Context ctx;

  @Before
  public void prepareCtx() {
    this.ctx = createDefaultContext();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @After
  public void disposeCtx() {
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
