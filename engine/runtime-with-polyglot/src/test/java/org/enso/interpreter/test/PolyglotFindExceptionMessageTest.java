package org.enso.interpreter.test;

import org.enso.polyglot.HostEnsoUtils;
import static org.junit.Assert.assertEquals;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import static org.junit.Assert.fail;
import org.junit.BeforeClass;
import org.junit.Test;

public class PolyglotFindExceptionMessageTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void initCtx() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Test
  public void testJavaScriptException() {
    String src = """
    main = err

    foreign js err = \"""
        throw Error('Wrong!')
    """;

    try {
      Value res = evalModule(ctx, src);
      fail("No result expected: " + res);
    } catch (PolyglotException ex) {
      assertExceptionMessage("Error: Wrong!", ex);
    }
  }

  private void assertExceptionMessage(String exp, PolyglotException ex) {
    var msg = HostEnsoUtils.findExceptionMessage(ex);
    assertEquals(exp, msg);
  }
}
