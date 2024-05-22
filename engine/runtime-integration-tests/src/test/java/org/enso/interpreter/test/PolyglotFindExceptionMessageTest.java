package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.enso.common.HostEnsoUtils;
import org.enso.test.utils.TestUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class PolyglotFindExceptionMessageTest {
  private static Context ctx;

  @BeforeClass
  public static void initCtx() {
    ctx = TestUtils.createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Test
  public void testJavaScriptException() {
    String src =
        """
    main = err

    foreign js err = \"""
        throw Error('Wrong!')
    """;

    try {
      Value res = TestUtils.evalModule(ctx, src);
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
