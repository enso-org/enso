package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.enso.common.HostEnsoUtils;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.ClassRule;
import org.junit.Test;

public class PolyglotFindExceptionMessageTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void testJavaScriptException() {
    String src =
        """
        main = err

        foreign js err = \"""
            throw Error('Wrong!')
        """;

    try {
      Value res = ctxRule.evalModule(src);
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
