package org.enso.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.common.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.junit.ClassRule;
import org.junit.Test;

public class WarningsAsErrorsTest {
  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext(
              ctxBldr -> ctxBldr.option(RuntimeOptions.TREAT_WARNINGS_AS_ERRORS, "true"))
          .build();

  @Test
  public void warningCausesCompilerFailure() {
    try {
      var module =
          ctxRule.eval(
              "enso",
              """
              foo =
                  unused = 1
                  2
              """);
      fail("Expecting an error but got: " + module);
    } catch (PolyglotException ex) {
      assertTrue("Syntax error", ex.isSyntaxError());
      assertTrue("Guest exception", ex.isGuestException());
      assertEquals(
          """
          Unnamed:2:5: warning: Unused variable unused.
              2 |     unused = 1
                |     ^~~~~~\
          """,
          ex.getMessage());
    }
  }
}
