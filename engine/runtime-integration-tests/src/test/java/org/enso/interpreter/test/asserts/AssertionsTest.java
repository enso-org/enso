package org.enso.interpreter.test.asserts;

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.stringContainsInOrder;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.List;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.ClassRule;
import org.junit.Test;

public class AssertionsTest {

  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext(b -> b.environment("ENSO_ENABLE_ASSERTIONS", "true"))
          .build();

  @Test
  public void jvmAssertionsAreEnabled() {
    boolean assertsOn = false;
    assert assertsOn = true;
    assertTrue(
        "JVM assertions must be enabled (with -ea cmd line option) in order to run all the tests"
            + " inside runtime-integration-tests project. Note that there are some features in the"
            + " runtime that work only with the JVM assertions enabled.",
        assertsOn);
  }

  @Test
  public void assertionsAreEnabled() {
    EnsoContext ensoCtx = ctxRule.ensoContext();
    assertTrue(ensoCtx.isAssertionsEnabled());
  }

  @Test
  public void simpleAssertionFailureWithMessage() {
    try {
      ctxRule.evalModule(
          """
          from Standard.Base import False, Runtime
          main = Runtime.assert False
          """);
      fail("Should throw Assertion_Error");
    } catch (PolyglotException e) {
      assertThat(e.getGuestObject().isException(), is(true));
    }
  }

  @Test
  public void assertionFailureDisplaysMessage() {
    try {
      ctxRule.evalModule(
          """
          from Standard.Base import False, Runtime
          main = Runtime.assert False 'My fail message'
          """);
      fail("Should throw Assertion_Error");
    } catch (PolyglotException e) {
      assertThat(
          e.getMessage(),
          allOf(containsString("Assertion Error"), containsString("My fail message")));
    }
  }

  @Test
  public void assertionFailureDisplaysStackTrace() {
    try {
      ctxRule.evalModule(
          """
          from Standard.Base import False, Runtime
          foo = Runtime.assert False 'My fail message'
          main = foo
          """);
      fail("Should throw Assertion_Error");
    } catch (PolyglotException e) {
      assertStack(e, "Panic", "Runtime.assert", "foo", "main");
    }
  }

  @Test
  public void assertionSuccessReturnsNothing() {
    Value res =
        ctxRule.evalModule(
            """
            from Standard.Base import Runtime, True
            main = Runtime.assert True
            """);
    assertTrue(res.isNull());
  }

  @Test
  public void assertChecksTypeOfReturnValue() {
    try {
      ctxRule.evalModule(
          """
          from Standard.Base import Runtime
          main = Runtime.assert [1,2,3]
          """);
      fail("Should throw Type_Error");
    } catch (PolyglotException e) {
      assertThat(e.getMessage(), stringContainsInOrder(List.of("Type", "error")));
    }
  }

  @Test
  public void actionInAssertIsComputedWhenAssertionsAreEnabled() {
    Value res =
        ctxRule.evalModule(
            """
            from Standard.Base import Runtime
            from Standard.Base.Nothing import all
            import Standard.Base.Runtime.Ref.Ref

            main =
                ref = Ref.new 10
                Runtime.assert (ref.put 23 . is_nothing . not)
                ref.get
            """);
    assertTrue(res.isNumber());
    assertThat(res.asInt(), is(23));
  }

  private static void assertStack(Throwable e, String... sampleWords) {
    var stack = new StringWriter();
    e.printStackTrace(new PrintWriter(stack));

    var lineNumber = 0;
    for (var i = 0; i < sampleWords.length; i++) {
      while (true) {
        if (e.getStackTrace().length <= lineNumber) {
          fail(
              "Cannot find "
                  + sampleWords[i]
                  + " (from "
                  + sampleWords
                  + ") in:\n"
                  + stack.toString());
        }
        var line = e.getStackTrace()[lineNumber++].toString();
        if (line.contains(sampleWords[i])) {
          // found another requested sample
          break;
        }
      }
    }
  }
}
