package org.enso.interpreter.test.asserts;

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.stringContainsInOrder;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.number.OrderingComparison.greaterThan;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

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
      assertThat(e.getStackTrace().length, greaterThan(5));
      assertThat(e.getStackTrace()[0].toString(), containsString("Panic"));
      assertThat(e.getStackTrace()[1].toString(), containsString("Runtime.assert"));
      // Ignore the next two frames as they are implementation details
      assertThat(e.getStackTrace()[4].toString(), containsString("foo"));
      assertThat(e.getStackTrace()[5].toString(), containsString("main"));
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
            import Standard.Base.Runtime.Ref.Ref

            main =
                ref = Ref.new 10
                Runtime.assert (ref.put 23 . is_nothing . not)
                ref.get
            """);
    assertTrue(res.isNumber());
    assertThat(res.asInt(), is(23));
  }
}
