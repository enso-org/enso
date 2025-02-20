package org.enso.interpreter.test.asserts;

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.stringContainsInOrder;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.number.OrderingComparison.greaterThan;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.util.List;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class AssertionsTest {

  private static Context ctx;

  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @BeforeClass
  public static void setupCtx() {
    ctx =
        ContextUtils.defaultContextBuilder(LanguageInfo.ID)
            .environment("ENSO_ENABLE_ASSERTIONS", "true")
            .out(out)
            .err(out)
            .build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close(true);
    ctx = null;
  }

  @After
  public void resetOutput() {
    out.reset();
  }

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
    EnsoContext ensoCtx =
        ctx.getBindings(LanguageInfo.ID)
            .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
            .asHostObject();
    assertTrue(ensoCtx.isAssertionsEnabled());
  }

  @Test
  public void simpleAssertionFailureWithMessage() {
    try {
      ContextUtils.evalModule(
          ctx,
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
      ContextUtils.evalModule(
          ctx,
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
      ContextUtils.evalModule(
          ctx,
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
        ContextUtils.evalModule(
            ctx,
            """
                from Standard.Base import Runtime, True
                main = Runtime.assert True
                """);
    assertTrue(res.isNull());
  }

  @Test
  public void assertChecksTypeOfReturnValue() {
    try {
      ContextUtils.evalModule(
          ctx,
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
        ContextUtils.evalModule(
            ctx,
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
