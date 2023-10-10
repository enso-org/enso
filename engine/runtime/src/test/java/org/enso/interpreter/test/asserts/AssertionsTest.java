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
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class AssertionsTest extends TestBase {
  private static Context ctx;

  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();


  @BeforeClass
  public static void setupCtx() {
    ctx =
        TestBase.defaultContextBuilder(LanguageInfo.ID)
            .environment("ENSO_ENABLE_ASSERTIONS", "true")
            .out(out)
            .err(out)
            .build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close(true);
  }

  @After
  public void resetOutput() {
    out.reset();
  }

  @Test
  public void assertionsAreEnabled() {
    EnsoContext ensoCtx = ctx.getBindings(LanguageInfo.ID).invokeMember(MethodNames.TopScope.LEAK_CONTEXT).asHostObject();
    assertTrue(ensoCtx.isAssertionsEnabled());
  }

  @Test
  public void simpleAssertionFailureWithMessage() {
    try {
      TestBase.evalModule(ctx, """
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
      TestBase.evalModule(ctx, """
      from Standard.Base import False, Runtime
      main = Runtime.assert False 'My fail message'
      """);
      fail("Should throw Assertion_Error");
    } catch (PolyglotException e) {
      assertThat(e.getMessage(),
          allOf(
              containsString("Assertion Error"),
              containsString("My fail message")
          )
      );
    }
  }

  @Test
  public void assertionFailureDisplaysStackTrace() {
    try {
      TestBase.evalModule(ctx, """
      from Standard.Base import False, Runtime
      foo = Runtime.assert False 'My fail message'
      main = foo
      """);
      fail("Should throw Assertion_Error");
    } catch (PolyglotException e) {
      assertThat(e.getStackTrace().length, greaterThan(3));
      assertThat(e.getStackTrace()[0].toString(), containsString("Runtime.assert_builtin"));
      assertThat(e.getStackTrace()[1].toString(), containsString("Runtime.assert"));
      assertThat(e.getStackTrace()[2].toString(), containsString("foo"));
      assertThat(e.getStackTrace()[3].toString(), containsString("main"));
    }
  }

  @Test
  public void assertionSuccessReturnsNothing() {
    Value res = TestBase.evalModule(ctx, """
      from Standard.Base import Runtime, True
      main = Runtime.assert True
      """);
    assertTrue(res.isNull());
  }

  @Test
  public void assertChecksTypeOfReturnValue() {
    try {
      TestBase.evalModule(ctx, """
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
    Value res = TestBase.evalModule(ctx, """
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
