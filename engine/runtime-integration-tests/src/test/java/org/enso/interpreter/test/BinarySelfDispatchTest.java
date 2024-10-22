package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.core.AllOf.allOf;
import static org.junit.Assert.fail;

import org.enso.common.MethodNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class BinarySelfDispatchTest {
  private static Context ctx;
  private static Value module;

  @BeforeClass
  public static void initCtx() throws Exception {
    ctx = ContextUtils.createDefaultContext();

    var prelude =
        Source.newBuilder(
                "enso",
                """
                from Standard.Base import all

                type Delay
                    private Days n
                type Datum
                    private On at delta

                    + self (delta : Delay) -> Datum = Datum.On self.n delta.n

                today_plus n =
                  delta = Delay.Days n
                  now = Datum.On 0 delta
                  now + now
                """,
                "error.enso")
            .build();
    module = ctx.eval(prelude);
  }

  @AfterClass
  public static void closeCtx() {
    module = null;
    ctx.close();
    ctx = null;
  }

  @Test
  public void avoidSelfConversion() {
    try {
      var error = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "today_plus 3");
      fail("The code should yield a panic: " + error);
    } catch (PolyglotException ex) {
      assertThat(
          "The error message is correct",
          ex.getMessage(),
          allOf(
              containsString("expected `delta`"),
              containsString("to be Delay"),
              containsString("but got Datum")));
    }
  }
}
