package org.enso.interpreter.test.builtins;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class BuiltinsInvocationTest {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx = ContextUtils.createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }

  @Test
  public void invokeBuiltinWithWrongArguments_ShouldNotCrash() {
    var src =
        """
        from Standard.Base import all

        main =
            (Error.catch_primitive self=(y->y)) (x->x)
        """;
    try {
      ContextUtils.evalModule(ctx, src);
    } catch (PolyglotException e) {
      var panic = e.getGuestObject();
      assertThat("Should be panic", panic.isException());
      assertThat("Should have Type error as payload", e.getMessage(), containsString("Type error"));
    }
  }
}
