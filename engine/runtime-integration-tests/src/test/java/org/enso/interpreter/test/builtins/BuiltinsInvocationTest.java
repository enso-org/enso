package org.enso.interpreter.test.builtins;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.junit.ClassRule;
import org.junit.Test;

public class BuiltinsInvocationTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void invokeBuiltinWithWrongArguments_ShouldNotCrash() {
    var src =
        """
        from Standard.Base import all

        main =
            (Error.catch_primitive self=(y->y)) (x->x)
        """;
    try {
      ctxRule.evalModule(src);
    } catch (PolyglotException e) {
      var panic = e.getGuestObject();
      assertThat("Should be panic", panic.isException());
      assertThat(
          "Should have Not_Invokable error as payload",
          e.getMessage(),
          containsString("Type error"));
    }
  }
}
