package org.enso.interpreter.test.semantic;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.enso.common.MethodNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class DataflowErrorPropagationTest {
  private static Context ctx;
  private static Value suppressError;
  private static Value suppressErrorWithAssign;

  @BeforeClass
  public static void prepareCtx() {
    ctx = ContextUtils.createDefaultContext();
    var code =
        """
    from Standard.Base import all

    private yield_error yes:Boolean -> Text =
        if yes then Error.throw "Yielding an error" else
            "OK"

    suppress_error yes:Boolean value =
        yield_error yes
        value

    suppress_error_with_assign yes:Boolean value =
        _ = yield_error yes
        value
    """;
    suppressError =
        ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "suppress_error");
    suppressErrorWithAssign =
        ctx.eval("enso", code)
            .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "suppress_error_with_assign");
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }

  @Test
  public void noErrorReturnValue() {
    var value = suppressError.execute(false, 42);
    assertTrue("It is a number", value.isNumber());
    assertEquals(42, value.asInt());
  }

  @Test
  public void propagateErrorImmediatelly() {
    var value = suppressError.execute(true, 42);
    assertFalse("It is not a number", value.isNumber());
    assertTrue("It is an error", value.isException());
    try {
      throw value.throwException();
    } catch (PolyglotException ex) {
      assertEquals("Yielding an error", ex.getMessage());
    }
  }

  @Test
  public void noErrorReturnValueWithAssign() {
    var value = suppressErrorWithAssign.execute(false, 42);
    assertTrue("It is a number", value.isNumber());
    assertEquals(42, value.asInt());
  }

  @Test
  public void errorIsAssignedAndThatIsEnoughReturnValue() {
    var value = suppressErrorWithAssign.execute(true, 42);
    assertTrue("It is a number", value.isNumber());
    assertFalse("Not an error", value.isException());
    assertEquals(42, value.asInt());
  }
}
