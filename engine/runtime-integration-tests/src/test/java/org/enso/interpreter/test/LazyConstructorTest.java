package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class LazyConstructorTest extends TestBase {
  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();
  private static Context ctx;

  public LazyConstructorTest() {}

  @BeforeClass
  public static void prepareCtx() {
    ctx = createDefaultContext(out);
  }

  @Before
  public void resetOut() {
    out.reset();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Test
  public void lazyConstructorWithNoArgument() {
    try {
      var create =
          ctx.eval(
                  "enso",
                  """
              type N
                  False

                  materialize v:N = v.to_text

              create n = N.materialize (~False)
              """)
              .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
      assertTrue("Can evaluate", create.canExecute());
      assertEquals("False", create.execute(42).asString());

    } catch (PolyglotException e) {
      fail(e.getMessage() + " for \n" + out.toString());
    }
  }

  @Test
  public void lazyConstructorWithSingleArg() {
    try {
      var create =
          ctx.eval(
                  "enso",
                  """
              type M
                  Construct value

                  materialize v:M = v.value

              create n = M.materialize (~Construct n)
              """)
              .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
      assertTrue("Can evaluate", create.canExecute());
      assertEquals("42", create.execute(42).toString());

    } catch (PolyglotException e) {
      fail(e.getMessage() + " for \n" + out.toString());
    }
  }

  @Test
  public void wrongConstructorNameYieldsTypeError() {
    try {
      var create =
          ctx.eval(
                  "enso",
                  """
              type N
                  False

                  materialize v:N = v.to_text

              create n = N.materialize (~True)
              """)
              .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
      assertTrue("Can evaluate", create.canExecute());
      var r = create.execute(42);
      fail("Expecting an exception, not " + r);
    } catch (PolyglotException e) {
      assertTrue(
          "Expecting type error, but got: " + e.getMessage(),
          e.getMessage().contains("Type_Error"));
    }
  }
}
