package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import org.enso.common.MethodNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class AutoscopedConstructorTest {
  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();
  private static Context ctx;

  public AutoscopedConstructorTest() {}

  @BeforeClass
  public static void prepareCtx() {
    ctx = ContextUtils.createDefaultContext(out);
  }

  @After
  public void resetOut() {
    out.reset();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
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

                  create n = N.materialize (..False)
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

                  create n = M.materialize (..Construct n)
                  """)
              .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
      assertTrue("Can evaluate", create.canExecute());
      assertEquals("42", create.execute(42).toString());

    } catch (PolyglotException e) {
      fail(e.getMessage() + " for \n" + out.toString());
    }
  }

  @Test
  public void lazyConstructorWithTwoArgs() {
    try {
      var create =
          ctx.eval(
                  "enso",
                  """
                  type M
                      Construct v1 v2

                      materialize v:M = [v.v1, v.v2]

                  create a b = M.materialize (..Construct a b)
                  """)
              .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
      assertTrue("Can evaluate", create.canExecute());
      assertEquals("[6, 7]", create.execute(6, 7).toString());
    } catch (PolyglotException e) {
      fail(e.getMessage() + " for \n" + out.toString());
    }
  }

  @Test
  public void lazyConstructorWithTwoArgsCurried() {
    try {
      var create =
          ctx.eval(
                  "enso",
                  """
                  type M
                      Construct v1 v2

                      materialize v:M = [v.v1, v.v2]

                  create a b =
                      v0 = ..Construct
                      v1 = v0 a
                      v2 = v1 b
                      M.materialize v2
                  """)
              .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
      assertTrue("Can evaluate", create.canExecute());
      assertEquals("[7, 6]", create.execute(7, 6).toString());
    } catch (PolyglotException e) {
      fail(e.getMessage() + " for \n" + out.toString());
    }
  }

  @Test
  public void lazyConstructorWithTwoArgsNamed() {
    try {
      var create =
          ctx.eval(
                  "enso",
                  """
                  type M
                      Construct v1 v2

                      materialize v:M = [v.v1, v.v2]

                  create a b =
                      v0 = ..Construct v2=a v1=b
                      M.materialize v0
                  """)
              .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
      assertTrue("Can evaluate", create.canExecute());
      assertEquals("[7, 6]", create.execute(6, 7).toString());
    } catch (PolyglotException e) {
      fail(e.getMessage() + " for \n" + out.toString());
    }
  }

  @Test
  public void lazyConstructorWithNamedDefaultedArguments() {
    try {
      var module =
          ctx.eval(
              "enso",
              """
              type M
                  Construct v1=1 v2=2 v3=3 v4=4

                  materialize v:M = [v.v1, v.v2, v.v3, v.v4]

              c0 _ = M.materialize (..Construct)
              c1 a = M.materialize (..Construct a)
              c12 a b = M.materialize (..Construct a b)
              c123 a b c = M.materialize (..Construct a b c)
              c1234 a b c d = M.materialize (..Construct a b c d)
              c14 a d = M.materialize (..Construct a v4=d)
              c13 a c = M.materialize (..Construct a v3=c)
              c41 a d = M.materialize ((..Construct v4=d) a)
              c31 a c = M.materialize ((..Construct v3=c) a)
              """);

      var c0 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "c0");
      var c1 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "c1");
      var c12 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "c12");
      var c123 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "c123");
      var c1234 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "c1234");
      var c14 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "c14");
      var c13 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "c13");
      var c41 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "c41");
      var c31 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "c31");

      assertEquals("[1, 2, 3, 4]", c0.execute("ignored").toString());
      assertEquals("[9, 2, 3, 4]", c1.execute(9).toString());
      assertEquals("[9, 7, 3, 4]", c12.execute(9, 7).toString());
      assertEquals("[9, 7, 5, 4]", c123.execute(9, 7, 5).toString());
      assertEquals("[9, 7, 5, 3]", c1234.execute(9, 7, 5, 3).toString());
      assertEquals("[8, 2, 3, 7]", c14.execute(8, 7).toString());
      assertEquals("[8, 2, 7, 4]", c13.execute(8, 7).toString());
      assertEquals("[8, 2, 3, 7]", c41.execute(8, 7).toString());
      assertEquals("[8, 2, 7, 4]", c31.execute(8, 7).toString());
    } catch (PolyglotException e) {
      fail(e.getMessage() + " for \n" + out.toString());
    }
  }

  @Test
  public void tooManyConstructors() {
    final var count = 25;
    var sb = new StringBuilder();
    sb.append("type Too\n");
    for (var i = 0; i < count; i++) {
      sb.append("    C").append(i);
      for (var j = 0; j < i; j++) {
        sb.append(" x").append(j);
      }
      sb.append("\n");
    }
    sb.append("    materialize v:Too = v\n");

    sb.append("create n =\n");
    sb.append("    v = case n of\n");
    for (var i = 0; i < count; i++) {
      sb.append("        ").append(i).append(" -> ..C").append(i);
      for (var j = 0; j < i; j++) {
        sb.append(" ").append(j % 10);
      }
      sb.append("\n");
    }
    sb.append("    Too.materialize v\n");

    try {
      var create =
          ctx.eval("enso", sb.toString())
              .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
      assertTrue("Can evaluate", create.canExecute());
      for (var i = 1; i < count; i++) {
        var r = create.execute(i);
        var meta = r.getMetaObject();
        assertNotNull("At " + i + " meta object for " + r + " found", meta);
        var n = meta.getMetaSimpleName();
        assertEquals("At " + i + " got result " + r, "Too", n);
      }
    } catch (RuntimeException | Error err) {
      throw new AssertionError(sb.toString(), err);
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

                  create n = N.materialize (..True)
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

  @Test
  public void simpleAnyCheck() {
    var code =
        """
    import Standard.Base.Any.Any

    type A
        Typed x:Any

    t = ..Typed ..My_Other
    materialize v:A = v

    create = materialize t
    """;

    var create = ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");

    assertEquals("A", create.getMetaObject().getMetaSimpleName());
  }

  @Test
  public void simpleAnyOrACheck() {
    var code =
        """
    import Standard.Base.Any.Any

    type A
        Typed (x:Any|A)

    t = ..Typed ..My_Other
    materialize v:A = v

    create = materialize t
    """;

    var create = ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");

    assertEquals("A", create.getMetaObject().getMetaSimpleName());
  }

  @Test
  public void intersectionAnyOrACheck() {
    var code =
        """
    import Standard.Base.Any.Any

    type A
        Typed (x:Any&A)

    t = ..Typed ..My_Other
    materialize v:A = v

    create = materialize t
    """;

    try {
      var create =
          ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
      fail("Got value, but expecting an exception: " + create);
    } catch (PolyglotException ex) {
      assertThat(ex.getMessage(), containsString("Cannot find constructor ..My_Other among A."));
    }
  }
}
