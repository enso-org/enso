package org.enso.interpreter.test;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.net.URI;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests that verify that if type inference issues a warning about an error, that error actually
 * occurs in the runtime.
 */
public class TypeInferenceConsistencyTest {
  private static Context ctx;
  private static final ByteArrayOutputStream output = new ByteArrayOutputStream();

  @BeforeClass
  public static void prepareCtx() {
    ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.ENABLE_STATIC_ANALYSIS, "true")
            .out(output)
            .err(output)
            .build();
  }

  @After
  public void cleanMessages() {
    output.reset();
  }

  private String getOutput() {
    return output.toString();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }

  @Test
  public void notInvokableTest() throws Exception {
    final URI uri = new URI("memory://notInvokableTest.enso");
    final Source src =
        Source.newBuilder("enso", """
    foo = 1 2
    """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var res = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertNotInvokableRuntimeError("1", e);
    }

    // But we also expect the compile warning:
    assertNotInvokableWarning("Integer", getOutput());
  }

  /**
   * Tests that a `Function` type is correctly inferred to be equivalent to `Any -> Any` and thus
   * does not produce a spurious warning.
   */
  @Test
  public void notInvokableFunctionNoWarning() throws Exception {
    final URI uri = new URI("memory://notInvokableFunctionNoWarning.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    import Standard.Base.Data.Numbers
                    import Standard.Base.Function.Function
                    foo (fun : Function)  =
                        f = fun
                        x1 = f 123
                        x1
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var result = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo (x -> x + 1)");
    assertEquals(124, result.asLong());

    assertEquals("No warning diagnostics are expected.", "", getOutput());
  }

  /** Tests that a sum type that _may_ be a function, does not emit a warning. */
  @Test
  public void notInvokableSumTypeNoWarning() throws Exception {
    final URI uri = new URI("memory://notInvokableSumTypeNoWarning.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    import Standard.Base.Data.Numbers
                    import Standard.Base.Data.Numbers.Integer
                    import Standard.Base.Function.Function
                    foo (fun : Function | Integer)  =
                        f = fun
                        x1 = f 123
                        x1
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var r1 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo (x -> x + 1)");
    assertEquals(124, r1.asLong());

    try {
      var r2 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo 123");
      fail("Expecting an exception, not: " + r2);
    } catch (PolyglotException e) {
      assertNotInvokableRuntimeError("123", e);
    }

    assertEquals("No warning diagnostics are expected.", "", getOutput());
  }

  /** Tests that a sum type that _may_ be a function, does not emit a warning. */
  @Test
  public void notInvokableAnyNoWarning() throws Exception {
    final URI uri = new URI("memory://notInvokableAnyNoWarning.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    import Standard.Base.Any.Any
                    import Standard.Base.Data.Numbers
                    foo (fun : Any)  =
                        f = fun
                        x1 = f 123
                        x1
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var r1 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo (x -> x + 1)");
    assertEquals(124, r1.asLong());

    try {
      var r2 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo 123");
      fail("Expecting an exception, not: " + r2);
    } catch (PolyglotException e) {
      assertNotInvokableRuntimeError("123", e);
    }

    assertEquals("No warning diagnostics are expected.", "", getOutput());
  }

  @Test
  public void notInvokableWrongSumType() throws Exception {
    final URI uri = new URI("memory://notInvokableWrongSumType.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    foo x =
                        f = case x of
                          1 -> 33
                          _ -> "foo"
                        x1 = f 123
                        x1
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    try {
      var r1 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo 1");
      fail("Expecting an exception, not: " + r1);
    } catch (PolyglotException e) {
      assertNotInvokableRuntimeError("33", e);
    }

    try {
      var r2 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo 2");
      fail("Expecting an exception, not: " + r2);
    } catch (PolyglotException e) {
      assertNotInvokableRuntimeError("foo", e);
    }

    assertNotInvokableWarning("(Integer | Text)", getOutput());
  }

  /**
   * Tests that a function can be converted into a type through a `from` conversion like any other
   * type, and it will not produce any type warnings.
   */
  @Test
  public void noTypeErrorIfConversionExistsFunctions() throws Exception {
    final URI uri = new URI("memory://noTypeErrorIfConversionExistsFunctions.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                    import Standard.Base.Function.Function
                    type My_Type
                        Value v

                    My_Type.from (that : Function) = My_Type.Value (that 1)
                    foo =
                        f x = x+100
                        y = (f : My_Type)
                        y
                    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var result = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    assertEquals("(My_Type.Value 101)", result.as(Object.class).toString());

    assertEquals("No warning diagnostics are expected.", "", getOutput());
  }

  /**
   * Checks that the micro-distribution variant of Standard.Base can be compiled with type checking
   * enabled.
   */
  @Test
  public void microDistributionBase() throws Exception {
    final URI uri = new URI("memory://microDistributionBase.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import all
    foo = (123 + 10000).to_text.take 3
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var result = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    assertEquals("101", result.as(Object.class));
  }

  private void assertNotInvokableWarning(String expectedType, String message) {
    assertContains(
        "Invoking a value that has a non-function type "
            + expectedType
            + " will result in a Not_Invokable error in runtime.",
        message);
  }

  private void assertNotInvokableRuntimeError(String got, PolyglotException exception) {
    assertContains("Type error: expected a function, but got " + got, exception.getMessage());
  }

  private static void assertContains(String exp, String msg) {
    if (!msg.contains(exp)) {
      fail("Expecting '" + msg + "' to contain '" + exp + "'.");
    }
  }
}
