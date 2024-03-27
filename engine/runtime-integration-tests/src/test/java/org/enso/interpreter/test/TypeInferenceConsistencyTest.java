package org.enso.interpreter.test;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.net.URI;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests that verify that if type inference issues a warning about an error, that error actually
 * occurs in the runtime.
 */
public class TypeInferenceConsistencyTest extends TestBase {
  private static Context ctx;
  private static ByteArrayOutputStream output = new ByteArrayOutputStream();

  @BeforeClass
  public static void prepareCtx() {
    ctx =
        defaultContextBuilder()
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.ENABLE_TYPE_CHECK, "true")
            .out(output)
            .err(output)
            .build();
  }

  @Before
  public void cleanMessages() {
    output.reset();
  }

  private String getOutput() {
    return output.toString();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
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
      var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
      fail("Expecting an exception, not: " + neg);
    } catch (PolyglotException e) {
      assertContains("Type error: expected a function, but got 1", e.getMessage());
    }

    // But we also expect the compile warning:
    assertContains(
        "Invoking a value that has a non-function type Integer will result in a Not_Invokable error"
            + " in runtime.",
        getOutput());
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
}
