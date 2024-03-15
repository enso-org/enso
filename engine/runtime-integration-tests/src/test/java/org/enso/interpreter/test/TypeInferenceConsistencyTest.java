package org.enso.interpreter.test;

import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;

import static org.junit.Assert.*;

/** Tests that verify that if type inference issues a warning about an error, that error actually occurs in the runtime. */
public class TypeInferenceConsistencyTest extends TestBase {
  private static Context ctx;
  private static ByteArrayOutputStream output = new ByteArrayOutputStream();

  @BeforeClass
  public static void prepareCtx() {
    ctx =
        defaultContextBuilder()
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
        Source.newBuilder(
                "enso", """
    foo = 1 2
    """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
      fail("Expecting an exception, not: " + neg);
    } catch (PolyglotException e) {
      System.out.println(e.getMessage());
      System.out.println(e.getClass());
      System.out.println(e.getCause());

      // The runtime error
      assertContains("(Not_Invokable.Error 1)", e.toString());
    }

    // But we also expect the compile warning:
    assertContains("Invoking a value that has a non-function type Integer will result in a Not_Invokable error in runtime.", getOutput());
  }
}
