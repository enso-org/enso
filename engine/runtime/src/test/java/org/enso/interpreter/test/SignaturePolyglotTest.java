package org.enso.interpreter.test;

import static org.enso.interpreter.test.SignatureTest.assertTypeError;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.OutputStream;
import java.net.URI;
import java.time.format.DateTimeFormatter;
import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class SignaturePolyglotTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx =
        defaultContextBuilder()
            .out(OutputStream.nullOutputStream())
            .err(OutputStream.nullOutputStream())
            .build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Test
  public void polyglotDataTimeFormatter() throws Exception {
    final URI uri = new URI("memory://formatter.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import all

    polyglot java import java.time.format.DateTimeFormatter

    fn (x : DateTimeFormatter) = x.to_text
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var fn = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fn");

    assertStartsWith(
        "ParseCaseSensitive(false)(Value", fn.execute(DateTimeFormatter.ISO_DATE).asString());

    try {
      var ret = fn.execute("Hi");
      fail("Should fail, but got: " + ret);
    } catch (PolyglotException e) {
      assertTypeError("`x`", "java.time.format.DateTimeFormatter", "Text", e.getMessage());
    }

    try {
      var ret = fn.execute(new StringBuilder("Hi"));
      fail("Should fail, but got: " + ret);
    } catch (PolyglotException e) {
      assertTypeError("`x`", "java.time.format.DateTimeFormatter", "StringBuilder", e.getMessage());
    }
  }

  @Test
  public void polyglotDataTimeFormatterAndText() throws Exception {
    final URI uri = new URI("memory://formatter.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import all

    polyglot java import java.time.format.DateTimeFormatter

    fn x:(DateTimeFormatter | Text) = x.to_text
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var fn = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fn");

    var ret = fn.execute("Hi");
    assertEquals("Hi", ret.asString());

    assertStartsWith(
        "ParseCaseSensitive(false)(Value", fn.execute(DateTimeFormatter.ISO_DATE).asString());
  }

  private static void assertStartsWith(String exp, String real) {
    if (!real.startsWith(exp)) {
      fail("Expecting something that starts with '" + exp + "', but got '" + real + "'");
    }
  }
}
