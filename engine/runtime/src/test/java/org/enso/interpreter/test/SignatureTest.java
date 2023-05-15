package org.enso.interpreter.test;

import java.net.URI;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.BeforeClass;
import org.junit.Test;

public class SignatureTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Test
  public void wrongFunctionSignature() throws Exception {
    final URI uri = new URI("memory://neg.enso");
    final Source src = Source.newBuilder("enso", """
    neg : Xyz -> Abc
    neg a = 0 - a
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember("eval_expression", "neg");
      fail("Expecting an exception from compilation, not: " + neg);
    } catch (PolyglotException e) {
      assertTrue("It is a syntax error exception", e.isSyntaxError());
    }
  }

  @Test
  public void wrongAscribedTypeSignature() throws Exception {
    final URI uri = new URI("memory://neg.enso");
    final Source src = Source.newBuilder("enso", """
    neg (a : Xyz) = 0 - a
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember("eval_expression", "neg");
      fail("Expecting an exception from compilation, not: " + neg);
    } catch (PolyglotException e) {
      assertTrue("It is a syntax error exception", e.isSyntaxError());
    }
  }

  @Test
  public void wrongAscribedInConstructor() throws Exception {
    final URI uri = new URI("memory://constructor.enso");
    final Source src = Source.newBuilder("enso", """
    type Neg
      Val (a : Xyz)

    neg = Neg.Val 10
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember("eval_expression", "neg");
      fail("Expecting an exception from compilation, not: " + neg);
    } catch (PolyglotException e) {
      assertTrue("It is a syntax error exception", e.isSyntaxError());
    }
  }

  @Test
  public void ascribedWithAParameter() throws Exception {
    final URI uri = new URI("memory://constructor.enso");
    final Source src = Source.newBuilder("enso", """
    type Maybe a
        Nothing
        Some unwrap:a
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var some = module.invokeMember("eval_expression", "Maybe.Some 10");
    assertEquals("Can read ten", 10, some.getMember("unwrap").asInt());
  }
}
