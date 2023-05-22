package org.enso.interpreter.test;

import java.net.URI;
import java.net.URISyntaxException;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
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

  @Test
  public void binaryWithZero() throws Exception {
    Value module = exampleWithBinary();
    var ok = module.invokeMember("eval_expression", "Bin.Zero Zero");
    assertEquals("binary.Bin", ok.getMetaObject().getMetaQualifiedName());
    try {
      var v = module.invokeMember("eval_expression", "Bin.Zero 'hi'");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertEquals("Type_Error.Error", ex.getMessage());
    }
    try {
      var v = module.invokeMember("eval_expression", "Bin.Zero One");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertEquals("Type_Error.Error", ex.getMessage());
    }
  }

  private Value exampleWithBinary() throws URISyntaxException {
    var uri = new URI("memory://binary.enso");
    var src = Source.newBuilder("enso", """
    type Zero
    type One

    type Bin
        Zero (v:Zero)
        One (v:One)
        Either v:(Zero | One)
    """, uri.getHost()).uri(uri).buildLiteral();
    return ctx.eval(src);
  }

  @Test
  public void binaryWithOne() throws Exception {
    Value module = exampleWithBinary();
    var ok = module.invokeMember("eval_expression", "Bin.One One");
    assertEquals("binary.Bin", ok.getMetaObject().getMetaQualifiedName());
    try {
      var v = module.invokeMember("eval_expression", "Bin.One 10");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertEquals("Type_Error.Error", ex.getMessage());
    }
    try {
      var v = module.invokeMember("eval_expression", "Bin.One Zero");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertEquals("Type_Error.Error", ex.getMessage());
    }
  }
}
