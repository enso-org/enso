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
      assertTypeError("Argument #1", "Zero", "Text", ex.getMessage());
    }
    try {
      var v = module.invokeMember("eval_expression", "Bin.Zero One");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("Argument #1", "Zero", "Zero", ex.getMessage());
    }
  }

  private Value exampleWithBinary() throws URISyntaxException {
    var uri = new URI("memory://binary.enso");
    var src = Source.newBuilder("enso", """
    from Standard.Base import all

    type Zero
    type One

    type Bin
        Zero (v:Zero)
        One (v:One)
        Either v:(Zero | One)
        Vec v:(Integer | Range | Vector (Integer | Range))
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
      assertTypeError("Argument #1", "One", "Integer", ex.getMessage());
    }
    try {
      var v = module.invokeMember("eval_expression", "Bin.One Zero");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("Argument #1", "One", "Zero", ex.getMessage());
    }
  }

  @Test
  public void binaryWithEither() throws Exception {
    Value module = exampleWithBinary();
    var ok1 = module.invokeMember("eval_expression", "Bin.Either One");
    assertEquals("binary.Bin", ok1.getMetaObject().getMetaQualifiedName());
    try {
      var v = module.invokeMember("eval_expression", "Bin.Either 10");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("Argument #1", "Zero, One", "Integer", ex.getMessage());
    }
    var ok2 = module.invokeMember("eval_expression", "Bin.Either Zero");
    assertEquals("binary.Bin", ok2.getMetaObject().getMetaQualifiedName());
  }

  @Test
  public void binaryWithVec() throws Exception {
    Value module = exampleWithBinary();
    var ok1 = module.invokeMember("eval_expression", "Bin.Vec [1, 2, 3]");
    assertEquals("binary.Bin", ok1.getMetaObject().getMetaQualifiedName());
    try {
      var v = module.invokeMember("eval_expression", "Bin.Vec 'Hi'");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("Argument #1", "Integer, Range, Vector", "Integer", ex.getMessage());
    }
    var ok2 = module.invokeMember("eval_expression", "Bin.Either Zero");
    assertEquals("binary.Bin", ok2.getMetaObject().getMetaQualifiedName());
    var ok3 = module.invokeMember("eval_expression", "Bin.Vec 5");
    assertEquals("binary.Bin", ok3.getMetaObject().getMetaQualifiedName());
  }

  private static void assertTypeError(String expArg, String expType, String realType, String msg) {
    if (!msg.contains(expArg)) {
      fail("Expecting value " + expArg + " in " + msg);
    }
    if (!msg.contains(expType)) {
      fail("Expecting value " + expType + " in " + msg);
    }
    if (!msg.contains(realType)) {
      fail("Expecting value " + realType + " in " + msg);
    }
  }
}
