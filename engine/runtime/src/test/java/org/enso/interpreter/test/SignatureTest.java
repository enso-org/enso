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
  public void runtimeCheckOfAscribedFunctionSignature() throws Exception {
    final URI uri = new URI("memory://neg.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer, Error

    err msg = Error.throw msg
    neg (a : Integer) = 0 - a
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var neg = module.invokeMember("eval_expression", "neg");
    var err = module.invokeMember("eval_expression", "err");
    try {
      var res = neg.execute("Hi");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertTypeError("`a`", "Integer", "Text", e.getMessage());
    }

    var ten = neg.execute(-10);
    assertEquals("Ten", 10, ten.asInt());

    var error = err.execute("I am an error");
    assertTrue("Error value", error.isException());

    var yieldsError = neg.execute(error);
    assertTrue("Yields Error value", yieldsError.isException());
  }

  @Test
  public void runtimeCheckOfAscribedInstanceMethodSignature() throws Exception {
    final URI uri = new URI("memory://twice_instance.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer
    type Neg
        Singleton

        twice self (a : Integer) = a + a
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var neg = module.invokeMember("eval_expression", "Neg.Singleton.twice");

    var ten = neg.execute(5);
    assertEquals("Ten", 10, ten.asInt());

    try {
      var res = neg.execute("Hi");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertTypeError("`a`", "Integer", "Text", e.getMessage());
    }
  }


  @Test
  public void runtimeCheckOfAscribedStaticMethodSignature() throws Exception {
    final URI uri = new URI("memory://twice_static.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer
    type Neg
        twice (a : Integer) = a + a
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var neg = module.invokeMember("eval_expression", "Neg.twice");

    var ten = neg.execute(5);
    assertEquals("Ten", 10, ten.asInt());

    try {
      var res = neg.execute("Hi");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertTypeError("`a`", "Integer", "Text", e.getMessage());
    }
  }

  @Test
  public void runtimeCheckOfAscribedLocalMethodSignature() throws Exception {
    final URI uri = new URI("memory://twice_local.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer

    call_twice x =
        twice (a : Integer) = a + a
        twice x
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var neg = module.invokeMember("eval_expression", "call_twice");

    var ten = neg.execute(5);
    assertEquals("Ten", 10, ten.asInt());

    try {
      var res = neg.execute("Hi");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertTypeError("`a`", "Integer", "Text", e.getMessage());
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
  public void suspendedAscribedParameter() throws Exception {
    final URI uri = new URI("memory://suspended.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer

    type Maybe a
        Nothing
        Some (~unwrap : Integer)
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var some = module.invokeMember("eval_expression", "Maybe.Some 10");
    assertEquals("Can read ten", 10, some.getMember("unwrap").asInt());
    var lazy = module.invokeMember("eval_expression", "Maybe.Some (2 * 5)");
    assertEquals("Can read first time ", 10, lazy.getMember("unwrap").asInt());
    assertEquals("Can read second time", 10, lazy.getMember("unwrap").asInt());
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
      assertTypeError("`v`", "Zero", "Text", ex.getMessage());
    }
    try {
      var v = module.invokeMember("eval_expression", "Bin.Zero One");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("`v`", "Zero", "Zero", ex.getMessage());
    }
  }

  @Test
  public void automaticConversionToAType() throws Exception {
    final URI uri = new URI("memory://convert.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer

    type V
        Val (a : Integer)

        # mul accepts V as the other parameter
        mul self (other : V) = V.Val self.a*other.a

    V.from (that : Integer) = V.Val that

    create x:Integer = V.from x

    # invokes V.mul with Integer parameter, not V!
    mix a:V b:Integer = a.mul b
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var factory = module.invokeMember("eval_expression", "create");
    var mix = module.invokeMember("eval_expression", "mix");

    var six = factory.execute(6);
    var fourtyTwoAsV = mix.execute(six, 7);

    assertEquals("V", fourtyTwoAsV.getMetaObject().getMetaSimpleName());
    assertEquals(42, fourtyTwoAsV.getMember("a").asInt());
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
      assertTypeError("`v`", "One", "Integer", ex.getMessage());
    }
    try {
      var v = module.invokeMember("eval_expression", "Bin.One Zero");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("`v`", "One", "Zero", ex.getMessage());
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
      assertTypeError("`v`", "Zero | One", "Integer", ex.getMessage());
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
      assertTypeError("`v`", "Integer | Range | Vector", "Integer", ex.getMessage());
    }
    var ok2 = module.invokeMember("eval_expression", "Bin.Either Zero");
    assertEquals("binary.Bin", ok2.getMetaObject().getMetaQualifiedName());
    var ok3 = module.invokeMember("eval_expression", "Bin.Vec 5");
    assertEquals("binary.Bin", ok3.getMetaObject().getMetaQualifiedName());
  }

  @Test
  public void partiallyAppliedConstructor() throws Exception {
    final URI uri = new URI("memory://partial.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer

    type V
        Val a b c

    create x:V = x.a + x.b + x.c

    mix a =
      partial = V.Val 1 a
      create partial
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var mix = module.invokeMember("eval_expression", "mix");

    try {
      var res = mix.execute(7);
      fail("No result expected: " + res);
    } catch (PolyglotException ex) {
      assertContains("Type error", ex.getMessage());
      assertContains("expected `x` to be V", ex.getMessage());
      assertContains("got V.Val[partial", ex.getMessage());
      assertContains("a=1", ex.getMessage());
      assertContains("b=7", ex.getMessage());
      assertContains("c=_", ex.getMessage());
    }
  }

  @Test
  public void oversaturatedFunction() throws Exception {
    final URI uri = new URI("memory://oversaturated.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer

    fn a b c =
      sum = a + b + c
      add a = sum + a
      add

    neg x:Integer = -x

    mix n = neg (fn 2 a=4 n)
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var mix = module.invokeMember("eval_expression", "mix");

    try {
      var res = mix.execute(7);
      fail("No result expected: " + res);
    } catch (PolyglotException ex) {
      assertContains("Type error", ex.getMessage());
      assertContains("expected `x` to be Integer", ex.getMessage());
      assertContains("got oversaturated.fn[", ex.getMessage());
      assertContains("a=2", ex.getMessage());
      assertContains("b=7", ex.getMessage());
      assertContains("c=_", ex.getMessage());
      assertContains("+a=4", ex.getMessage());
    }
  }

  @Test
  public void suspendedArgumentsUnappliedFunction() throws Exception {
    final URI uri = new URI("memory://suspended.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer

    fn ~a ~b ~c =
      add x = if x == 0 then 0 else x * (a + b + c)
      add

    neg x:Integer = -x

    mix a = neg (fn c=(2/0) b=(a/0))
    """, uri.getHost())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var mix = module.invokeMember("eval_expression", "mix");

    try {
      var res = mix.execute(0);
      fail("No result expected: " + res);
    } catch (PolyglotException ex) {
      assertContains("Type error", ex.getMessage());
      assertContains("expected `x` to be Integer", ex.getMessage());
      assertContains("got suspended.fn[", ex.getMessage());
      assertContains("a=_", ex.getMessage());
      assertContains("b=suspended.mix<arg-b>", ex.getMessage());
      assertContains("c=suspended.mix<arg-c>", ex.getMessage());
      assertContains("[suspended:9:28-30]", ex.getMessage());
    }
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

  private static void assertContains(String exp, String msg) {
    if (!msg.contains(exp)) {
      fail("Expecting " + msg + " to contain " + exp);
    }
  }
}
