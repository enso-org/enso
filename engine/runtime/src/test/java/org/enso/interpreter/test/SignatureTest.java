package org.enso.interpreter.test;

import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;

import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.BeforeClass;
import org.junit.Test;

public class SignatureTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx = defaultContextBuilder()
        .out(OutputStream.nullOutputStream())
        .err(OutputStream.nullOutputStream())
        .build();
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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "neg");
      fail("Expecting an exception from compilation, not: " + neg);
    } catch (PolyglotException e) {
      assertTrue("It is a syntax error exception", e.isSyntaxError());
    }
  }

  @Test
  public void wrongLiteralSignature() throws Exception {
    final URI uri = new URI("memory://literal_signature.enso");
    final Source src = Source.newBuilder("enso", """
    neg a = 0 - a:Xyz
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember("eval_expression", "neg").execute(-1);
      fail("Expecting an exception from compilation, not: " + neg);
    } catch (PolyglotException e) {
      assertTrue("It is a syntax error exception", e.isSyntaxError());
    }
  }

  @Test
  public void wrongExpressionSignature() throws Exception {
    final URI uri = new URI("memory://exp_signature.enso");
    final Source src = Source.newBuilder("enso", """
    neg a = (0 - a):Xyz
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember("eval_expression", "neg").execute(-1);
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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "neg");
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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "neg");
    var err = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "err");
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
  public void lazyIntegerInConstructor() throws Exception {
    final URI uri = new URI("memory://int_simple_complex.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import all

    type Int
        Simple v
        Complex (~unwrap : Int)

        value self = case self of
            Int.Simple v -> v
            Int.Complex unwrap -> unwrap.value

        + self (that:Int) = Int.Simple self.value+that.value

    simple v = Int.Simple v
    complex x y = Int.Complex (x+y)
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    var simple = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "simple");
    var complex = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "complex");

    var six = simple.execute(6);
    var seven = simple.execute(7);
    var some13 = complex.execute(six, seven);
    var thirteen = some13.invokeMember("value");
    assertNotNull("member found", thirteen);
    assertEquals(13, thirteen.asInt());

    var someHello = complex.execute("Hello", "World");
    try {
      var error = someHello.invokeMember("value");
      fail("not expecting any value: " + error);
    } catch (PolyglotException e) {
      assertTypeError("`unwrap`", "Int", "Text", e.getMessage());
    }
    try {
      var secondError = someHello.invokeMember("value");
      fail("not expecting any value again: " + secondError);
    } catch (PolyglotException e) {
      assertTypeError("`unwrap`", "Int", "Text", e.getMessage());
    }
  }

  @Test
  public void runtimeCheckOfLazyAscribedFunctionSignature() throws Exception {
    final URI uri = new URI("memory://neg_lazy.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer, IO

    build (~zero : Integer) =
      neg (~a : Integer) = zero - a
      neg

    make arr = build <|
      arr.at 0
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    var zeroValue = new Object[] { 0 };
    var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "make").execute((Object)zeroValue);

    zeroValue[0] = "Wrong";
    try {
      var error = neg.execute(-5);
      fail("Expecting an error: " + error);
    } catch (PolyglotException ex) {
      assertTypeError("`zero`", "Integer", "Text", ex.getMessage());
    }

    zeroValue[0] = 0;
    var five = neg.execute(-5);
    assertEquals("Five", 5, five.asInt());

    try {
      var res = neg.execute("Hi");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertTypeError("`a`", "Integer", "Text", e.getMessage());
    }
    zeroValue[0] = 5;
    var fifteen = neg.execute(-10);
    assertEquals("Five + Ten as the zeroValue[0] is always read again", 15, fifteen.asInt());

    zeroValue[0] = 0;
    var ten = neg.execute(-10);
    assertEquals("Just ten as the zeroValue[0] is always read again", 10, ten.asInt());
  }

  @Test
  public void runtimeCheckOfLazyAscribedConstructorSignature() throws Exception {
    final URI uri = new URI("memory://neg_lazy_const.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer, IO, Polyglot

    type Lazy
        Value (~zero : Integer)

        neg self (~a : Integer) = self.zero - a

    make arr = Lazy.Value <|
      Polyglot.invoke arr "add" [ arr.length ]
      arr.at 0
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    var zeroValue = new ArrayList<Integer>();
    zeroValue.add(0);
    var lazy = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "make").execute((Object)zeroValue);
    assertEquals("No read from zeroValue, still size 1", 1, zeroValue.size());

    var five = lazy.invokeMember("neg", -5);
    assertEquals("Five", 5, five.asInt());
    assertEquals("One read from zeroValue, size 2", 2, zeroValue.size());

    try {
      var res = lazy.invokeMember("neg", "Hi");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertTypeError("`a`", "Integer", "Text", e.getMessage());
    }
    zeroValue.set(0, 5);
    var fifteen = lazy.invokeMember("neg", -10);
    assertEquals("Five + Ten as the zeroValue[0] is never read again", 10, fifteen.asInt());
    assertEquals("One read from zeroValue, size 2", 2, zeroValue.size());

    zeroValue.set(0, 0);
    var ten = lazy.invokeMember("neg", -9);
    assertEquals("Just nine as the zeroValue[0] is always read again", 9, ten.asInt());
    assertEquals("One read from zeroValue, size 2", 2, zeroValue.size());
  }

  @Test
  public void runtimeCheckOfAscribedInstanceMethodSignature() throws Exception {
    final URI uri = new URI("memory://twice_instance.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer
    type Neg
        Singleton

        twice self (a : Integer) = a + a
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Neg.Singleton.twice");

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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Neg.twice");

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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "call_twice");

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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "neg");
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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var some = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Maybe.Some 10");
    assertEquals("Can read ten", 10, some.getMember("unwrap").asInt());
  }

  @Test
  public void ascribedWithAParameterAndMethod() throws Exception {
    final URI uri = new URI("memory://getter.enso");
    final Source src = Source.newBuilder("enso", """
    type Maybe a
        Nothing
        Some unwrap:a

        get : a
        get self = self.unwrap
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var some = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Maybe.Some 10");
    assertEquals("Can get ten", 10, some.invokeMember("get").asInt());
  }

  @Test
  public void suspendedAscribedParameter() throws Exception {
    final URI uri = new URI("memory://suspended.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import Integer

    type Maybe a
        Nothing
        Some (~unwrap : Integer)
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var some = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Maybe.Some 10");
    assertEquals("Can read ten", 10, some.getMember("unwrap").asInt());
    var lazy = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Maybe.Some (2 * 5)");
    assertEquals("Can read first time ", 10, lazy.getMember("unwrap").asInt());
    assertEquals("Can read second time", 10, lazy.getMember("unwrap").asInt());
  }

  @Test
  public void binaryWithZero() throws Exception {
    Value module = exampleWithBinary();
    var ok = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Zero Zero");
    assertEquals("binary.Bin", ok.getMetaObject().getMetaQualifiedName());
    try {
      var v = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Zero 'hi'");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("`v`", "Zero", "Text", ex.getMessage());
    }
    try {
      var v = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Zero One");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("`v`", "Zero", "Zero", ex.getMessage());
    }
  }

  /**
   * The folowwing test should not end with Type_Error, but rather with Panic (Compilation error).
   */
  @Test
  public void panicSentinelSupersedesTypeError() throws URISyntaxException {
    URI uri = new URI("memory://panic_sentinel.enso");
    var src = Source.newBuilder("enso", """
    from Standard.Base import Integer
    my_func (x : Integer) = x + 1
    main = my_func (Non_Existing_Func 23)
    """, uri.getAuthority())
        .uri(uri)
        .buildLiteral();
    Value module = ctx.eval(src);
    try {
      module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "my_func (Non_Existing_Func 23)");
      fail("Expecting Compile error");
    } catch (PolyglotException e) {
      assertContains("Compilation aborted", e.getMessage());
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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var factory = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create");
    var mix = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "mix");

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
    """,uri.getAuthority()).uri(uri).buildLiteral();
    return ctx.eval(src);
  }

  @Test
  public void binaryWithOne() throws Exception {
    Value module = exampleWithBinary();
    var ok = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.One One");
    assertEquals("binary.Bin", ok.getMetaObject().getMetaQualifiedName());
    try {
      var v = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.One 10");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("`v`", "One", "Integer", ex.getMessage());
    }
    try {
      var v = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.One Zero");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("`v`", "One", "Zero", ex.getMessage());
    }
  }

  @Test
  public void binaryWithEither() throws Exception {
    Value module = exampleWithBinary();
    var ok1 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Either One");
    assertEquals("binary.Bin", ok1.getMetaObject().getMetaQualifiedName());
    try {
      var v = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Either 10");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("`v`", "Zero | One", "Integer", ex.getMessage());
    }
    var ok2 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Either Zero");
    assertEquals("binary.Bin", ok2.getMetaObject().getMetaQualifiedName());
  }

  @Test
  public void binaryWithVec() throws Exception {
    Value module = exampleWithBinary();
    var ok1 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Vec [1, 2, 3]");
    assertEquals("binary.Bin", ok1.getMetaObject().getMetaQualifiedName());
    try {
      var v = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Vec 'Hi'");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("`v`", "Integer | Range | Vector", "Integer", ex.getMessage());
    }
    var ok2 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Either Zero");
    assertEquals("binary.Bin", ok2.getMetaObject().getMetaQualifiedName());
    var ok3 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Vec 5");
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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var mix = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "mix");

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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var mix = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "mix");

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
    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var mix = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "mix");

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

  @Test
  public void andConversions() throws Exception {
    final URI uri = new URI("memory://and_conv.enso");
    final Source src = Source.newBuilder("enso", """
    from Standard.Base import all

    type Plus
        Impl value dict

        + self (that:Plus) = if self.dict != that.dict then Panic.throw "panic!" else
          self.dict.plus self.value that.value
    type Mul
        Impl value dict

        * self (that:Mul) = if self.dict != that.dict then Panic.throw "panic!" else
          self.dict.mul self.value that.value

    compute (a : Plus & Mul) (b : Plus & Mul) =
      p = a+b
      m = a*b
      p:Plus + m:Plus

    type BooleanPlus
        plus a:Boolean b:Boolean = a || b
    Plus.from(that:Boolean) = Plus.Impl that BooleanPlus

    type BooleanMul
        mul a:Boolean b:Boolean = a && b
    Mul.from(that:Boolean) = Mul.Impl that BooleanMul

    """,uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var compute = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "compute");

    assertTrue("true & true", compute.execute(true, true).asBoolean());
    assertTrue("true & false", compute.execute(true, false).asBoolean());
    assertFalse("false & false", compute.execute(false, false).asBoolean());
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
