package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import org.enso.common.MethodNames;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.Test;

public class SignatureTest extends ContextTest {

  @Test
  public void wrongFunctionSignature() throws Exception {
    final URI uri = new URI("memory://neg.enso");
    final Source src =
        Source.newBuilder(
                "enso", """
    neg : Xyz -> Abc
    neg a = 0 - a
    """, uri.getAuthority())
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
    final Source src =
        Source.newBuilder("enso", """
    neg a = 0 - a:Xyz
    """, uri.getAuthority())
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
    final Source src =
        Source.newBuilder("enso", """
    neg a = (0 - a):Xyz
    """, uri.getAuthority())
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
    final Source src =
        Source.newBuilder("enso", """
    neg (a : Xyz) = 0 - a
    """, uri.getAuthority())
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, Error

    err msg = Error.throw msg
    neg (a : Integer) = 0 - a
    """,
                uri.getAuthority())
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
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
    """,
                uri.getAuthority())
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, IO

    build (~zero : Integer) =
      neg (~a : Integer) = zero - a
      neg

    make arr = build <|
      arr.at 0
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    var zeroValue = new Object[] {0};
    var neg =
        module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "make").execute((Object) zeroValue);

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
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, IO, Polyglot

    type Lazy
        Value (~zero : Integer)

        neg self (~a : Integer) = self.zero - a

    make arr = Lazy.Value <|
      Polyglot.invoke arr "add" [ arr.length ]
      arr.at 0
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    var zeroValue = new ArrayList<Integer>();
    zeroValue.add(0);
    var lazy =
        module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "make").execute((Object) zeroValue);
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer
    type Neg
        Singleton

        twice self (a : Integer) = a + a
    """,
                uri.getAuthority())
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer
    type Neg
        twice (a : Integer) = a + a
    """,
                uri.getAuthority())
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer

    call_twice x =
        twice (a : Integer) = a + a
        twice x
    """,
                uri.getAuthority())
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
  public void runtimeCheckOfSelfType() throws Exception {
    final URI uri = new URI("memory://self_type_check.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import all
    type My_Type
        Value x
        f self y = self.x+y

    type Other_Type
        Ctor x

    normal_call = (My_Type.Value 42).f 10
    static_call = My_Type.f (My_Type.Value 23) 100
    invalid_static_call = My_Type.f (Other_Type.Ctor 11) 1000
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var normal_call = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "normal_call");
    assertEquals("Normal call", 52, normal_call.asInt());

    var static_call = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "static_call");
    assertEquals("Static call", 123, static_call.asInt());

    try {
      var invalid_static_call =
          module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "invalid_static_call");
      fail("Expecting an exception, not: " + invalid_static_call);
    } catch (PolyglotException e) {
      assertTypeError("`self`", "My_Type", "Other_Type", e.getMessage());
    }
  }

  @Test
  public void wrongAscribedInConstructor() throws Exception {
    final URI uri = new URI("memory://constructor.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    type Neg
      Val (a : Xyz)

    neg = Neg.Val 10
    """,
                uri.getAuthority())
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
    type Maybe a
        Nothing
        Some unwrap:a
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var some = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Maybe.Some 10");
    assertEquals("Can read ten", 10, some.getMember("unwrap").asInt());
  }

  @Test
  public void ascribedWithAParameterAndMethod() throws Exception {
    final URI uri = new URI("memory://getter.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    type Maybe a
        Nothing
        Some unwrap:a

        get : a
        get self = self.unwrap
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var some = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Maybe.Some 10");
    assertEquals("Can get ten", 10, some.invokeMember("get").asInt());
  }

  @Test
  public void suspendedAscribedParameter() throws Exception {
    final URI uri = new URI("memory://suspended.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer

    type Maybe a
        Nothing
        Some (~unwrap : Integer)
    """,
                uri.getAuthority())
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
      assertTypeError("`v`", "Zero", "One", ex.getMessage());
    }
  }

  /**
   * The folowwing test should not end with Type_Error, but rather with Panic (Compilation error).
   */
  @Test
  public void panicSentinelSupersedesTypeError() throws URISyntaxException {
    URI uri = new URI("memory://panic_sentinel.enso");
    var src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer
    my_func (x : Integer) = x + 1
    main = my_func (Non_Existing_Func 23)
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();
    try {
      var module = ctx.eval(src);
      module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "my_func (Non_Existing_Func 23)");
      fail("Expecting Compile error");
    } catch (PolyglotException e) {
      assertContains("The name `Non_Existing_Func` could not be found.", e.getMessage());
    }
  }

  @Test
  public void automaticConversionToAType() throws Exception {
    final URI uri = new URI("memory://convert.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer

    type V
        Val (a : Integer)

        # mul accepts V as the other parameter
        mul self (other : V) = V.Val self.a*other.a

    V.from (that : Integer) = V.Val that

    create x:Integer = V.from x

    # invokes V.mul with Integer parameter, not V!
    mix a:V b:Integer = a.mul b
    """,
                uri.getAuthority())
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

  @Test
  public void selfTypeConversion() throws Exception {
    final URI uri = new URI("memory://self_type_conversion.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import all
    type My_Type
        Value x
        f self y = self.x+y

    type Convertible_Type
        A x

    type Inconvertible_Type
        B x

    My_Type.from (that : Convertible_Type) = My_Type.Value that.x+1

    static_my_type = My_Type.f (My_Type.Value 23) 1000
    static_convertible = My_Type.f (Convertible_Type.A 23) 1000
    static_inconvertible = My_Type.f (Inconvertible_Type.B 23) 1000
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var static_my_type = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "static_my_type");
    assertEquals(
        "My_Type.f is executed directly on 23, yielding 1023", 1023, static_my_type.asInt());

    var convertible = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "static_convertible");
    assertEquals(
        "My_Type.f is executed on the converted value, so 23 is incremented to 24, yielding 1024"
            + " proving that the conversion has been applied",
        1024,
        convertible.asInt());

    try {
      var invalid_static_call =
          module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "static_inconvertible");
      fail("Expecting an exception, not: " + invalid_static_call);
    } catch (PolyglotException e) {
      assertTypeError("`self`", "My_Type", "Inconvertible_Type", e.getMessage());
    }
  }

  private Value exampleWithBinary() throws URISyntaxException {
    var uri = new URI("memory://binary.enso");
    var src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import all

    type Zero
    type One

    type Bin
        Zero (v:Zero)
        One (v:One)
        Either v:(Zero | One)
        Vec v:(Integer | Range | Vector (Integer | Range))
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();
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
      assertTypeError("`v`", "Integer | Range | Vector", "Text", ex.getMessage());
    }
    var ok2 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Either Zero");
    assertEquals("binary.Bin", ok2.getMetaObject().getMetaQualifiedName());
    var ok3 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Bin.Vec 5");
    assertEquals("binary.Bin", ok3.getMetaObject().getMetaQualifiedName());
  }

  @Test
  public void partiallyAppliedConstructor() throws Exception {
    final URI uri = new URI("memory://partial.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer

    type V
        Val a b c

    create x:V = x.a + x.b + x.c

    mix a =
      partial = V.Val 1 a
      create partial
    """,
                uri.getAuthority())
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer

    fn a b c =
      sum = a + b + c
      add a = sum + a
      add

    neg x:Integer = -x

    mix n = neg (fn 2 a=4 n)
    """,
                uri.getAuthority())
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer

    fn ~a ~b ~c =
      add x = if x == 0 then 0 else x * (a + b + c)
      add

    neg x:Integer = -x

    mix a = neg (fn c=(2/0) b=(a/0))
    """,
                uri.getAuthority())
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
    final Source src =
        Source.newBuilder(
                "enso",
                """
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

    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var compute = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "compute");

    assertTrue("true & true", compute.execute(true, true).asBoolean());
    assertTrue("true & false", compute.execute(true, false).asBoolean());
    assertFalse("false & false", compute.execute(false, false).asBoolean());
  }

  @Test
  public void unionInArgument() throws Exception {
    var uri = new URI("memory://binary.enso");
    var src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import all
    foo (arg : Integer | Text) = arg
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();
    var module = ctx.eval(src);

    var ok1 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo 42");
    assertEquals(42, ok1.asInt());
    var ok2 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo 'Hi'");
    assertEquals("Hi", ok2.asString());
    try {
      var v = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo []");
      fail("Expecting an error, not " + v);
    } catch (PolyglotException ex) {
      assertTypeError("`arg`", "Integer | Text", "Vector", ex.getMessage());
    }
  }

  @Test
  public void unresolvedReturnTypeSignature() throws Exception {
    final URI uri = new URI("memory://neg.enso");
    final Source src =
        Source.newBuilder("enso", """
    neg a -> Xyz = 0 - a
    """, uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctx.eval(src);
      var neg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "neg");
      fail("Expecting an exception from compilation, not: " + neg);
    } catch (PolyglotException e) {
      System.out.println(e);
      assertTrue("It is a syntax error exception", e.isSyntaxError());
    }
  }

  @Test
  public void validReturnTypeSignature() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer
    add1 a b -> Integer = a+b
    add2 (a : Integer) (b : Integer) -> Integer = a+b
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var add1 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "add1");
    assertEquals(3, add1.execute(1, 2).asInt());

    var add2 = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "add2");
    assertEquals(3, add2.execute(1, 2).asInt());
  }

  @Test
  public void returnTypeCheckOptInError() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer
    plusChecked a b -> Integer = b+a
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var plusChecked = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "plusChecked");
    assertEquals(5, plusChecked.execute(2, 3).asInt());
    try {
      var res = plusChecked.execute("a", "b");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains(
          "expected the result of `plusChecked` to be Integer, but got Text", e.getMessage());
    }
  }

  @Test
  public void returnTypeNoCheckJustSignature() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                type Integer

                plusChecked a b =
                    x:Integer
                    x = a + b
                    x
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var plusChecked = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "plusChecked");
    assertEquals(5, plusChecked.execute(2, 3).asInt());
    var res = plusChecked.execute("a", "b");
    assertEquals("ab", res.asString());
  }

  @Test
  public void returnTypeCheckByLastStatement() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                from Standard.Base import Integer

                plusChecked a b =
                    x = a + b
                    x:Integer
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var plusChecked = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "plusChecked");
    assertEquals(5, plusChecked.execute(2, 3).asInt());
    try {
      var res = plusChecked.execute("a", "b");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains("expected expression to be Integer, but got Text", e.getMessage());
    }
  }

  /**
   * Similar scenario to {@code returnTypeCheckOptInError}, but with the opt out signature the check
   * is not currently performed.
   */
  @Test
  public void returnTypeCheckOptOut() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer
    plusUnchecked : Integer -> Integer -> Integer
    plusUnchecked a b = b+a
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var plusChecked = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "plusUnchecked");
    assertEquals(5, plusChecked.execute(2, 3).asInt());
    // This variant does allow other types, because the signature remains unchecked:
    assertEquals("ba", plusChecked.execute("a", "b").asString());
  }

  @Test
  public void returnTypeCheckOptInErrorZeroArguments() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer
    constant -> Integer = "foo"
    foo a b = a + constant + b
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var plusChecked = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    try {
      var res = plusChecked.execute(2, 3);
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains(
          "expected the result of `constant` to be Integer, but got Text", e.getMessage());
    }
  }

  @Test
  public void returnTypeCheckOptInErrorZeroArgumentsExpression() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer
    foo a =
        x -> Integer = a+a
        x+x
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var foo = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    assertEquals(8, foo.execute(2).asInt());
    try {
      var res = foo.execute(".");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains("expected the result of `x` to be Integer, but got Text", e.getMessage());
    }
  }

  @Test
  public void returnTypeCheckOptInErrorZeroArgumentsBlock() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, IO
    foo a =
        x -> Integer =
            a+a
        x+x
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var plusChecked = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    assertEquals(8, plusChecked.execute(2).asInt());
    try {
      var res = plusChecked.execute(".");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains("expected the result of `x` to be Integer, but got Text", e.getMessage());
    }
  }

  @Test
  public void returnTypeCheckOptInAllowDataflowErrors() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, Error
    foo x -> Integer = case x of
        1 -> 100
        2 -> "TWO"
        3 -> Error.throw "My error"
        _ -> x+1
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var foo = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    assertEquals(100, foo.execute(1).asInt());

    try {
      var res = foo.execute(2);
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains("expected the result of `foo` to be Integer, but got Text", e.getMessage());
    }

    var res = foo.execute(3);
    assertTrue(res.isException());
    assertContains("My error", res.toString());
  }

  @Test
  public void returnTypeCheckOptInTailRec() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, Error
    factorial (x : Integer) -> Integer =
        go n acc -> Integer =
            if n == 0 then acc else
                if n == 10 then "TEN :)" else
                    @Tail_Call go (n-1) (acc*n)
        go x 1
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var factorial = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "factorial");
    assertEquals(120, factorial.execute(5).asInt());
    assertEquals(1, factorial.execute(0).asInt());
    try {
      var res = factorial.execute(20);
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains("expected the result of `go` to be Integer, but got Text", e.getMessage());
    }
  }

  /**
   * An additional test to ensure that the way the return type check is implemented does not break
   * the TCO. We execute a recursive computation that goes 100k frames deep - if TCO did not kick in
   * here, we'd get a stack overflow.
   */
  @Test
  public void returnTypeCheckOptInTCO() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, Error
    foo (counter : Integer) (trap : Integer) -> Integer =
        go i acc -> Integer =
            if i == 0 then acc else
                if i == trap then "TRAP!" else
                    @Tail_Call go (i-1) (acc+1)
        go counter 1
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var foo = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    long n = 100000;
    assertEquals(n + 1, foo.execute(n, -1).asInt());
    try {
      var res = foo.execute(n, 1);
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains("expected the result of `go` to be Integer, but got Text", e.getMessage());
    }
  }

  @Test
  public void returnTypeCheckOptInTCO2() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, Error
    foo_ok counter -> Integer =
        if counter == 0 then 0 else
            @Tail_Call foo_ok (counter-1)
    foo_bad counter -> Integer =
        if counter == 0 then "ZERO" else
            @Tail_Call foo_bad (counter-1)
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var foo_ok = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo_ok");
    long n = 100000;
    assertEquals(0, foo_ok.execute(n).asInt());

    try {
      var foo_bad = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo_bad");
      var res = foo_bad.execute(n);
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertEquals(
          "Type error: expected the result of `foo_bad` to be Integer, but got Text.",
          e.getMessage());
    }
  }

  @Test
  public void returnTypeCheckErrorSignature() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer
    import Standard.Base.Errors.Illegal_State.Illegal_State
    foo a -> Integer ! Illegal_State =
        a+a
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var foo = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    assertEquals(20, foo.execute(10).asInt());
    try {
      var res = foo.execute(".");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains("expected the result of `foo` to be Integer, but got Text", e.getMessage());
    }
  }

  /**
   * The `!` part in the type signature is currently only for documentation purposes - it is not
   * checked. Other kinds of dataflow errors may be returned as well and this is not an error. In
   * particular, we don't distinguish errors raised by the function from errors propagate from its
   * inputs.
   */
  @Test
  public void returnTypeCheckErrorSignatureAllowsAllErrors() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, Error
    import Standard.Base.Errors.Illegal_Argument.Illegal_Argument
    import Standard.Base.Errors.Illegal_State.Illegal_State

    foo a -> Integer ! Illegal_State =
        Error.throw (Illegal_Argument.Error "foo: "+a.to_text)
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var foo = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    var result = foo.execute(10);
    assertTrue(result.isException());
    assertContains("Illegal_Argument.Error 'foo: 10'", result.toString());
  }

  @Test
  public void returnTypeCheckSum() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, Text
    foo a -> Integer | Text =
        a+a
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var foo = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    assertEquals(20, foo.execute(10).asInt());
    assertEquals("..", foo.execute(".").asString());

    try {
      Object vec = new Integer[] {1, 2, 3};
      var res = foo.execute(vec);
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains(
          "expected the result of `foo` to be Integer | Text, but got Vector", e.getMessage());
    }
  }

  @Test
  public void returnTypeCheckProduct() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    from Standard.Base import Integer, Text
    type Clazz
        Value a
    Clazz.from (that : Integer) = Clazz.Value that

    foo a -> (Integer | Text) & Clazz =
        a+a
    """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    var foo = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    assertEquals(20, foo.execute(10).asInt());

    try {
      var res = foo.execute(".");
      fail("Expecting an exception, not: " + res);
    } catch (PolyglotException e) {
      assertContains(
          "expected the result of `foo` to be (Integer | Text) & Clazz, but got Text",
          e.getMessage());
    }
  }

  static void assertTypeError(String expArg, String expType, String realType, String msg) {
    assertEquals(
        "Type error: expected " + expArg + " to be " + expType + ", but got " + realType + ".",
        msg);
  }

  private static void assertContains(String exp, String msg) {
    if (!msg.contains(exp)) {
      fail("Expecting " + msg + " to contain " + exp);
    }
  }
}
