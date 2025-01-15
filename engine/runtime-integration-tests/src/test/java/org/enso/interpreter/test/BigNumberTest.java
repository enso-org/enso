package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class BigNumberTest {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx = ContextUtils.createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }

  @Test
  public void evaluation() throws Exception {
    final String code =
        """
    import Standard.Base.Data.Vector.Builder
    from Standard.Base.Data.Vector import Vector

    powers n =
        go x v b = if x > n then b.to_vector else
            b.append v
            @Tail_Call go x+1 v*3 b
        go 1 1 Builder.new
    """;
    var powers = evalCode(code, "powers");

    var vec = powers.execute(200);
    assertTrue("Got an array", vec.hasArrayElements());
    assertEquals("Size 200", 200, vec.getArraySize());

    var longs = 0;
    var doubles = 0;
    var values = new ArrayList<BigInteger>();
    for (long i = 0; i < vec.getArraySize(); i++) {
      var e = vec.getArrayElement(i);
      assertTrue("All numbers are numbers, but " + e + " is not", e.isNumber());
      if (e.fitsInLong()) {
        longs++;
      }
      if (e.fitsInDouble()) {
        doubles++;
      }
      var s = e.toString();
      var b = e.asBigInteger();
      assertNotNull("Each Enso number can be parsed as big integer", b);
      assertEquals("Textual values are the same", s, b.toString());
      values.add(b);
    }
    assertEquals("There are few long values and rest of doubles", 40, longs);
    assertEquals("There are few double values and rest of Numbers", 34, doubles);
    assertEquals("Two hundred numbers collected", 200, values.size());
    for (int i = 1; i < values.size(); i++) {
      var prev = values.get(i - 1);
      var next = values.get(i);

      assertEquals("Each value is accurate", prev.multiply(BigInteger.valueOf(3)), next);
    }
  }

  private Value evalCode(final String code, final String methodName) throws URISyntaxException {
    final var testName = "test.enso";
    final URI testUri = new URI("memory://" + testName);
    final Source src = Source.newBuilder("enso", code, testName).uri(testUri).buildLiteral();
    var module = ctx.eval(src);
    var powers = module.invokeMember("eval_expression", methodName);
    return powers;
  }

  @Test
  public void averageOfMixedArrayOverNumber() throws Exception {
    var code =
        """
    import Standard.Base.Data.Vector.Builder
    from Standard.Base.Data.Vector import Vector
    polyglot java import org.enso.example.TestClass

    powers n =
            go x v b = if x > n then b.to_vector else
                b.append v
                @Tail_Call go x+1 v*2 b
            go 1 1 Builder.new

    avg n = TestClass.numberArrayAverage (powers n)
    """;
    var fn = evalCode(code, "avg");
    var avg = fn.execute(200);

    assertTrue("Got a number back " + avg, avg.isNumber());
    assertFalse("It's not a long", avg.fitsInLong());
    assertTrue("It's a big number", avg.fitsInBigInteger());
    assertEquals("It is big enough", Math.pow(2, 200) / 200, avg.asBigInteger().doubleValue(), 300);
  }

  @Test
  public void averageOfMixedArrayOverBigInteger() throws Exception {
    var code =
        """
    import Standard.Base.Data.Vector.Builder
    from Standard.Base.Data.Vector import Vector
    import Standard.Base.Data.Numbers
    polyglot java import org.enso.example.TestClass

    powers n =
            go x v b = if x > n then b.to_vector else
                b.append v
                @Tail_Call go x+1 v*2 b
            go 1 1 Builder.new

    avg n = TestClass.exactArrayAverage (powers n)
    """;
    var fn = evalCode(code, "avg");
    var avg = fn.execute(200);

    assertTrue("Got a number back " + avg, avg.isString());
    var actual = new BigInteger(avg.asString());
    var expect = BigInteger.TWO.pow(200).divide(BigInteger.valueOf(200));
    assertEquals("It is big enough", expect, actual);
  }

  private Value assertMul(Object a, Object b) throws URISyntaxException {
    var code = """
    import Standard.Base.Data.Numbers
    mul a b = a * b
    """;
    var fn = evalCode(code, "mul");
    return fn.execute(a, b);
  }

  @Test
  public void bigIntegerLong() throws Exception {
    var fourtyTwo = assertMul(new BigInteger("6"), 7);
    assertEquals(42, fourtyTwo.asInt());
  }

  @Test
  public void bigIntegerDouble() throws Exception {
    var fourtyTwo = assertMul(new BigInteger("6"), 7.0);
    assertEquals(42, fourtyTwo.asInt());
  }

  @Test
  public void bigIntegerBigInteger() throws Exception {
    var fourtyTwo = assertMul(new BigInteger("6"), new BigInteger("7"));
    assertEquals(42, fourtyTwo.asInt());
  }

  @Test
  public void longBigInteger() throws Exception {
    var fourtyTwo = assertMul(6L, new BigInteger("7"));
    assertEquals(42, fourtyTwo.asInt());
  }

  @Test
  public void doubleBigInteger() throws Exception {
    var fourtyTwo = assertMul(6.0, new BigInteger("7"));
    assertEquals(42, fourtyTwo.asInt());
  }

  @Test
  public void everyValueSmallerThanIntegerMaxVal_IsPrimitiveInt() {
    var almostMaxInt = Integer.toString(Integer.MAX_VALUE - 1);
    var intVal = ContextUtils.evalModule(ctx, "main = " + almostMaxInt);
    assertThat("Is a number", intVal.isNumber(), is(true));
    assertThat("Fits in int", intVal.fitsInInt(), is(true));
    assertThat("Fits in long", intVal.fitsInLong(), is(true));
    assertThat("Fits in double", intVal.fitsInDouble(), is(true));
    assertThat("Fits in big int", intVal.fitsInBigInteger(), is(true));
  }

  @Test
  public void everyValueSmallerThanLongMaxVal_IsPrimitiveLong() {
    var almostMaxLong = Long.toString(Long.MAX_VALUE - 1);
    var longVal = ContextUtils.evalModule(ctx, "main = " + almostMaxLong);
    assertThat("Is a number", longVal.isNumber(), is(true));
    assertThat("Does not fit in int", longVal.fitsInInt(), is(false));
    assertThat("Fits in long", longVal.fitsInLong(), is(true));
    // Does not fit in double, because it is not a power of 2 and therefore a precision would
    // be lost if converted to double.
    assertThat("Does not fit in double", longVal.fitsInDouble(), is(false));
    assertThat("Fits in big int", longVal.fitsInBigInteger(), is(true));
  }

  @Test
  public void everyValueBiggerThanLongMaxVal_IsEnsoBigInt() {
    // This number is bigger than Long.MAX_VALUE, and not a power of 2.
    var bigIntVal = ContextUtils.evalModule(ctx, "main = 9223372036854775808");
    assertThat("Is a number", bigIntVal.isNumber(), is(true));
    assertThat("Does not fit in int", bigIntVal.fitsInInt(), is(false));
    assertThat("Does not fit in long", bigIntVal.fitsInLong(), is(false));
    assertThat("Does not fit in double (not a power of 2)", bigIntVal.fitsInDouble(), is(false));
    assertThat("Fits in big int", bigIntVal.fitsInBigInteger(), is(true));
  }
}
