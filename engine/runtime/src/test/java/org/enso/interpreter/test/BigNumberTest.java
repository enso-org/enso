package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Map;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class BigNumberTest {
  private Context ctx;

  @Before
  public void prepareCtx() {
    this.ctx = Context.newBuilder()
      .allowExperimentalOptions(true)
      .allowIO(true)
      .allowAllAccess(true)
      .logHandler(new ByteArrayOutputStream())
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @Test
  public void evaluation() throws Exception {
    final String code = """
    from Standard.Base.Data.Vector import Vector

    powers n =
        go x v b = if x > n then b.to_vector else
            b.append v
            @Tail_Call go x+1 v*2 b
        go 1 1 Vector.new_builder
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
      boolean assertsOn = false;
      // Explanation at
      // https://github.com/enso-org/enso/pull/4074#discussion_r1086222800
      // rewrite when proper support for BigInteger is available
      // https://github.com/oracle/graal/pull/5490
      assert assertsOn = true;
      String s;
      if (!assertsOn) {
        var n = e.as(Number.class);
        assertNotNull("All numbers can be seen as java.lang.Number", n);
        s = n.toString();
      } else {
        s = e.toString();
      }
      var b = new BigInteger(s);
      assertNotNull("Each Enso number can be parsed as big integer", b);
      assertEquals("Textual values are the same", s, b.toString());
      values.add(b);
    }
    assertEquals("There are few long values and rest of doubles", 63, longs);
    assertEquals("There are few double values and rest of Numbers", 63, doubles);
    assertEquals("Two hundred numbers collected", 200, values.size());
    for (int i = 1; i < values.size(); i++) {
      var prev = values.get(i - 1);
      var next = values.get(i);

      assertEquals("Each value is accurate", prev.multiply(BigInteger.valueOf(2)), next);
    }
  }

  private Value evalCode(final String code, final String methodName) throws URISyntaxException {
    final var testName = "test.enso";
    final URI testUri = new URI("memory://" + testName);
    final Source src = Source.newBuilder("enso", code, testName)
            .uri(testUri)
            .buildLiteral();
    var module = ctx.eval(src);
    var powers = module.invokeMember("eval_expression", methodName);
    return powers;
  }

  @Test
  public void averageOfMixedArrayOverDouble() throws Exception {
    boolean assertsOn = false;
    assert assertsOn = true;
    if (assertsOn) {
      // skip this test when asserts are on
      return;
    }
    var code = """
    from Standard.Base.Data.Vector import Vector
    polyglot java import org.enso.example.TestClass

    powers n =
            go x v b = if x > n then b.to_vector else
                b.append v
                @Tail_Call go x+1 v*2 b
            go 1 1 Vector.new_builder

    avg n = TestClass.doubleArrayAverage (powers n)
    """;
    var fn = evalCode(code, "avg");
    var avg = fn.execute(200);

    assertTrue("Got a number back " + avg,avg.isNumber());
    assertFalse("It's not a long", avg.fitsInLong());
    assertTrue("It's a double", avg.fitsInDouble());
    assertEquals("It is big enough", Math.pow(2, 200) / 200, avg.asDouble(), 300);
  }

  @Test
  public void averageOfMixedArrayOverNumber() throws Exception {
    var code = """
    from Standard.Base.Data.Vector import Vector
    polyglot java import org.enso.example.TestClass

    powers n =
            go x v b = if x > n then b.to_vector else
                b.append v
                @Tail_Call go x+1 v*2 b
            go 1 1 Vector.new_builder

    avg n = TestClass.numberArrayAverage (powers n)
    """;
    var fn = evalCode(code, "avg");
    var avg = fn.execute(200);

    assertTrue("Got a number back " + avg,avg.isNumber());
    assertFalse("It's not a long", avg.fitsInLong());
    assertTrue("It's a double", avg.fitsInDouble());
    assertEquals("It is big enough", Math.pow(2, 200) / 200, avg.asDouble(), 300);
  }

  @Test
  public void averageOfMixedArrayOverBigInteger() throws Exception {
    var code = """
    from Standard.Base.Data.Vector import Vector
    polyglot java import org.enso.example.TestClass

    powers n =
            go x v b = if x > n then b.to_vector else
                b.append v
                @Tail_Call go x+1 v*2 b
            go 1 1 Vector.new_builder

    avg n = TestClass.exactArrayAverage (powers n)
    """;
    var fn = evalCode(code, "avg");
    var avg = fn.execute(200);

    assertTrue("Got a number back " + avg,avg.isString());
    var actual = new BigInteger(avg.asString());
    var expect = BigInteger.TWO.pow(200).divide(BigInteger.valueOf(200));
    assertEquals("It is big enough", expect, actual);
  }
}
