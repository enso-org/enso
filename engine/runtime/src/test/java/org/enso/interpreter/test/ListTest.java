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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class ListTest {
  private Context ctx;
  private final int size = 100_000;
  private Value generator;
  private Value plusOne;
  private Value evenOnes;
  private Value taken;
  private Value init;
  private Value asVector;
  private Value asText;

  @Before
  public void prepareCtx() throws Exception {
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

    final String code = """
    from Standard.Base.Data.List.List import Cons, Nil

    init list = list.init

    taken list n = list.take_start n

    even_ones list = list.filter (x -> x % 2 == 0)

    plus_one list = list.map (x -> x + 1)

    as_vector list = list.to_vector

    as_text list = list.to_text

    generator n =
        go x v l = if x > n then l else
            @Tail_Call go x+1 v+1 (Cons v l)
        go 1 1 Nil
    """;

    generator = evalCode(code, "generator");
    plusOne = evalCode(code, "plus_one");
    evenOnes = evalCode(code, "even_ones");
    taken = evalCode(code, "taken");
    init = evalCode(code, "init");
    asVector = evalCode(code, "as_vector");
    asText = evalCode(code, "as_text");
  }

  @Test
  public void mapPlusOneAndIterate() throws Exception {
    var list = generator.execute(size);
    assertEquals("Size is OK", size, list.invokeMember("length").asInt());

    var values = new ArrayList<BigInteger>();
    {
      var it = plusOne.execute(list);

      for (long i = 0; i < size; i++) {
        var e = it.getMember("x");
        assertTrue("All numbers are numbers, but " + e + " is not", e.isNumber());
        var n = e.as(Number.class);
        assertNotNull("All numbers can be seen as java.lang.Number", n);
        var b = new BigInteger(n.toString());
        assertNotNull("Each Enso number can be parsed as big integer", b);
        assertEquals("Textual values are the same", n.toString(), b.toString());
        values.add(b);
        it = it.getMember("xs");
      }
    }
    assertEquals("Two hundred numbers collected", size, values.size());
    for (int i = 1; i < values.size(); i++) {
      var prev = values.get(i - 1);
      var next = values.get(i);

      assertEquals("Each value is accurate", next.add(BigInteger.ONE), prev);
    }
  }

  @Test
  public void filterEvenOnes() throws Exception {
    var list = generator.execute(size);
    assertLength("Generated", size, list);
    var even = evenOnes.execute(list);
    assertLength("Half the size", size / 2, even);
  }

  @Test
  public void takeHalf() throws Exception {
    var list = generator.execute(size);
    assertLength("Generated", size, list);
    var even = taken.execute(list, size / 2);
    assertLength("Half the size", size / 2, even);
  }

  @Test
  public void initAllButLast() throws Exception {
    var list = generator.execute(size);
    assertLength("Generated all", size, list);
    var shorterByOne = init.execute(list);
    assertLength("Last element is gone", size - 1, shorterByOne);
  }

  @Test
  public void toVector() throws Exception {
    var list = generator.execute(size);
    assertLength("Generated all", size, list);
    var vec = asVector.execute(list);
    assertEquals("It's vector", "Vector", vec.getMetaObject().getMetaSimpleName());
    assertTrue("And an array like object", vec.hasArrayElements());
    assertEquals("The same size remains", size, vec.getArraySize());
  }

  @Test
  public void toText() throws Exception {
    var list = generator.execute(size);
    assertLength("Generated all", size, list);
    var str = asText.execute(list);
    assertTrue("It is a string", str.isString());

    String s = str.asString();
    int open = 0;
    int close = 0;
    for (int i = 0; i < s.length(); i++) {
      switch (s.charAt(i)) {
        case '(' -> open++;
        case ')' -> close++;
      }
    }
    assertEquals("Correct number of opening braces", size, open);
    assertEquals("Correct number of closing braces", size, close);
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

  private void assertLength(String msg, long expected, Value list) {
    var actual = list.invokeMember("length");
    assertTrue("Size fits into number", actual.fitsInLong());
    assertEquals(msg, expected, actual.asLong());
  }
}
