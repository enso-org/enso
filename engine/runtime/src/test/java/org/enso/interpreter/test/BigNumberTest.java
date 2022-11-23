package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.net.URI;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyArray;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class BigNumberTest {
  private Context ctx;

  @Before
  public void prepareCtx() {
    Engine eng = Engine.newBuilder()
      .allowExperimentalOptions(true)
      .logHandler(new ByteArrayOutputStream())
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    this.ctx = Context.newBuilder()
      .engine(eng)
      .allowIO(true)
      .allowAllAccess(true)
      .build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @Test
  public void evaluation() throws Exception {
    final URI facUri = new URI("memory://choose.enso");
    final Source facSrc = Source.newBuilder("enso", """
    from Standard.Base.Data.Vector import Vector

    powers n =
        go x v b = if x > n then b.to_vector else
            b.append v
            @Tail_Call go x+1 v*2 b
        go 1 1 Vector.new_builder
    """, "powers.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(facSrc);
    var powers = module.invokeMember("eval_expression", "powers");
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
      var n = e.as(Number.class);
      assertNotNull("All numbers can be seend as java.lang.Number", n);
      var b = new BigInteger(n.toString());
      assertNotNull("Each Enso number can be parsed as big integer", b);
      assertEquals("Textual values are the same", n.toString(), b.toString());
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
}
