package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.nio.file.Paths;
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
    import Standard.Base.Data.Vector

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
    for (long i = 0; i < vec.getArraySize(); i++) {
      var e = vec.getArrayElement(i);
      if (e.fitsInLong()) {
        longs++;
      }
      assertTrue("All numbers must fit into double, but " + e + " doesn't", e.fitsInDouble());
    }
    assertEquals("There are few long values and rest of doubles", 63, longs);
  }
}
