package org.enso.interpreter.test;

import java.net.URI;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class VectorTest {
  private Context ctx;

  @Before
  public void prepareCtx() {
    Engine eng = Engine.newBuilder()
      .allowExperimentalOptions(true)
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

    choose x = case x of
        Vector -> "is vector module"
        Vector.Vector -> "is vector type"
        _ -> "nothing"

    check = choose [1, 2, 3]
    """, "choose.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(facSrc);
    var res = module.invokeMember("eval_expression", "check");
    assertEquals("is vector type", res.asString());
  }

  @Test
  public void passingVectorDirectlyIntoJava() throws Exception {
    final URI uri = new URI("memory://callback.enso");
    final Source src = Source.newBuilder("enso", """
    import Standard.Base.Data.Vector

    callback f = f.accept ([1, 2, 3].map +5)
    """, "callback.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    class ConsumeList implements Consumer<List<Long>> {
      boolean called;

      ConsumeList() {
      }

      @Override
      public void accept(List<Long> c) {
        assertEquals(3, c.size());
        assertEquals(6L, (long)c.get(0));
        assertEquals(7L, (long)c.get(1));
        assertEquals(8L, (long)c.get(2));
        called = true;
      }
    }
    var consumeList = new ConsumeList();

    var callback = module.invokeMember("eval_expression", "callback");
    var res = callback.execute(consumeList);
    assertTrue("No result", res.isNull());
    assertTrue("Callback called", consumeList.called);
  }
}
