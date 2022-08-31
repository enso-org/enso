package org.enso.interpreter.test;

import java.net.URI;
import java.nio.file.Paths;
import java.util.Map;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.Test;

public class VectorTest {
  @Test
  public void evaluation() throws Exception {
    Engine eng = Engine.newBuilder()
      .allowExperimentalOptions(true)
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    Context ctx = Context.newBuilder()
      .engine(eng)
      .allowIO(true)
      .allowAllAccess(true)
      .build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));

    final URI facUri = new URI("memory://choose.enso");
    final Source facSrc = Source.newBuilder("enso", """
    import Standard.Base.Data.Vector

    choose x = case x of
        Vector.Vector_Data -> "is vector"
        _ -> "nothing"

    check = choose [1, 2, 3]
    """, "choose.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(facSrc);
    var res = module.invokeMember("eval_expression", "check");
    assertEquals("is vector", res.asString());
  }
}
