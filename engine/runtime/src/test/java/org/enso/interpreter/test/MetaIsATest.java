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

public class MetaIsATest {
  private Context ctx;
  private Value isACheck;

  @Before
  public void prepareCtx() throws Exception {
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

    final URI facUri = new URI("memory://choose.enso");
    final Source facSrc = Source.newBuilder("enso", """
    import Standard.Base.Meta

    check x y = Meta.is_a x y
    """, "check.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(facSrc);
    isACheck = module.invokeMember("eval_expression", "check");
    assertTrue("it is a function", isACheck.canExecute());
  }

  @Test
  public void checkNumbers() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    var typeNumber = g.typeNumber();
    for (var v : g.numbers()) {
      var r = isACheck.execute(v, typeNumber);
      assertTrue("Value " + v + " is a number", r.asBoolean());
    }
  }
}
