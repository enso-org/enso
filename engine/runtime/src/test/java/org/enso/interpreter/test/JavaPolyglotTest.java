package org.enso.interpreter.test;

import java.nio.file.Paths;
import java.util.Map;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.junit.Assert;
import org.junit.Test;

public class JavaPolyglotTest {
  @Test
  public void evaluation() {
    Engine eng = Engine.newBuilder()
      .allowExperimentalOptions(true)
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    Context ctx = Context.newBuilder()
      .engine(eng)
      .allowIO(true)
      .build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    org.junit.Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));
    var module = ctx.eval("enso", """
    fac n =
        if n <= 1 then 1 else n * here.fac n-1
    """);
    var facFn = module.invokeMember("eval_expression", "here.fac");
    var fac5 = facFn.execute(5);
    Assert.assertEquals("5!", 120, fac5.asInt());
  }
}
