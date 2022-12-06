package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.nio.file.Paths;
import java.util.Map;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import static org.junit.Assert.assertFalse;
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
  public void checkNumbersAreNumber() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    var typeNumber = g.typeNumber();
    for (var v : g.numbers()) {
      var r = isACheck.execute(v, typeNumber);
      assertTrue("Value " + v + " is a number", r.asBoolean());
    }
  }

  @Test
  public void checkNumbersAreAny() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    var typeAny = g.typeAny();
    for (var v : g.numbers()) {
      var r = isACheck.execute(v, typeAny);
      assertTrue("Value " + v + " is any", r.asBoolean());
    }
  }

  @Test
  public void checkNumbersAreNotText() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    for (var v : g.numbers()) {
      var r = isACheck.execute(v, g.typeText());
      assertFalse("Value " + v + " is not a string", r.asBoolean());
    }
  }

  @Test
  public void checkTextsAreText() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    for (var v : g.textual()) {
      var r = isACheck.execute(v, g.typeText());
      assertTrue("Value " + v + " is a string", r.asBoolean());
    }
  }

  @Test
  public void checkTextsAreAny() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    for (var v : g.textual()) {
      var r = isACheck.execute(v, g.typeAny());
      assertTrue("Value " + v + " is Any", r.asBoolean());
    }
  }

  @Test
  public void checkTextsAreNotNumbers() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    for (var v : g.textual()) {
      var r = isACheck.execute(v, g.typeNumber());
      assertFalse("Value " + v + " is not a number", r.asBoolean());
    }
  }
}
