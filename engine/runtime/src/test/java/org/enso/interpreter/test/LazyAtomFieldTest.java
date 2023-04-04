package org.enso.interpreter.test;

import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Map;
import java.util.stream.Collectors;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.Before;
import org.junit.Test;

public class LazyAtomFieldTest {
  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();
  private Context ctx;

  @Before
  public void prepareCtx() {
    this.ctx = Context.newBuilder()
      .allowExperimentalOptions(true)
      .allowIO(true)
      .allowAllAccess(true)
      .logHandler(new ByteArrayOutputStream())
      .out(out)
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    out.reset();
  }

  @Test
  public void evaluation() throws Exception {
    final String code = """
    from Standard.Base import IO

    type Lazy
        LazyValue ~x ~y

        say self w = "Hello " + w.to_text

        meaning self =
            IO.println "Computing meaning"
            v = self.x * self.y
            IO.println "Computed meaning"
            v

    meanings =
        compute_x =
            IO.println "Computing x"
            v = 6
            IO.println "Computing x done"
            v

        compute_y =
            IO.println "Computing y"
            v = 7
            IO.println "Computing y done"
            v

        IO.println "Start"
        l = Lazy.LazyValue compute_x compute_y
        IO.println "Lazy value ready"
        IO.println <| l.say "World!"
        IO.println l.meaning
        IO.println <| l.say "Again!"
        IO.println l.meaning
        l.meaning
    """;
    var meanings = evalCode(code, "meanings");
    assertEquals(42, meanings.asInt());

    String log = out.toString(StandardCharsets.UTF_8);
    var lazyReadyAndThen = log.lines().dropWhile(l -> l.contains("Lazy value ready")).collect(Collectors.toList());
    var computingX = lazyReadyAndThen.stream().filter(l -> l.contains("Computing x done")).count();
    assertEquals(log, 1, computingX);
    var computingY = lazyReadyAndThen.stream().filter(l -> l.contains("Computing y done")).count();
    assertEquals(log, 1, computingY);
    var hellos = lazyReadyAndThen.stream().filter(l -> l.startsWith("Hello")).count();
    assertEquals(log, 2, hellos);
  }

  private Value evalCode(final String code, final String methodName) throws URISyntaxException {
    final var testName = "test.enso";
    final URI testUri = new URI("memory://" + testName);
    final Source src = Source.newBuilder("enso", code, testName)
            .uri(testUri)
            .buildLiteral();
    var module = ctx.eval(src);
    return module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, methodName);
  }
}
