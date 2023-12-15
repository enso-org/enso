package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.nio.file.Paths;
import java.util.logging.Level;
import org.enso.interpreter.test.MockLogHandler;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class VerifyLanguageAvailabilityTest {
  private static Context ctx;
  private static MockLogHandler handler;

  @BeforeClass
  public static void initEnsoContext() {
    handler = new MockLogHandler();
    ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .option(RuntimeOptions.PREINITIALIZE, "js")
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option(RuntimeOptions.LOG_LEVEL, Level.FINE.getName())
            .logHandler(handler)
            .allowAllAccess(true)
            .build();
    assertNotNull("Enso language is supported", ctx.getEngine().getLanguages().get("enso"));
    var fourtyTwo =
        ctx.eval("enso", "mul x y = x * y").invokeMember("eval_expression", "mul").execute(6, 7);
    assertEquals(42, fourtyTwo.asInt());
  }

  @AfterClass
  public static void closeEnsoContext() throws Exception {
    ctx.close();

    var args =
        handler.assertMessage("epb.org.enso.interpreter.epb.EpbContext", "Parsing foreign script");
    assertEquals("js", args[0]);
    assertEquals("mul.mul", args[1]);
  }

  @Test
  public void javaScriptIsPresent() throws Exception {
    var js = ctx.getEngine().getLanguages().get("js");
    assertNotNull("JavaScript is available", js);
    var src =
        Source.newBuilder(
                "enso",
                """
    foreign js mul a b = \"\"\"
        return a * b

    run = mul 6 7
    """,
                "mul.enso")
            .build();
    var fourtyTwo = ctx.eval(src).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "run");
    assertEquals(42, fourtyTwo.asInt());
  }

  @Test
  public void ensoIsPresent() {
    var enso = ctx.getEngine().getLanguages().get("enso");
    assertNotNull("Enso is available", enso);
  }
}
