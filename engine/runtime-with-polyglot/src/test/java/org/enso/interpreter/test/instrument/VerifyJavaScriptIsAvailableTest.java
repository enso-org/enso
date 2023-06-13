package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.nio.file.Paths;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class VerifyJavaScriptIsAvailableTest {
  private static Context ctx;
  private static MockLogHandler handler;

  @BeforeClass
  public static void initEnsoContext() {
    handler = new MockLogHandler();
    ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(true)
            .option(RuntimeOptions.PREINITIALIZE, "js")
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option("log.level", "FINE")
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
        handler.assertMessage(
            "epb.org.enso.interpreter.epb.EpbContext", "Done initializing language");
    assertEquals("js", args[0]);
    assertEquals(Boolean.TRUE, args[1]);
  }

  @Test
  public void javaScriptIsPresent() {
    var js = ctx.getEngine().getLanguages().get("js");
    assertNotNull("JavaScript is available", js);
    var fourtyTwo = ctx.eval("js", "6 * 7");
    assertEquals(42, fourtyTwo.asInt());
  }

  @Test
  public void ensoIsPresent() {
    var enso = ctx.getEngine().getLanguages().get("enso");
    assertNotNull("Enso is available", enso);
  }
}
