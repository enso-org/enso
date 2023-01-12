package org.enso.interpreter.test.instrument;

import java.io.OutputStream;
import java.nio.file.Paths;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import static org.junit.Assert.assertNotNull;
import org.junit.BeforeClass;
import org.junit.Test;

public class VerifyJavaScriptIsAvailableTest {
  private static Context ctx;

  @BeforeClass
  public static void initEnsoContext() {
    ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(true)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .logHandler(OutputStream.nullOutputStream())
            .allowAllAccess(true)
            .build();
    assertNotNull("Enso language is supported", ctx.getEngine().getLanguages().get("enso"));
  }

  @AfterClass
  public static void closeEnsoContext() throws Exception {
    ctx.close();
  }

  @Test
  public void javaScriptIsPresent() {
    var js = ctx.getEngine().getLanguages().get("js");
    assertNotNull("JavaScript is available", js);
  }

  @Test
  public void ensoIsPresent() {
    var enso = ctx.getEngine().getLanguages().get("enso");
    assertNotNull("Enso is available", enso);
  }
}
