package org.enso.compiler;

import java.io.OutputStream;
import java.nio.file.Paths;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class ExecCompilerTest {
  private static Context ctx;

  @BeforeClass
  public static void initEnsoContext() {
    ctx = Context.newBuilder()
        .allowExperimentalOptions(true)
        .allowIO(true)
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        )
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
  public void testCaseOfWithNegativeConstant() throws Exception {
    var module = ctx.eval("enso", """
    run value =
        case value of
            -1 -> "minus one"
            _ -> "none"
    """);
    var run = module.invokeMember("eval_expression", "run");
    var minusOne = run.execute(-1);
    assertEquals("minus one", minusOne.asString());
    var none = run.execute(33);
    assertEquals("none", none.asString());
  }
}
