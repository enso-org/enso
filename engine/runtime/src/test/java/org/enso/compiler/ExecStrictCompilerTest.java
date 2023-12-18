package org.enso.compiler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.logging.Level;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class ExecStrictCompilerTest {
  private static Context ctx;
  private static final ByteArrayOutputStream MESSAGES = new ByteArrayOutputStream();

  @BeforeClass
  public static void initEnsoContext() {
    ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
            .out(MESSAGES)
            .err(MESSAGES)
            .allowAllAccess(true)
            .build();
    assertNotNull("Enso language is supported", ctx.getEngine().getLanguages().get("enso"));
  }

  @Before
  public void cleanMessages() {
    MESSAGES.reset();
  }

  @AfterClass
  public static void closeEnsoContext() throws Exception {
    ctx.close();
  }

  @Test
  public void redefinedArgument() throws Exception {
    var module = ctx.eval("enso", """
    type My_Type
        Value a b c a
    """);
    try {
      var run = module.invokeMember("eval_expression", "My_Type.Value");
      fail("Expecting no returned value: " + run);
    } catch (PolyglotException ex) {
      assertEquals("Compilation aborted due to errors.", ex.getMessage());
      assertTrue("Syntax error", ex.isSyntaxError());
      assertTrue("Guest exception", ex.isGuestException());

      var errors = new String(MESSAGES.toByteArray(), StandardCharsets.UTF_8);
      assertNotEquals(
          "Errors reported in " + errors,
          -1,
          errors.indexOf("Redefining arguments is not supported"));
      assertNotEquals(
          "Identifier recognized in " + errors, -1, errors.indexOf("a is defined multiple times"));
    }
  }
}
