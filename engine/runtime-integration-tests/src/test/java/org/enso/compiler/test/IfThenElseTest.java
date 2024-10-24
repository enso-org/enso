package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.logging.Level;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.io.IOAccess;
import org.hamcrest.Matchers;
import org.hamcrest.core.AllOf;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class IfThenElseTest {
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

  @After
  public void cleanMessages() {
    MESSAGES.reset();
  }

  @AfterClass
  public static void closeEnsoContext() {
    ctx.close();
    ctx = null;
  }

  @Test
  public void simpleIfThenElse() throws Exception {
    var module = ctx.eval("enso", """
    check x = if x then "Yes" else "No"
    """);

    var check = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "check");

    assertEquals("Yes", check.execute(true).asString());
    assertEquals("No", check.execute(false).asString());
  }

  @Test
  public void simpleIfThen() throws Exception {
    var module = ctx.eval("enso", """
    check x = if x then "Yes"
    """);

    var check = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "check");

    assertEquals("Yes", check.execute(true).asString());
    assertTrue("Expect Nothing", check.execute(false).isNull());
  }

  @Test
  public void variableDefinedInThen() throws Exception {
    var module =
        ctx.eval(
            "enso", """
    check x = if x then
        xt = x.to_text
        "Good:"+xt
    """);

    var check = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "check");

    assertEquals("Good:True", check.execute(true).asString());
    assertTrue("Expect Nothing", check.execute(false).isNull());
  }

  @Test
  public void variableDefinedInElse() throws Exception {
    var module =
        ctx.eval(
            "enso",
            """
    check x = if x then "OKeyish:"+x.to_text else
        xt = x.to_text
        "Bad:"+xt
    """);

    var check = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "check");

    assertEquals("OKeyish:True", check.execute(true).asString());
    assertEquals("Bad:False", check.execute(false).asString());
  }

  @Test
  public void variableUsedAfterTheBranch() throws Exception {
    try {
      var module =
          ctx.eval(
              "enso",
              """
    check x =
        res = if x then "OKeyish:"+x.to_text else
            xt = x.to_text
            "Bad:"+xt

        xt
    """);

      var check = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "check");
      fail("Expecting error, but got: " + check);
    } catch (PolyglotException ex) {
      assertThat(
          ex.getMessage(),
          AllOf.allOf(
              Matchers.containsString("The name `xt` could not be found"),
              Matchers.containsString("6:5: error")));
    }
  }
}
