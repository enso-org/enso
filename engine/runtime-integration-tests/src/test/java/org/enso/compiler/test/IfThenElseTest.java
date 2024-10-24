package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
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
    ctx = ContextUtils.defaultContextBuilder().out(MESSAGES).err(MESSAGES).build();
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
    var code = """
    check x = if x then "Yes" else "No"
    """;

    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    assertEquals("Yes", check.execute(true).asString());
    assertEquals("No", check.execute(false).asString());
  }

  @Test
  public void simpleIfThen() throws Exception {
    var code = """
    check x = if x then "Yes"
    """;

    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    assertEquals("Yes", check.execute(true).asString());
    assertTrue("Expect Nothing", check.execute(false).isNull());
  }

  @Test
  public void variableDefinedInThen() throws Exception {
    var code = """
    check x = if x then
        xt = x.to_text
        "Good:"+xt
    """;

    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    assertEquals("Good:True", check.execute(true).asString());
    assertTrue("Expect Nothing", check.execute(false).isNull());
  }

  @Test
  public void variableDefinedInElse() throws Exception {
    var code =
        """
    check x = if x then "OKeyish:"+x.to_text else
        xt = x.to_text
        "Bad:"+xt
    """;
    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    assertEquals("OKeyish:True", check.execute(true).asString());
    assertEquals("Bad:False", check.execute(false).asString());
  }

  @Test
  public void variableUsedAfterTheBranch() throws Exception {
    try {
      var code =
          """
    check x =
        res = if x then "OKeyish:"+x.to_text else
            xt = x.to_text
            "Bad:"+xt

        xt
    """;

      var check = ContextUtils.getMethodFromModule(ctx, code, "check");
      fail("Expecting error, but got: " + check);
    } catch (PolyglotException ex) {
      assertThat(
          ex.getMessage(),
          AllOf.allOf(
              Matchers.containsString("The name `xt` could not be found"),
              Matchers.containsString("6:5: error")));
    }
  }

  @Test
  public void conditionMustBeBoolean() throws Exception {
    var code = """
    check x = if x then "Yes" else "No"
    """;

    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    try {
      var res = check.execute("Yes").asString();
      fail("Expecting error, not: " + res);
    } catch (PolyglotException ex) {
      assertThat(ex.getMessage(), Matchers.containsString(".Error"));
    }
    try {
      var res = check.execute((Object) null).asString();
      fail("Expecting error, not: " + res);
    } catch (PolyglotException ex) {
      assertThat(ex.getMessage(), Matchers.containsString(".Error"));
    }
  }

  @Test
  public void javaScriptBooleanIsSupported() throws Exception {
    var code =
        """
    foreign js toBool txt = '''
      if (txt == "Ano") return true;
      if (txt == "Ne") return false;
      throw "What do you mean by: " + txt;

    check x = if toBool x then "Yes" else "No"
    """;

    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    assertEquals("Yes", check.execute("Ano").asString());
    assertEquals("No", check.execute("Ne").asString());
  }
}
