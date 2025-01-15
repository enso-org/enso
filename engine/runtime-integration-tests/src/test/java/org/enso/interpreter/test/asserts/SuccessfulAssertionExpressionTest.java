package org.enso.interpreter.test.asserts;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.util.List;
import org.enso.common.LanguageInfo;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class SuccessfulAssertionExpressionTest {

  private static Context ctx;

  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @BeforeClass
  public static void setupCtx() {
    ctx =
        ContextUtils.defaultContextBuilder(LanguageInfo.ID)
            .environment("ENSO_ENABLE_ASSERTIONS", "true")
            .out(out)
            .err(out)
            .build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close(true);
    ctx = null;
  }

  @After
  public void resetOutput() {
    out.reset();
  }

  private static final String imports =
      """
from Standard.Base import all
polyglot java import java.lang.System as Java_System

eq_method x y =
    x == y
""";

  @Parameters(name = "{0}")
  public static List<String> succExpressions() {
    return List.of(
        "True",
        "1 == 1",
        "eq_method 23 23",
        "(Java_System.getenv '__NON_EXISTING_ENV_VAR__') == Nothing");
  }

  @Parameter public String succExpr;

  @Test
  public void assertTakesAnyExpression() {
    var sb = new StringBuilder();
    sb.append(imports).append("\n");
    sb.append("main = Runtime.assert (").append(succExpr).append(")\n");
    var code = sb.toString();
    var res = ContextUtils.evalModule(ctx, code);
    assertTrue(res.isNull());
  }
}
