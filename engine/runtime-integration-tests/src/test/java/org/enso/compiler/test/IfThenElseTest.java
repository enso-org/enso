package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.ByteArrayOutputStream;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.hamcrest.Matchers;
import org.hamcrest.core.AllOf;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
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

  @Ignore
  @Test
  public void truffleObjectConvertibleToBooleanIsSupported() throws Exception {
    var code =
        """
    from Standard.Base import all

    check x = if x then "Yes" else "No"
    """;

    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    var t = new BoolObject(true);
    var f = new BoolObject(false);

    assertEquals("Yes", check.execute(t).asString());
    assertEquals("No", check.execute(f).asString());
  }

  @ExportLibrary(InteropLibrary.class)
  static final class BoolObject implements TruffleObject {
    private final boolean value;

    public BoolObject(boolean value) {
      this.value = value;
    }

    @ExportMessage
    boolean isBoolean() {
      return true;
    }

    @ExportMessage
    boolean asBoolean() {
      return value;
    }
  }

  @Test
  public void warningsAndIfThenElse() throws Exception {
    var code =
        """
    from Standard.Base import all

    check x = if x then "Yes" else "No"
    """;

    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    var warnCode = """
    from Standard.Base import all

    warn w v = Warning.attach w v
    """;
    var warn = ContextUtils.getMethodFromModule(ctx, warnCode, "warn");

    var t = warn.execute("Maybe", true);
    var f = warn.execute("Maybe not", false);

    var yes = check.execute(t);
    var no = check.execute(f);

    assertEquals("Yes", yes.asString());
    assertWarning("Maybe", yes);
    assertEquals("No", no.asString());
    assertWarning("Maybe not", no);
  }

  @Test
  public void warningsInThenOrElse() throws Exception {
    var code = """
    from Standard.Base import all

    check x y n = if x then y else n
    """;

    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    var warnCode = """
    from Standard.Base import all

    warn w v = Warning.attach w v
    """;
    var warn = ContextUtils.getMethodFromModule(ctx, warnCode, "warn");

    var y = warn.execute("Good", "Yes");
    var n = warn.execute("Bad", "No");

    var yes = check.execute(true, y, n);
    var no = check.execute(false, y, n);

    assertEquals("Yes", yes.asString());
    assertWarning("Good", yes);
    assertEquals("No", no.asString());
    assertWarning("Bad", no);
  }

  @Test
  public void warningsInCondAndThenOrElse() throws Exception {
    var code = """
    from Standard.Base import all

    check x y n = if x then y else n
    """;

    var check = ContextUtils.getMethodFromModule(ctx, code, "check");

    var warnCode = """
    from Standard.Base import all

    warn w v = Warning.attach w v
    """;
    var warn = ContextUtils.getMethodFromModule(ctx, warnCode, "warn");

    var y = warn.execute("Good", "Yes");
    var n = warn.execute("Bad", "No");
    var t = warn.execute("Maybe", true);
    var f = warn.execute("Maybe not", false);

    var yes = check.execute(t, y, n);
    var no = check.execute(f, y, n);

    assertEquals("Yes", yes.asString());
    assertWarning("GoodMaybe", yes);
    assertEquals("No", no.asString());
    assertWarning("BadMaybe not", no);
  }

  private static void assertWarning(String txt, Value v) {
    assertTrue("Value " + v + " should be an exceptional", v.isException());
    try {
      throw v.throwException();
    } catch (PolyglotException ex) {
      assertEquals(txt, ex.getMessage());
    }
  }
}
