package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.core.AllOf;
import org.junit.ClassRule;
import org.junit.Test;

public class IfThenElseTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void simpleIfThenElse() {
    var code =
        """
        check x = if x then "Yes" else "No"
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

    assertEquals("Yes", check.execute(true).asString());
    assertEquals("No", check.execute(false).asString());
  }

  @Test
  public void simpleIfThen() {
    var code =
        """
        check x = if x then "Yes"
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

    assertEquals("Yes", check.execute(true).asString());
    assertTrue("Expect Nothing", check.execute(false).isNull());
  }

  @Test
  public void variableDefinedInThen() {
    var code =
        """
        check x = if x then
            xt = x.to_text
            "Good:"+xt
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

    assertEquals("Good:True", check.execute(true).asString());
    assertTrue("Expect Nothing", check.execute(false).isNull());
  }

  @Test
  public void indexSubRange() throws Exception {
    var code =
        """
        check step first = case step of
          _ -> "Every " + step.to_display_text + (if first == 0 then "" else " from " + first.to_display_text)
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

    assertEquals("Every Prefix", check.execute("Prefix", 0).asString());
    assertEquals("Every Count from 1", check.execute("Count", 1).asString());
  }

  @Test
  public void variableDefinedInElse() {
    var code =
        """
        check x = if x then "OKeyish:"+x.to_text else
            xt = x.to_text
            "Bad:"+xt
        """;
    var check = ctxRule.getMethodFromModule(code, "check");

    assertEquals("OKeyish:True", check.execute(true).asString());
    assertEquals("Bad:False", check.execute(false).asString());
  }

  @Test
  public void variableUsedAfterTheBranch() {
    try {
      var code =
          """
          check x =
              res = if x then "OKeyish:"+x.to_text else
                  xt = x.to_text
                  "Bad:"+xt

              xt
          """;

      var check = ctxRule.getMethodFromModule(code, "check");
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
  public void variableInMultipleIfBranches() throws Exception {
    var code =
        """
        check x =
            if x then
                xt = "Yes"
            if x.not then
                xt = "No"
            "Hooo"
        """;
    var check = ctxRule.getMethodFromModule(code, "check");

    assertEquals("Hooo", check.execute(true).asString());
    assertEquals("Hooo", check.execute(false).asString());
  }

  @Test
  public void variableNotVisibleAfterBranches() throws Exception {
    var code =
        """
        check x =
            if x then
                xt = "Yes"
            if x.not then
                xt = "No"
            xt
        """;
    try {
      var check = ctxRule.getMethodFromModule(code, "check");
      var res = check.execute(true);
      fail("The code should not compile, but returned: " + res);
    } catch (PolyglotException ex) {
      MatcherAssert.assertThat(
          ex.getMessage(), Matchers.containsString("name `xt` could not be found"));
    }
  }

  @Test
  public void conditionMustBeBoolean() {
    var code =
        """
        check x = if x then "Yes" else "No"
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

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
  public void javaScriptBooleanIsSupported() {
    var code =
        """
        foreign js toBool txt = '''
          if (txt == "Ano") return true;
          if (txt == "Ne") return false;
          throw "What do you mean by: " + txt;

        check x = if toBool x then "Yes" else "No"
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

    assertEquals("Yes", check.execute("Ano").asString());
    assertEquals("No", check.execute("Ne").asString());
  }

  @Test
  public void truffleObjectConvertibleToBooleanIsSupported() {
    var code =
        """
        from Standard.Base import all

        check x = if x then "Yes" else "No"
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

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
  public void warningsAndIfThenElse() {
    var code =
        """
        from Standard.Base import all

        check x = if x then "Yes" else "No"
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

    var warnCode =
        """
        from Standard.Base import all

        warn w v = Warning.attach w v
        """;
    var warn = ctxRule.getMethodFromModule(warnCode, "warn");

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
  public void warningsInThenOrElse() {
    var code =
        """
        from Standard.Base import all

        check x y n = if x then y else n
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

    var warnCode =
        """
        from Standard.Base import all

        warn w v = Warning.attach w v
        """;
    var warn = ctxRule.getMethodFromModule(warnCode, "warn");

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
  public void warningsInCondAndThenOrElse() {
    var code =
        """
        from Standard.Base import all

        check x y n = if x then y else n
        """;

    var check = ctxRule.getMethodFromModule(code, "check");

    var warnCode =
        """
        from Standard.Base import all

        warn w v = Warning.attach w v
        """;
    var warn = ctxRule.getMethodFromModule(warnCode, "warn");

    var y = warn.execute("Good", "Yes");
    var n = warn.execute("Bad", "No");
    var t = warn.execute("Maybe", true);
    var f = warn.execute("Maybe not", false);

    var yes = check.execute(t, y, n);
    var no = check.execute(f, y, n);

    assertEquals("Yes", yes.asString());
    assertWarning("MaybeGood", yes);
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

  @Test
  public void dontOverrideVariablesFromOuterScope() throws Exception {
    var code =
        """
        type Hello
            World msg good

            join self init =
                if self.good then "Ciao" else
                    x = init
                    x + self.msg

        hello state =
            Hello.World "World" state . join "Hello "
        """;

    var hello = ctxRule.getMethodFromModule(code, "hello");

    assertEquals("Ciao", hello.execute(true).asString());
    assertEquals("Hello World", hello.execute(false).asString());
  }
}
