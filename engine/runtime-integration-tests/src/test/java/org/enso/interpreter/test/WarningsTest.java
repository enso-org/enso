package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.warning.AppendWarningNode;
import org.enso.interpreter.runtime.warning.Warning;
import org.enso.interpreter.runtime.warning.WithWarnings;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.hamcrest.core.AllOf;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

public class WarningsTest {

  private static Context ctx;
  private static ValuesGenerator generator;
  private static Value wrap;
  private static EnsoContext ensoContext;

  @BeforeClass
  public static void initEnsoContext() {
    ctx = ContextUtils.createDefaultContext();
    generator = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    ensoContext = ContextUtils.leakContext(ctx);
    var module =
        ctx.eval(
            "enso",
            """
    from Standard.Base import Warning

    wrap msg value = Warning.attach msg value
    """);
    wrap = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrap");
  }

  @AfterClass
  public static void disposeContext() {
    generator.dispose();
    ctx.close();
    ctx = null;
    ensoContext.shutdown();
    ensoContext = null;
  }

  @Test
  public void doubleWithWarningsWrap() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var warn1 = Warning.create(ensoContext, "w1", this);
          var warn2 = Warning.create(ensoContext, "w2", this);
          var value = 42L;

          var with1 =
              (WithWarnings) AppendWarningNode.getUncached().executeAppend(null, value, warn1);
          var with2 =
              (WithWarnings) AppendWarningNode.getUncached().executeAppend(null, with1, warn2);

          assertEquals(value, with1.getValue());
          assertEquals(value, with2.getValue());
          Assert.assertArrayEquals(new Object[] {warn1}, with1.getWarningsArray(false));
          Assert.assertArrayEquals(new Object[] {warn1, warn2}, with2.getWarningsArray(false));
          return null;
        });
  }

  @Test
  public void wrapAndUnwrap() {
    var value = 42;
    Object without;
    try {
      without = AppendWarningNode.getUncached().executeAppend(null, 42, new Warning[0]);
    } catch (AssertionError e) {
      // OK
      return;
    }
    fail("One shall not be created WithWarnings without any warnings " + without);
  }

  @Test
  public void warningIsAnException() {
    var warning42 = wrap.execute("warn:1", 42);
    var warningHi = wrap.execute("warn:2", "Hi");

    assertTrue("value is a number", warning42.isNumber());
    assertTrue("value is Int", warning42.fitsInInt());
    assertEquals(42, warning42.asInt());

    assertTrue("value2 is a text", warningHi.isString());
    assertTrue("value2 not a number", warning42.isNumber());
    assertEquals("Hi", warningHi.asString());

    assertTrue("value1 with warning is also an exception", warning42.isException());
    assertTrue("value2 with warning is also an exception", warningHi.isException());

    try {
      warning42.throwException();
      fail("Shouldn't reach here");
    } catch (PolyglotException ex) {
      assertEquals("warn:1", ex.getMessage());
    }

    var warningMulti = wrap.execute("warn:3", warning42);
    assertTrue("multi value is a number", warningMulti.isNumber());
    assertTrue("multi value is Int", warningMulti.fitsInInt());
    assertEquals(42, warningMulti.asInt());

    assertTrue("multi vlaue with warning is also an exception", warningMulti.isException());

    try {
      warningMulti.throwException();
      fail("Shouldn't reach here");
    } catch (PolyglotException ex) {
      assertThat(ex.getMessage(), AllOf.allOf(containsString("warn:1"), containsString("warn:3")));
    }
  }

  @Test
  public void allWarningsAreExceptions() throws Exception {
    for (var v : generator.allValues()) {
      if (v.isNull() || v.isBoolean()) {
        continue;
      }
      assertWarningsForAType(v);
    }
  }

  private void assertWarningsForAType(Value v) {
    var type = v.getMetaObject();

    var warning1 = wrap.execute("warn:once", v);
    var warning2 = wrap.execute("warn:twice", warning1);

    var warningType = warning2.getMetaObject();
    assertEquals("Types without and with warnings are the same", type, warningType);
    assertTrue("It is an exception. Type: " + type, warning2.isException());
    try {
      throw warning2.throwException();
    } catch (PolyglotException ex) {
      if (ex.getMessage() == null) {
        assertEquals(generator.typeError(), type);
        assertEquals(generator.typeError(), warningType);
      } else {
        try {
          assertThat(
              "Warning found for " + type,
              ex.getMessage(),
              AllOf.allOf(containsString("warn:once"), containsString("warn:twice")));
        } catch (AssertionError err) {
          if (type != null && v.equals(warning1) && v.equals(warning2)) {
            assertEquals(
                "Cannot attach warnings to Error - check it is an error",
                "Standard.Base.Error.Error",
                type.getMetaQualifiedName());
            return;
          }
          throw err;
        }
      }
    }
  }

  @Test
  public void warningOnAnError() throws Exception {
    var code =
        """
    from Standard.Base import Integer, Warning, Error
    import Standard.Base.Errors.Illegal_Argument.Illegal_Argument
    from Standard.Base.Errors.Common import Out_Of_Range

    err_warn -> Integer ! Illegal_Argument =
        v = Warning.attach (Out_Of_Range.Error "qewr") 12
        case v of
            _ : Integer -> Error.throw (Illegal_Argument.Error "asdf")
    """;

    var module = ctx.eval(LanguageInfo.ID, code);
    var errorWithWarning = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "err_warn");
    assertFalse("Something is returned", errorWithWarning.isNull());
    assertTrue("But it represents an exception object", errorWithWarning.isException());
    assertEquals(
        "Standard.Base.Error.Error", errorWithWarning.getMetaObject().getMetaQualifiedName());
  }
}
