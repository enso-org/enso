package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.util.List;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.interpreter.node.expression.foreign.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapGetNode;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNode;
import org.enso.interpreter.runtime.data.hash.HashMapSizeNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.warning.AppendWarningNode;
import org.enso.interpreter.runtime.warning.Warning;
import org.enso.interpreter.runtime.warning.WarningsLibrary;
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
  public void withWarningsDelegatesToMetaObject() {
    var warning42 = wrap.execute("warn:1", "Text");
    var meta = warning42.getMetaObject();
    assertThat(
        "Value (" + warning42 + ") wrapped in warning must have a meta object",
        meta,
        is(notNullValue()));
    assertThat(meta.toString(), containsString("Text"));
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

  @Test
  public void toDisplayText() throws Exception {
    var code =
        """
    from Standard.Base import Integer, Warning, Error, Text

    type My_Warning
        private Value msg

        to_display_text self -> Text = Error.throw "Don't call me!"
        to_text self -> Text = "My_Warning to_text: "+self.msg

    fn =
        Warning.attach (My_Warning.Value "ONE") 1
    """;

    var module = ctx.eval(LanguageInfo.ID, code);
    var ownWarning = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fn");

    assertTrue("Warning is seen as exception", ownWarning.isException());
    try {
      throw ownWarning.throwException();
    } catch (PolyglotException ex) {
      assertEquals("My_Warning to_text: ONE", ex.getMessage());
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

  @Test
  public void warningsArray_readViaInterop_shouldNotRemoveWarnings() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var warn1 = Warning.create(ensoContext, 1L, null);
          var warn2 = Warning.create(ensoContext, 2L, null);
          var arr = ArrayLikeHelpers.wrapEnsoObjects(warn1, warn2);
          var interop = InteropLibrary.getUncached();
          var warn1FromArr = interop.readArrayElement(arr, 0);
          assertThat(
              "warn1 and warn1FromArr should be the same reference",
              warn1,
              is(sameInstance(warn1FromArr)));
          var warn2FromArr = interop.readArrayElement(arr, 1);
          assertThat(
              "warn2 and warn2FromArr should be the same reference",
              warn2,
              is(sameInstance(warn2FromArr)));
          return null;
        });
  }

  @Test
  public void warningsArray_collectWarningsViaWarningsLibrary() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var appendWarnNode = AppendWarningNode.getUncached();
          var warnsLib = WarningsLibrary.getUncached();
          var hashMapSizeNode = HashMapSizeNode.getUncached();
          var hashMapGetNode = HashMapGetNode.getUncached();

          var warn1 = Warning.create(ensoContext, 1L, null);
          var warn2 = Warning.create(ensoContext, 2L, null);
          var warnsMap = createWarningsMap(List.of(warn1, warn2));
          var text1 = Text.create("1");
          var text2 = Text.create("2");
          var arr = ArrayLikeHelpers.wrapEnsoObjects(text1, text2);
          var arrWithWarns = appendWarnNode.executeAppend(null, arr, warnsMap);
          assertThat(warnsLib.hasWarnings(arrWithWarns), is(true));
          var gatheredWarns = warnsLib.getWarnings(arrWithWarns, false);
          assertThat("Hash size should be 2", hashMapSizeNode.execute(gatheredWarns), is(2L));
          var warn1FromMap =
              hashMapGetNode.execute(null, null, gatheredWarns, warn1.getSequenceId(), null);
          assertThat(
              "Original warning and warning gathered via WarningsLibrary should be the same object",
              warn1 == warn1FromMap,
              is(true));
          return null;
        });
  }

  @Test
  public void nothingWithWarn_IsNotRemovedByHostValueToEnsoNode() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var hostValueToEnsoNode = HostValueToEnsoNode.getUncached();
          var warn = Warning.create(ensoContext, ensoContext.getNothing(), null);
          var converted = hostValueToEnsoNode.execute(warn);
          assertThat(converted, is(sameInstance(warn)));
          return null;
        });
  }

  @Test
  public void nothingWithWarn_FromMapToArray() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var warn = Warning.create(ensoContext, ensoContext.getNothing(), null);
          var warnsMap = createWarningsMap(List.of(warn));
          var warns = Warning.fromMapToArray(warnsMap);
          assertThat(warns.length, is(1));
          return null;
        });
  }

  private EnsoHashMap createWarningsMap(List<Warning> warns) {
    var map = EnsoHashMap.empty();
    var mapInsertNode = HashMapInsertNode.getUncached();
    for (var warn : warns) {
      map = mapInsertNode.execute(null, map, warn.getSequenceId(), warn);
    }
    return map;
  }
}
