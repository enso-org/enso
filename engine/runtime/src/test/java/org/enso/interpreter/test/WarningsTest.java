package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

public class WarningsTest extends TestBase {

  private static Context ctx;
  private static EnsoContext ensoContext;

  @BeforeClass
  public static void initEnsoContext() {
    ctx = createDefaultContext();
    ensoContext =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
  }

  @AfterClass
  public static void disposeContext() {
    ensoContext = null;
    ctx.close();
  }

  @Test
  public void doubleWithWarningsWrap() {
    var warn1 = Warning.create(ensoContext, "w1", this);
    var warn2 = Warning.create(ensoContext, "w2", this);
    var value = 42;

    var with1 = WithWarnings.wrap(ensoContext, 42, warn1);
    var with2 = WithWarnings.wrap(ensoContext, with1, warn2);

    assertEquals(value, with1.getValue());
    assertEquals(value, with2.getValue());
    Assert.assertArrayEquals(
        new Object[] {warn1}, with1.getWarningsArray(WarningsLibrary.getUncached()));
    Assert.assertArrayEquals(
        new Object[] {warn1, warn2}, with2.getWarningsArray(WarningsLibrary.getUncached()));
  }

  @Test
  public void wrapAndUnwrap() {
    var value = 42;
    WithWarnings without;
    try {
      without = WithWarnings.wrap(ensoContext, 42, new Warning[0]);
    } catch (AssertionError e) {
      // OK
      return;
    }
    fail("One shall not be created WithWarnings without any warnings " + without);
  }
}
