package org.enso.interpreter.test;

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

import static org.junit.Assert.assertEquals;

public class WarningsTest extends TestBase {

  private static Context ctx;

  @BeforeClass
  public static void initEnsoContext() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void disposeContext() {
    ctx.close();
  }

  @Test
  public void doubleWithWarningsWrap() {
    var ensoContext =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var warn1 = Warning.create(ensoContext, "w1", this);
    var warn2 = Warning.create(ensoContext, "w2", this);
    var value = 42;

    var with1 = WithWarnings.wrap(42, warn1);
    var with2 = WithWarnings.wrap(with1, warn2);

    assertEquals(value, with1.getValue());
    assertEquals(value, with2.getValue());
    Assert.assertArrayEquals(
        new Object[] {warn1}, with1.getWarningsArray(WarningsLibrary.getUncached()));
    Assert.assertArrayEquals(
        new Object[] {warn1, warn2}, with2.getWarningsArray(WarningsLibrary.getUncached()));
  }
}
