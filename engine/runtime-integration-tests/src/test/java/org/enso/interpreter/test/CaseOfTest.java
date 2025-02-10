package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;

import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class CaseOfTest {
  private static Context ctx;
  private static EnsoContext leak;

  @BeforeClass
  public static void initCtx() throws Exception {
    ctx = ContextUtils.createDefaultContext();
    leak = ContextUtils.leakContext(ctx);
  }

  @AfterClass
  public static void closeCtx() {
    ctx.close();
    ctx = null;
    leak = null;
  }

  @Test
  public void caseOfBoolean() {
    doCaseOfBoolean(true, false);
  }

  @Test
  public void caseOfInteropBoolean() {
    var t = new WrappedPrimitive(true);
    var f = new WrappedPrimitive(false);
    doCaseOfBoolean(t, f);
  }

  @Test
  public void caseOfMultiValueBoolean() {
    var n = EnsoMultiValue.NewNode.getUncached();

    var bAndT =
        new Type[] {leak.getBuiltins().bool().getType(), leak.getBuiltins().number().getInteger()};
    var t = n.newValue(bAndT, 2, 0, new Object[] {true, 300});
    var f = n.newValue(bAndT, 2, 0, new Object[] {false, 200});
    doCaseOfBoolean(t, f);
  }

  private void doCaseOfBoolean(Object t, Object f) {
    var code =
        """
                   from Standard.Base import True, False

                   choose v = case v of
                       True -> 1
                       False -> 2
                       _ -> 3
                   """;

    var choose = ContextUtils.evalModule(ctx, code, "choose.enso", "choose");

    var one = choose.execute(t);
    assertEquals("With " + t + " we should get 1", 1, one.asInt());
    var two = choose.execute(f);
    assertEquals("With " + f + " we should get 2", 2, two.asInt());
  }
}
