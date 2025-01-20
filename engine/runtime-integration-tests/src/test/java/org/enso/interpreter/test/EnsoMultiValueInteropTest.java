package org.enso.interpreter.test;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.util.ArrayList;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class EnsoMultiValueInteropTest {

  @Parameterized.Parameter(0)
  public Object value;

  private static Context ctx;

  private static Context ctx() {
    if (ctx == null) {
      ctx = ContextUtils.defaultContextBuilder().build();
    }
    return ctx;
  }

  @Parameterized.Parameters
  public static Object[][] allEnsoMultiValuePairs() throws Exception {
    var g = ValuesGenerator.create(ctx());
    var typeOf =
        ContextUtils.evalModule(
            ctx(),
            """
    from Standard.Base import all

    typ obj = Meta.type_of obj
    main = typ
    """);
    var data = new ArrayList<Object[]>();
    for (var v1 : g.allValues()) {
      for (var v2 : g.allValues()) {
        registerValue(g, typeOf, v1, v2, data);
      }
    }
    return data.toArray(new Object[0][]);
  }

  private static void registerValue(
      ValuesGenerator g, Value typeOf, Value v1, Value v2, ArrayList<Object[]> data) {
    var t1 = typeOf.execute(v1);
    var t2 = typeOf.execute(v2);
    if (!t1.isNull() && !t2.isNull()) {
      var rawT1 = ContextUtils.unwrapValue(ctx(), t1);
      var rawT2 = ContextUtils.unwrapValue(ctx(), t2);
      if (rawT1 instanceof Type typ1 && rawT2 instanceof Type typ2) {
        var r1 = ContextUtils.unwrapValue(ctx, v1);
        if (r1 instanceof EnsoMultiValue) {
          return;
        }
        var r2 = ContextUtils.unwrapValue(ctx, v2);
        if (r2 instanceof EnsoMultiValue) {
          return;
        }
        if (typ1 == typ2) {
          return;
        }
        var both =
            EnsoMultiValue.NewNode.getUncached().newValue(new Type[] {typ1, typ2}, 2, 0, r1, r2);
        data.add(new Object[] {both});
      }
    }
  }

  @AfterClass
  public static void disposeCtx() throws Exception {
    if (ctx != null) {
      ctx.close();
      ctx = null;
    }
  }

  @Test
  public void isStringDoesntFail() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          return InteropLibrary.getUncached().isString(value);
        });
  }
}
