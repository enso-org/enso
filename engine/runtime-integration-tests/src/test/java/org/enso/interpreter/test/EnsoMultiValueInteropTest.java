package org.enso.interpreter.test;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.util.ArrayList;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class EnsoMultiValueInteropTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  private Object value;

  public EnsoMultiValueInteropTest(Object[] data) {
    this.value = data;
    data[0] = null;
  }

  @After
  public void releaseValue() {
    value = null;
  }

  @Parameterized.Parameters
  public static Object[][] allEnsoMultiValuePairs() throws Exception {
    ctxRule.context().enter();
    var typeOf =
        ctxRule.evalModule(
            """
            from Standard.Base import all

            typ obj = Meta.type_of obj
            main = typ
            """);
    var data = new ArrayList<Object[]>();
    try (ValuesGenerator g = ValuesGenerator.create(ctxRule)) {
      for (var v1 : g.allValues()) {
        for (var v2 : g.allValues()) {
          registerValue(g, typeOf, v1, v2, data);
        }
      }
    }
    ctxRule.context().leave();
    return data.stream().map(v -> new Object[] {v}).toArray(Object[][]::new);
  }

  private static void registerValue(
      ValuesGenerator g, Value typeOf, Value v1, Value v2, ArrayList<Object[]> data) {
    var t1 = typeOf.execute(v1);
    var t2 = typeOf.execute(v2);
    if (!t1.isNull() && !t2.isNull()) {
      var rawT1 = ctxRule.unwrapValue(t1);
      var rawT2 = ctxRule.unwrapValue(t2);
      if (rawT1 instanceof Type typ1 && rawT2 instanceof Type typ2) {
        var r1 = ctxRule.unwrapValue(v1);
        if (r1 instanceof EnsoMultiValue) {
          return;
        }
        var r2 = ctxRule.unwrapValue(v2);
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

  @Test
  public void isStringDoesntFail() {
    InteropLibrary.getUncached().isString(value);
  }
}
