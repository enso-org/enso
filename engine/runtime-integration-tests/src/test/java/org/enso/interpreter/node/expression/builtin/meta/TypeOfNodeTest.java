package org.enso.interpreter.node.expression.builtin.meta;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.ArrayList;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.test.ValuesGenerator;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class TypeOfNodeTest {
  @ClassRule
  public static final ContextUtils ctxRule = ContextUtils.newBuilder().assertGC(false).build();

  @Parameterized.Parameter(0)
  public Object value;

  @Parameterized.Parameter(1)
  public String type;

  @Parameterized.Parameters
  public static Object[][] allPossibleEnsoInterpreterValues() throws Exception {
    ctxRule.context().enter();
    var g = ValuesGenerator.create(ctxRule);
    var typeOf =
        ctxRule.evalModule(
            """
            from Standard.Base import all

            typ obj = Meta.type_of obj
            main = typ
            """);
    var data = new ArrayList<Object[]>();
    for (var v : g.allValues()) {
      var t = typeOf.execute(v);
      if (!v.isNull()) {
        assertTrue("Type of " + v + " is " + t, t.isMetaObject());
        var n = t.getMetaSimpleName();
        var raw = ctxRule.unwrapValue(v);
        data.add(new Object[] {raw, n});
      }
    }
    data.add(new Object[] {UnresolvedSymbol.build("unknown_name", null), "Function"});
    data.add(new Object[] {UnresolvedConstructor.build(null, "Unknown_Name"), "Function"});
    ctxRule.context().leave();
    return data.toArray(new Object[0][]);
  }

  @Test
  public void typeOfCheck() {
    assertType(value, type, false);
  }

  @Test
  public void typeOfCheckAfterPriming() {
    assertType(value, type, true);
  }

  private static void assertType(Object symbol, String expectedTypeName, boolean withPriming) {
    var node = TypeOfNode.create();
    var root =
        new TestRootNode(
            (frame) -> {
              var arg = frame.getArguments()[0];
              var typeOrNull = node.findTypeOrNull(arg);
              var typeOrError = node.findTypeOrError(arg);
              if (typeOrNull == null) {
                if (typeOrError instanceof EnsoObject) {
                  assertTrue("Expecting error for " + arg, typeOrError instanceof DataflowError);
                } else {
                  // probably HostMetaObject
                }
              } else {
                assertEquals("Types should be the same for " + arg, typeOrNull, typeOrError);
              }
              return typeOrError;
            });
    root.insertChildren(node);
    var call = root.getCallTarget();

    if (withPriming) {
      class ForeignObject implements TruffleObject {}
      var foreignType = call.call(new ForeignObject());
      assertTrue("Empty foreign is unknown: " + foreignType, foreignType instanceof DataflowError);
    }
    var symbolType = call.call(symbol);
    var symbolTypeValue = ctxRule.asValue(symbolType);
    assertTrue("It is meta object: " + symbolTypeValue, symbolTypeValue.isMetaObject());
    assertEquals(expectedTypeName, symbolTypeValue.getMetaSimpleName());
  }
}
