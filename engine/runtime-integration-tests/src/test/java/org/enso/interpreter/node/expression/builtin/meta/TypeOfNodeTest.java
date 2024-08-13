package org.enso.interpreter.node.expression.builtin.meta;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.ArrayList;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.test.ValuesGenerator;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class TypeOfNodeTest {
  @Parameterized.Parameter(0)
  public Object value;

  @Parameterized.Parameter(1)
  public String type;

  private static Context ctx;

  private static Context ctx() {
    if (ctx == null) {
      ctx = ContextUtils.defaultContextBuilder().build();
    }
    return ctx;
  }

  @Parameterized.Parameters
  public static Object[][] allPossibleEnsoInterpreterValues() throws Exception {
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
    for (var v : g.allValues()) {
      var t = typeOf.execute(v);
      if (!v.isNull()) {
        assertTrue("Type of " + v + " is " + t, t.isMetaObject());
        var n = t.getMetaSimpleName();
        var raw = ContextUtils.unwrapValue(ctx(), v);
        data.add(new Object[] {raw, n});
      }
    }
    data.add(new Object[] {UnresolvedSymbol.build("unknown_name", null), "Function"});
    data.add(new Object[] {UnresolvedConstructor.build(null, "Unknown_Name"), "Function"});
    return data.toArray(new Object[0][]);
  }

  @AfterClass
  public static void disposeCtx() throws Exception {
    if (ctx != null) {
      ctx.close();
      ctx = null;
    }
  }

  @Test
  public void typeOfCheck() {
    assertType(value, type, false);
  }

  @Test
  public void typeOfCheckAfterPriming() {
    assertType(value, type, true);
  }

  private void assertType(Object symbol, String expectedTypeName, boolean withPriming) {
    ContextUtils.executeInContext(
        ctx(),
        () -> {
          var node = TypeOfNode.create();
          var root = new TestRootNode((frame) -> node.execute(frame.getArguments()[0]));
          root.insertChildren(node);
          var call = root.getCallTarget();

          if (withPriming) {
            class ForeignObject implements TruffleObject {}
            var foreignType = call.call(new ForeignObject());
            assertTrue(
                "Empty foreign is unknown: " + foreignType, foreignType instanceof DataflowError);
          }
          var symbolType = call.call(symbol);
          var symbolTypeValue = ctx.asValue(symbolType);
          assertTrue("It is meta object: " + symbolTypeValue, symbolTypeValue.isMetaObject());
          assertEquals(expectedTypeName, symbolTypeValue.getMetaSimpleName());
          return null;
        });
  }
}
