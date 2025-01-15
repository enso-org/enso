package org.enso.interpreter.node.expression.builtin.meta;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.RootCallTarget;
import java.util.ArrayList;
import java.util.Arrays;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.interpreter.test.ValuesGenerator;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class TypeOfNodeMultiValueTest {

  private static RootCallTarget testTypesCall;

  @Parameterized.Parameter(0)
  public Object value;

  @Parameterized.Parameter(1)
  public String type;

  @Parameterized.Parameter(2)
  public int typeIndex;

  private static Context ctx;

  private static Context ctx() {
    if (ctx == null) {
      ctx = ContextUtils.defaultContextBuilder().build();
      ContextUtils.executeInContext(
          ctx,
          () -> {
            var node = TypeOfNode.create();
            var root =
                new TestRootNode(
                    (frame) -> {
                      var arg = frame.getArguments()[0];
                      var allTypes = (boolean) frame.getArguments()[1];
                      var t = node.findTypeOrError(arg);
                      var all = node.findAllTypesOrNull(arg, allTypes);
                      return new Object[] {t, all};
                    });
            root.insertChildren(node);
            testTypesCall = root.getCallTarget();
            return null;
          });
    }
    assertNotNull("Test types call initialized", testTypesCall);
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
    for (var polyValue : g.allValues()) {
      registerValue(g, typeOf, polyValue, data);
    }
    return data.toArray(new Object[0][]);
  }

  private static void registerValue(
      ValuesGenerator g, Value typeOf, Value polyValue, ArrayList<Object[]> data) {
    var t = typeOf.execute(polyValue);
    if (!polyValue.isNull()) {
      assertTrue("Type of " + polyValue + " is " + t, t.isMetaObject());
      var rawValue = ContextUtils.unwrapValue(ctx(), polyValue);
      if (rawValue instanceof EnsoMultiValue) {
        return;
      }
      var rawInt = (Type) ContextUtils.unwrapValue(ctx(), g.typeInteger());
      var rawType = ContextUtils.unwrapValue(ctx(), t);
      if (rawType instanceof Type type) {
        if (rawType == rawInt) {
          return;
        }
        var singleMultiValue =
            EnsoMultiValue.NewNode.getUncached()
                .newValue(new Type[] {type}, 1, 0, new Object[] {rawValue});
        var n = t.getMetaSimpleName();
        data.add(new Object[] {singleMultiValue, n, 0});
        var secondMultiValue =
            EnsoMultiValue.NewNode.getUncached()
                .newValue(new Type[] {rawInt, type}, 2, 0, new Object[] {5L, rawValue});
        data.add(new Object[] {secondMultiValue, n, 1});
        var firstMultiValue =
            EnsoMultiValue.NewNode.getUncached()
                .newValue(new Type[] {type, rawInt}, 2, 0, new Object[] {rawValue, 6L});
        data.add(new Object[] {firstMultiValue, n, 0});
      } else {
        if (!t.isHostObject()) {
          data.add(new Object[] {rawValue, null, -1});
        }
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
  public void typeOfCheckAllTypes() {
    assertType(value, type, typeIndex, true);
  }

  @Test
  public void typeOfCheckHasBeenCastToTypes() {
    assertType(value, type, typeIndex, false);
  }

  private static void assertType(
      Object value, String expectedTypeName, int typeIndex, boolean allTypes) {
    assertNotNull("Value " + value + " should have a type", expectedTypeName);
    ContextUtils.executeInContext(
        ctx(),
        () -> {
          var pairResult = (Object[]) testTypesCall.call(value, allTypes);
          var t = pairResult[0];
          var all = (Object[]) pairResult[1];

          Object symbolType;
          if (t instanceof DataflowError) {
            assertNull("No types for errors", all);
            symbolType = t;
          } else {
            assertNotNull("All types found for " + value, all);
            assertTrue(
                "Size is at least " + typeIndex + " but was: " + Arrays.toString(all),
                all.length >= typeIndex);
            assertEquals("Major type is the same with first of allTypes for" + value, t, all[0]);
            symbolType = all[typeIndex];
          }

          var symbolTypeValue = ctx.asValue(symbolType);
          assertTrue("It is meta object: " + symbolTypeValue, symbolTypeValue.isMetaObject());
          assertEquals(expectedTypeName, symbolTypeValue.getMetaSimpleName());
          return null;
        });
  }
}
