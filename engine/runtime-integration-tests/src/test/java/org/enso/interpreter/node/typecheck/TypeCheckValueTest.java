package org.enso.interpreter.node.typecheck;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.CallTarget;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.Test;

public class TypeCheckValueTest {
  private static Context ctx;

  private static Context ctx() {
    if (ctx == null) {
      ctx = ContextUtils.defaultContextBuilder().build();
    }
    return ctx;
  }

  @AfterClass
  public static void closeCtx() {
    if (ctx != null) {
      ctx.close();
    }
    ctx = null;
  }

  @Test
  public void avoidDoubleWrappingOfEnsoMultiValue() {
    var convert = allOfIntegerAndText();

    ContextUtils.executeInContext(
        ctx(),
        () -> {
          var builtins = ContextUtils.leakContext(ctx).getBuiltins();
          var hi = Text.create("Hi");
          var m1 =
              EnsoMultiValue.NewNode.getUncached()
                  .newValue(
                      new Type[] {builtins.text(), builtins.number().getInteger()}, 2, 0, hi, 42);
          assertEquals("Text & Integer", m1.toDisplayString(true));

          var res = convert.call(m1);
          assertTrue("Got multivalue again", res instanceof EnsoMultiValue);
          var emv = (EnsoMultiValue) res;

          assertEquals("Integer & Text", emv.toDisplayString(true));
          return null;
        });
  }

  private static CallTarget allOfIntegerAndText() {
    var call = new CallTarget[1];
    ContextUtils.executeInContext(
        ctx(),
        () -> {
          var builtins = ContextUtils.leakContext(ctx).getBuiltins();
          var intNode = TypeCheckValueNode.single("int", builtins.number().getInteger());
          var textNode = TypeCheckValueNode.single("text", builtins.text());
          var bothNode = TypeCheckValueNode.allOf("int&text", intNode, textNode);
          var root =
              new TestRootNode(
                  (frame) -> {
                    var arg = frame.getArguments()[0];
                    var res = bothNode.handleCheckOrConversion(frame, arg, null);
                    return res;
                  });
          root.insertChildren(bothNode);
          call[0] = root.getCallTarget();
          return null;
        });
    return call[0];
  }
}
