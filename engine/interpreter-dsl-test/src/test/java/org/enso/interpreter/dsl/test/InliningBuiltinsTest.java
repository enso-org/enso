package org.enso.interpreter.dsl.test;

import org.enso.interpreter.node.InlineableRootNode;
import org.enso.interpreter.runtime.callable.function.Function;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Test;

public class InliningBuiltinsTest {

  /** @see InliningBuiltinsInNode#execute(long, long) */
  @Test
  public void executeWithoutVirtualFrame() {
    var fn = InliningBuiltinsInMethodGen.makeFunction(null);
    if (fn.getCallTarget().getRootNode() instanceof InlineableRootNode root) {
      var call = root.createDirectCallNode();
      var clazz = call.getClass().getSuperclass();
      assertEquals("InlinedCallNode", clazz.getSimpleName());
      assertEquals("BuiltinRootNode", clazz.getEnclosingClass().getSimpleName());

      var res = call.call(Function.ArgumentsHelper.buildArguments(null, null, new Object[] { null, 5L, 7L }));
      assertEquals(12L, res);
    } else {
      fail("It is inlineable: " + fn.getCallTarget().getRootNode());
    }
  }

  /** @see InliningBuiltinsOutNode#execute(com.oracle.truffle.api.frame.VirtualFrame, long, long) */
  @Test
  public void executeWithVirtualFrame() {
    var fn = InliningBuiltinsOutMethodGen.makeFunction(null);
    if (fn.getCallTarget().getRootNode() instanceof InlineableRootNode root) {
      var call = root.createDirectCallNode();
      var clazz = call.getClass().getSuperclass();
      assertEquals("com.oracle.truffle.api.nodes.DirectCallNode", clazz.getName());

      var res = call.call(Function.ArgumentsHelper.buildArguments(null, null, new Object[] { null, 3L, 9L }));
      assertEquals(12L, res);
    } else {
      fail("It is inlineable: " + fn.getCallTarget().getRootNode());
    }
  }
}
