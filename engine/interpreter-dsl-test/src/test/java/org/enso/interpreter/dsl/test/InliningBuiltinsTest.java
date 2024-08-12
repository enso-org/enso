package org.enso.interpreter.dsl.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.node.InlineableNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.test.utils.ContextUtils;
import org.junit.Test;

public class InliningBuiltinsTest {

  /**
   * @see InliningBuiltinsInNode#execute(long, long)
   */
  @Test
  public void executeWithoutVirtualFrame() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      ContextUtils.executeInContext(
          ctx,
          () -> {
            var fn = InliningBuiltinsInMethodGen.makeFunction(null);
            if (fn.getCallTarget().getRootNode() instanceof InlineableNode.Root root) {
              var call = root.createInlineableNode();
              var clazz = call.getClass();
              assertEquals("InlineableNode", clazz.getSuperclass().getSimpleName());
              assertEquals(
                  "org.enso.interpreter.node.InlineableNode$Root",
                  clazz.getEnclosingClass().getInterfaces()[0].getName());

              var res =
                  WithFrame.invoke(
                      (frame) -> {
                        return call.call(
                            frame,
                            Function.ArgumentsHelper.buildArguments(
                                null, null, new Object[] {null, 5L, 7L}));
                      });
              assertEquals(12L, res);
            } else {
              fail("It is inlineable: " + fn.getCallTarget().getRootNode());
            }
            return null;
          });
    }
  }

  /**
   * @see InliningBuiltinsOutNode#execute(com.oracle.truffle.api.frame.VirtualFrame, long, long)
   */
  @Test
  public void executeWithVirtualFrame() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      ContextUtils.executeInContext(
          ctx,
          () -> {
            var fn = InliningBuiltinsOutMethodGen.makeFunction(null);
            if (fn.getCallTarget().getRootNode() instanceof InlineableNode.Root root) {
              fail("The node isn't inlineable: " + fn.getCallTarget().getRootNode());
            } else {
              var call = DirectCallNode.create(fn.getCallTarget());
              var clazz = call.getClass().getSuperclass();
              assertEquals("com.oracle.truffle.api.nodes.DirectCallNode", clazz.getName());

              var res =
                  WithFrame.invoke(
                      (frame) -> {
                        return call.call(
                            Function.ArgumentsHelper.buildArguments(
                                null, null, new Object[] {null, 3L, 9L}));
                      });
              assertEquals(12L, res);
            }
            return null;
          });
    }
  }

  /**
   * @see InliningBuiltinsNeedsNode#execute(long, long)
   */
  @Test
  public void executeWhenNeedsVirtualFrame() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      ContextUtils.executeInContext(
          ctx,
          () -> {
            var fn = InliningBuiltinsNeedsMethodGen.makeFunction(null);
            if (fn.getCallTarget().getRootNode() instanceof InlineableNode.Root root) {
              fail("The node isn't inlineable: " + fn.getCallTarget().getRootNode());
            } else {
              var call = DirectCallNode.create(fn.getCallTarget());
              var clazz = call.getClass().getSuperclass();
              assertEquals("com.oracle.truffle.api.nodes.DirectCallNode", clazz.getName());

              var res =
                  WithFrame.invoke(
                      (frame) -> {
                        return call.call(
                            Function.ArgumentsHelper.buildArguments(
                                null, null, new Object[] {null, 3L, 9L}));
                      });
              assertEquals(12L, res);
            }
            return null;
          });
    }
  }

  /**
   * @see InliningBuiltinsNeedNotNode#execute(com.oracle.truffle.api.frame.VirtualFrame, long, long)
   */
  @Test
  public void executeWhenNeedNotVirtualFrame() {
    try (var ctx = ContextUtils.createDefaultContext()) {
      ContextUtils.executeInContext(
          ctx,
          () -> {
            var fn = InliningBuiltinsNeedNotMethodGen.makeFunction(null);
            if (fn.getCallTarget().getRootNode() instanceof InlineableNode.Root root) {
              var call = root.createInlineableNode();
              var clazz = call.getClass();
              assertEquals("InlineableNode", clazz.getSuperclass().getSimpleName());
              assertEquals(
                  "org.enso.interpreter.node.InlineableNode$Root",
                  clazz.getEnclosingClass().getInterfaces()[0].getName());

              var res =
                  WithFrame.invoke(
                      (frame) -> {
                        return call.call(
                            frame,
                            Function.ArgumentsHelper.buildArguments(
                                null, null, new Object[] {null, 5L, 7L}));
                      });
              assertEquals(12L, res);
            } else {
              fail("It is inlineable: " + fn.getCallTarget().getRootNode());
            }
            return null;
          });
    }
  }

  private static final class WithFrame<T> extends RootNode {
    private final java.util.function.Function<VirtualFrame, T> fn;

    private WithFrame(java.util.function.Function<VirtualFrame, T> fn) {
      super(null);
      this.fn = fn;
    }

    @Override
    public Object execute(VirtualFrame frame) {
      return fn.apply(frame);
    }

    @SuppressWarnings("unchecked")
    static <T> T invoke(java.util.function.Function<VirtualFrame, T> fn, Object... args) {
      return (T) new WithFrame<>(fn).getCallTarget().call(args);
    }
  }
}
