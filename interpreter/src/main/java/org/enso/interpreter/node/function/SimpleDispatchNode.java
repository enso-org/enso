package org.enso.interpreter.node.function;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.optimiser.TailCallException;

/**
 * Optimistic version of {@link DispatchNode} for the non tail call recursive case. Tries to just
 * call the function. If that turns out to be a tail call, it replaces itself with a {@link
 * LoopingDispatchNode}. Thanks to this design, the (much more common) case of calling a function in
 * a non-tail position does not force the overhead of loop.
 */
public class SimpleDispatchNode extends DispatchNode {
  @Child private CallNode callNode = CallNodeGen.create();

  @Override
  public Object executeDispatch(Object receiver, Object[] arguments) {
    try {
      return callNode.executeCall(receiver, arguments);
    } catch (TailCallException e) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      DispatchNode replacement = new LoopingDispatchNode();
      this.replace(replacement);
      return replacement.executeDispatch(e.getFunction(), e.getArguments());
    }
  }
}
