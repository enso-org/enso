package org.enso.interpreter.node.callable.dispatch;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.ExecuteCallNodeGen;
import org.enso.interpreter.runtime.control.TailCallException;

/**
 * Optimistic version of {@link CallOptimiserNode} for the non tail call recursive case. Tries to
 * just call the function. If that turns out to be a tail call, it replaces itself with a {@link
 * LoopingCallOptimiserNode}. Thanks to this design, the (much more common) case of calling a
 * function in a non-tail position does not force the overhead of loop.
 */
public class SimpleCallOptimiserNode extends CallOptimiserNode {
  @Child private ExecuteCallNode executeCallNode = ExecuteCallNodeGen.create();

  /**
   * Calls the provided {@code callable} using the provided {@code arguments}.
   *
   * @param callable the callable to execute
   * @param arguments the arguments to {@code callable}
   * @return the result of executing {@code callable} using {@code arguments}
   */
  @Override
  public Object executeDispatch(Object callable, Object[] arguments) {
    try {
      return executeCallNode.executeCall(callable, arguments);
    } catch (TailCallException e) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      CallOptimiserNode replacement = new LoopingCallOptimiserNode();
      this.replace(replacement);
      return replacement.executeDispatch(e.getFunction(), e.getArguments());
    }
  }
}
