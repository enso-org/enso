package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.BaseNode.TailStatus;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Runtime",
    name = "no_inline_with_arg",
    description =
        "Runs its first argument applied to the second argument without the possibility of the call or its argument getting inlined.",
    autoRegister = false)
public class NoInlineWithArgNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode;

  NoInlineWithArgNode() {
    invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {new CallArgumentInfo()},
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.EXECUTE);
    invokeCallableNode.setTailStatus(TailStatus.NOT_TAIL);
  }

  Object execute(VirtualFrame frame, State state, Object action, Object argument) {
    MaterializedFrame materializedFrame = null;
    if (frame != null) {
      materializedFrame = frame.materialize();
    }
    return doInvoke(materializedFrame, state, action, argument);
  }

  @CompilerDirectives.TruffleBoundary
  Object doInvoke(MaterializedFrame frame, State state, Object action, Object argument) {
    return invokeCallableNode.execute(action, frame, state, new Object[] {argument});
  }
}
