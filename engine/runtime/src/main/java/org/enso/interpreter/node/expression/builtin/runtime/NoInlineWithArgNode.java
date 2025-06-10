package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.BaseNode.TailStatus;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;

@BuiltinMethod(
    type = "Runtime",
    name = "no_inline_with_arg",
    description =
        "Runs its first argument applied to the second argument without the possibility of the call"
            + " or its argument getting inlined.",
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

  Object execute(VirtualFrame frame, Object action, Object argument) {
    MaterializedFrame materializedFrame = null;
    if (frame != null) {
      materializedFrame = frame.materialize();
    }
    return doInvoke(materializedFrame, action, argument);
  }

  @CompilerDirectives.TruffleBoundary
  Object doInvoke(MaterializedFrame frame, Object action, Object argument) {
    var ctx = EnsoContext.get(this);
    var state = ctx.currentState();
    return invokeCallableNode.execute(action, frame, state, new Object[] {argument});
  }
}
