package org.enso.interpreter.node.expression.builtin.resource;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.ManagedResource;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Managed_Resource",
    name = "with_builtin",
    description =
        "Applies the passed action to the underlying resource managed by the passed"
            + " Managed_Resource object.")
public final class WithNode extends Node {
  private WithNode() {}

  private @Child InvokeCallableNode invokeCallableNode =
      InvokeCallableNode.build(
          new CallArgumentInfo[] {new CallArgumentInfo()},
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);

  static WithNode build() {
    return new WithNode();
  }

  Object execute(State state, VirtualFrame frame, ManagedResource mr, Object action) {
    var ctx = EnsoContext.get(this);
    var resourceManager = ctx.getResourceManager();
    if (mr.getPhantomReference().refersTo(mr)) {
      resourceManager.park(mr);
      try {
        return invokeCallableNode.execute(action, frame, state, new Object[] {mr.getResource()});
      } finally {
        resourceManager.unpark(mr);
      }
    } else {
      var payload = ctx.getBuiltins().error().makeUninitializedStateError(mr);
      var err = DataflowError.withDefaultTrace(payload, this);
      return invokeCallableNode.execute(action, frame, state, new Object[] {err});
    }
  }
}
