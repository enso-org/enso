package org.enso.interpreter.node.expression.builtin.resource;

import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.ManagedResource;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Managed_Resource",
    name = "with",
    description =
        "Applies the passed action to the underlying resource managed by the passed Managed_Resource object.")
public abstract class WithNode extends Node {

  private @Child InvokeCallableNode invokeCallableNode =
      InvokeCallableNode.build(
          new CallArgumentInfo[] {new CallArgumentInfo()},
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);

  static WithNode build() {
    return WithNodeGen.create();
  }

  abstract Stateful execute(
      @MonadicState Object state,
      VirtualFrame frame,
      Object _this,
      ManagedResource resource,
      Object action);

  @Specialization
  Stateful doWith(
      Object state,
      VirtualFrame frame,
      Object _this,
      ManagedResource resource,
      Object action,
      @CachedContext(Language.class) Context context) {
    context.getResourceManager().park(resource);
    try {
      return invokeCallableNode.execute(
          action, frame, state, new Object[] {resource.getResource()});
    } finally {
      context.getResourceManager().unpark(resource);
    }
  }
}
