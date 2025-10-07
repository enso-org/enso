package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.library.CachedLibrary;
import java.util.UUID;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;

/**
 * Node responsible for <i>static method invocation</i>.
 * Static method invocation is a method call with specified {@code self} argument at first position.
 * Such invocation will not preapply the {@code self} argument, but will pass it directly to the method.
 * This is different to {@link InstanceInvokeMethodNode instance method invocation}.
 *
 * TODO: HTML link to GH
 * @see `docs/types/dynamic-dispatch.md`
 */
abstract class StaticInvokeMethodNode extends InvokeMethodNode {

  StaticInvokeMethodNode(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thisArgumentPosition,
      boolean onBoundary) {
    super(schema, defaultsExecutionMode, argumentsExecutionMode, thisArgumentPosition, onBoundary);
  }

  @Override
  public void setId(UUID id) {

  }

  @Specialization
  Object doInvoke(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Object self,
      Object[] arguments,
      @CachedLibrary(limit = "3") TypesLibrary typesLib) {
    if (self instanceof Type type) {
      type.get
    }
    return null;
  }
}
