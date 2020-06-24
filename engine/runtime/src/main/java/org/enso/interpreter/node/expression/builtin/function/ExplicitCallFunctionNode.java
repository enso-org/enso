package org.enso.interpreter.node.expression.builtin.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Function",
    name = "call",
    description = "Allows function calls to be made explicitly",
    alwaysDirect = false)
public class ExplicitCallFunctionNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode;

  ExplicitCallFunctionNode() {
    invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[0],
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
    invokeCallableNode.markTail();
  }

  Stateful execute(VirtualFrame frame, @MonadicState Object state, Function _this) {
    return invokeCallableNode.execute(_this, frame, state, new Object[0]);
  }
}
