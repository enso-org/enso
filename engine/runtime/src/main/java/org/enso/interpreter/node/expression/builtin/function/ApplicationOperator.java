package org.enso.interpreter.node.expression.builtin.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Function",
    name = "<|",
    description = "Allows function calls to be made explicitly",
    alwaysDirect = false)
public class ApplicationOperator extends Node {
  private @Child InvokeCallableNode invokeCallableNode;

  ApplicationOperator() {
    invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {new CallArgumentInfo()},
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.EXECUTE);
    invokeCallableNode.markTail();
  }

  Stateful execute(VirtualFrame frame, @MonadicState Object state, Function _this, Thunk argument) {
    return invokeCallableNode.execute(_this, frame, state, new Object[] {argument});
  }
}
