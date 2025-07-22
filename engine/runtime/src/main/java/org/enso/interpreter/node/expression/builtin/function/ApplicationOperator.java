package org.enso.interpreter.node.expression.builtin.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;

@BuiltinMethod(
    type = "Function",
    name = "<|",
    description = "Takes a function and an argument and applies the function to the argument.",
    inlineable = true)
public class ApplicationOperator extends Node {
  private @Child InvokeCallableNode invokeCallableNode;

  ApplicationOperator() {
    invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {new CallArgumentInfo()},
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.EXECUTE);
    invokeCallableNode.setTailStatus(BaseNode.TailStatus.TAIL_DIRECT);
  }

  Object execute(VirtualFrame frame, Object self, @Suspend Object argument) {
    return invokeCallableNode.execute(
        self, frame, EnsoContext.get(this).currentState(), new Object[] {argument});
  }
}
