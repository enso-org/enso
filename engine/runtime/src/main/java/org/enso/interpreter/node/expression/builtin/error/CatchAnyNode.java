package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "Any",
    name = "catch_primitive",
    description =
        "If called on an error, executes the provided handler on the error's payload. Otherwise acts as identity.")
public class CatchAnyNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode;

  CatchAnyNode() {
    this.invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {new CallArgumentInfo()},
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
    this.invokeCallableNode.setTailStatus(BaseNode.TailStatus.TAIL_DIRECT);
  }

  Stateful execute(
      VirtualFrame frame, @MonadicState Object state, Object _this, Object handler) {
    return new Stateful(state, _this);
  }
}
