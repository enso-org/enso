package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;

@BuiltinMethod(
    type = "State",
    name = "run",
    description = "Runs a stateful computation in a local state environment.")
public class RunStateNode extends Node {
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  Object execute(Object _this, Object local_state, Thunk computation) {
    return thunkExecutorNode.executeThunk(computation, local_state, false).getValue();
  }
}
