package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Runtime",
    name = "allow_output_in",
    description = "Allows output in the specified scope.")
public class AllowOutputInNode extends Node {
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();
  private @Child ExpectStringNode expectStringNode = ExpectStringNode.build();

  Object execute(State state, Object env_name, @Suspend Object action) {
    String envName = expectStringNode.execute(env_name);
    return thunkExecutorNode.executeThunk(
        action, state.withOutputAllowedIn(envName), BaseNode.TailStatus.NOT_TAIL);
  }
}
