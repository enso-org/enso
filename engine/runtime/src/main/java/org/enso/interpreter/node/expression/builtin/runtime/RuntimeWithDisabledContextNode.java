package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Runtime",
    name = "with_disabled_context_builtin",
    description = "Disallows context in the specified scope.",
    autoRegister = false)
public class RuntimeWithDisabledContextNode extends Node {
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();
  private @Child ExpectStringNode expectStringNode = ExpectStringNode.build();

  Object execute(State state, Atom context, @Suspend Object action, Object env_name) {
    String envName = expectStringNode.execute(env_name);
    return thunkExecutorNode.executeThunk(
        action, state.withContextDisabledIn(context, envName), BaseNode.TailStatus.NOT_TAIL);
  }
}
