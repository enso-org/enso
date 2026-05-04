package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.state.ExecutionEnvironment;
import org.enso.interpreter.runtime.state.GetStateNode;
import org.enso.interpreter.runtime.state.PutStateNode;
import org.enso.interpreter.runtime.state.WithContextNode;

@BuiltinMethod(
    type = "Runtime",
    name = "with_context_builtin",
    description = "Enabled/disables a context in the specified scope.",
    inlineable = true)
final class RuntimeWithContextNode extends Node {
  private @Child GetStateNode getNode = GetStateNode.create();
  private @Child PutStateNode putNode = PutStateNode.create();
  private @Child WithContextNode withNode = WithContextNode.create();
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  Object execute(VirtualFrame frame, Atom context, boolean enable, @Suspend Object action) {
    var ctx = EnsoContext.get(this);
    var origEnv =
        getNode.forClass(ExecutionEnvironment.class, EnsoContext::getGlobalExecutionEnvironment);
    var newEnv = withNode.executeEnvironmentUpdate(origEnv, context, enable);
    try {
      putNode.executePut(ExecutionEnvironment.class, newEnv, true);
      var state = ctx.currentState();
      return thunkExecutorNode.executeThunk(frame, action, state, BaseNode.TailStatus.NOT_TAIL);
    } finally {
      putNode.executePut(ExecutionEnvironment.class, origEnv, false);
    }
  }
}
