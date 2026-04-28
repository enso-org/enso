package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.state.WithContextNode;

@BuiltinMethod(
    type = "Runtime",
    name = "with_context_builtin",
    description = "Enabled/disables a context in the specified scope.",
    inlineable = true)
final class RuntimeWithContextNode extends Node {
  private @Child WithContextNode withNode = WithContextNode.create();
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  Object execute(VirtualFrame frame, Atom context, boolean enable, @Suspend Object action) {
    var ctx = EnsoContext.get(this);
    var origEng = ctx.getExecutionEnvironment();
    var newEnv = withNode.executeEnvironmentUpdate(origEng, context, enable);
    try {
      var state = ctx.currentState();
      ctx.setExecutionEnvironment(newEnv, false);
      return thunkExecutorNode.executeThunk(frame, action, state, BaseNode.TailStatus.NOT_TAIL);
    } finally {
      ctx.setExecutionEnvironment(origEng, false);
    }
  }
}
