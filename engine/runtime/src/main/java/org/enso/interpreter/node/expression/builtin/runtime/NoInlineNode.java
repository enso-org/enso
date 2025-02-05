package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;

@BuiltinMethod(
    type = "Runtime",
    name = "no_inline",
    description = "Runs its argument without the possibility of getting inlined.",
    autoRegister = false)
public class NoInlineNode extends Node {
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  Object execute(VirtualFrame frame, @Suspend Object action) {
    return executeImpl(frame.materialize(), action);
  }

  @CompilerDirectives.TruffleBoundary
  private Object executeImpl(MaterializedFrame frame, @Suspend Object action) {
    return thunkExecutorNode.executeThunk(frame, action, BaseNode.TailStatus.NOT_TAIL);
  }
}
