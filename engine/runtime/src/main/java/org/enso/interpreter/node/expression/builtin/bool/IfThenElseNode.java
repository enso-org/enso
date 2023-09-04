package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Boolean",
    name = "if_then_else",
    description = "Performs the standard if-then-else control flow operation.",
    inlineable = true)
public final class IfThenElseNode extends Node {
  private @Child ThunkExecutorNode leftThunkExecutorNode = ThunkExecutorNode.build();
  private @Child ThunkExecutorNode rightThunkExecutorNode = ThunkExecutorNode.build();
  private final CountingConditionProfile condProfile = CountingConditionProfile.create();

  public Object execute(
      VirtualFrame frame,
      State state,
      boolean self,
      @Suspend Object if_true,
      @Suspend Object if_false) {
    if (condProfile.profile(self)) {
      return leftThunkExecutorNode.executeThunk(
          frame, if_true, state, BaseNode.TailStatus.TAIL_DIRECT);
    } else {
      return rightThunkExecutorNode.executeThunk(
          frame, if_false, state, BaseNode.TailStatus.TAIL_DIRECT);
    }
  }
}
