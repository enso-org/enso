package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Boolean",
    name = "if_then",
    description = "Performs the standard if-then control flow operation.")
public abstract class IfThenNode extends Node {
  private @Child ThunkExecutorNode leftThunkExecutorNode = ThunkExecutorNode.build();
  private final ConditionProfile condProfile = ConditionProfile.createCountingProfile();

  static IfThenNode build() {
    return IfThenNodeGen.create();
  }

  abstract Object execute(State state, boolean self, @Suspend Object if_true);

  @Specialization
  Object doExecute(State state, boolean self, Object if_true) {
    if (condProfile.profile(self)) {
      return leftThunkExecutorNode.executeThunk(if_true, state, BaseNode.TailStatus.TAIL_DIRECT);
    } else {
      return Context.get(this).getNothing();
    }
  }
}
