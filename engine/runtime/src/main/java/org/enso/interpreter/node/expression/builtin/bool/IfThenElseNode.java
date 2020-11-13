package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Boolean",
    name = "if_then_else",
    alwaysDirect = false,
    description = "Performs the standard if-then-else control flow operation.")
public class IfThenElseNode extends Node {
  private @Child ThunkExecutorNode leftThunkExecutorNode = ThunkExecutorNode.build();
  private @Child ThunkExecutorNode rightThunkExecutorNode = ThunkExecutorNode.build();
  private final ConditionProfile condProfile = ConditionProfile.createCountingProfile();

  Stateful execute(@MonadicState Object state, boolean _this, Thunk if_true, Thunk if_false) {
    if (condProfile.profile(_this)) {
      return leftThunkExecutorNode.executeThunk(if_true, state, true);
    } else {
      return rightThunkExecutorNode.executeThunk(if_false, state, true);
    }
  }
}
