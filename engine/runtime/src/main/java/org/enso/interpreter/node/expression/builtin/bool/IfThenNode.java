package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Boolean",
    name = "if_then",
    alwaysDirect = false,
    description = "Performs the standard if-then control flow operation.")
public abstract class IfThenNode extends Node {
  private @Child ThunkExecutorNode leftThunkExecutorNode = ThunkExecutorNode.build();
  private final ConditionProfile condProfile = ConditionProfile.createCountingProfile();

  static IfThenNode build() {
    return IfThenNodeGen.create();
  }

  abstract Stateful execute(@MonadicState Object state, boolean _this, Thunk if_true);

  @Specialization
  Stateful doExecute(
      Object state, boolean _this, Thunk if_true, @CachedContext(Language.class) Context context) {
    if (condProfile.profile(_this)) {
      return leftThunkExecutorNode.executeThunk(if_true, state, true);
    } else {
      return new Stateful(state, context.getUnit().newInstance());
    }
  }
}
