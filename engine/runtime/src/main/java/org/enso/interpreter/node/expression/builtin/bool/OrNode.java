package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Boolean",
    name = "||",
    description = "Computes the logical disjunction of two booleans")
public class OrNode extends Node {
  private @Child ThunkExecutorNode rhsThunkExecutorNode = ThunkExecutorNode.build();
  private final ConditionProfile condProfile = ConditionProfile.createCountingProfile();

  Stateful execute(@MonadicState Object state,  boolean _this, @Suspend Object that) {
    if (condProfile.profile(_this)) {
      return new Stateful(state, true);
    } else {
      return ensureBooleanOrDataflowError(rhsThunkExecutorNode.executeThunk(that, state, BaseNode.TailStatus.TAIL_DIRECT));
    }
  }

  private Stateful ensureBooleanOrDataflowError(Stateful v) {
    if ((v.getValue() instanceof Boolean) || (v.getValue() instanceof DataflowError)) {
      return v;
    } else {
      var typeError = Context.get(this).getBuiltins().error().makeTypeError("Boolean", v.getValue(), "bool");
      throw new PanicException(typeError, this);
    }
  }
}
