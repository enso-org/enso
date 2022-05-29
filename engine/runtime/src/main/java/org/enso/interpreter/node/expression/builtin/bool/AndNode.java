package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
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
    name = "&&",
    description = "Computes the logical conjunction of two booleans")
public abstract class AndNode extends Node {

  public static AndNode build() {
    return AndNodeGen.create();
  }

  abstract Stateful execute(@MonadicState Object state, boolean _this, @Suspend Object that);

  @Specialization(guards = {"!_this"})
  Stateful executeLhs(@MonadicState Object state, boolean _this, @Suspend Object that) {
    return new Stateful(state, false);
  }

  @Specialization(
      guards = {"_this"},
      rewriteOn = ClassCastException.class)
  Stateful executeBool(
      @MonadicState Object state,
      boolean _this,
      @Suspend Object that,
      @Cached ThunkExecutorNode rhsThunkExecutorNode) {
    Stateful result =
        rhsThunkExecutorNode.executeThunk(that, state, BaseNode.TailStatus.TAIL_DIRECT);
    if (result.getValue() instanceof Boolean) {
      return result;
    }
    throw new ClassCastException("expected Boolean");
  }

  @Specialization(
      guards = {"_this"},
      rewriteOn = ClassCastException.class)
  Stateful executeDataflowError(
      @MonadicState Object state,
      boolean _this,
      @Suspend Object that,
      @Cached ThunkExecutorNode rhsThunkExecutorNode) {
    Stateful result =
        rhsThunkExecutorNode.executeThunk(that, state, BaseNode.TailStatus.TAIL_DIRECT);
    if (result.getValue() instanceof DataflowError) {
      return result;
    }
    throw new ClassCastException("expected DataflowError");
  }

  @Specialization(guards = {"_this"})
  Stateful executePanic(
      @MonadicState Object state,
      boolean _this,
      @Suspend Object that,
      @Cached ThunkExecutorNode rhsThunkExecutorNode) {
    Stateful v = rhsThunkExecutorNode.executeThunk(that, state, BaseNode.TailStatus.TAIL_DIRECT);
    var typeError =
        Context.get(this).getBuiltins().error().makeTypeError("Boolean", v.getValue(), "bool");
    throw new PanicException(typeError, this);
  }
}
