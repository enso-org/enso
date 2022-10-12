package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "State",
    name = "run",
    description = "Runs a stateful computation in a local state environment.")
@ReportPolymorphism
public abstract class RunStateNode extends Node {
  static RunStateNode build() {
    return RunStateNodeGen.create();
  }

  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  abstract Object execute(
      @MonadicState State state, Object key, Object local_state, @Suspend Object computation);

  @Specialization
  Object doNothing(State state, Object key, Object local, Object computation) {
    return thunkExecutorNode.executeThunk(computation, state, BaseNode.TailStatus.NOT_TAIL);
  }
}
