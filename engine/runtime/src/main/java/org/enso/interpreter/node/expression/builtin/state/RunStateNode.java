package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
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

  abstract Object execute(State state, Object key, Object local_state, @Suspend Object computation);

  @Specialization(guards = "objects.containsKey(data, key)")
  Object doExisting(
      State state,
      Object key,
      Object local,
      Object computation,
      @Bind("state.getContainer()") State.Container data,
      @CachedLibrary(limit = "10") DynamicObjectLibrary objects) {
    var old = objects.getOrDefault(data, key, null);
    objects.put(data, key, local);
    try {
      return thunkExecutorNode.executeThunk(computation, state, BaseNode.TailStatus.NOT_TAIL);
    } finally {
      objects.put(state.getContainer(), key, old);
    }
  }

  @Specialization(guards = "!objects.containsKey(data, key)")
  Object doFresh(
      State state,
      Object key,
      Object local,
      Object computation,
      @Bind("state.getContainer()") State.Container data,
      @CachedLibrary(limit = "10") DynamicObjectLibrary objects) {
    objects.put(data, key, local);
    try {
      return thunkExecutorNode.executeThunk(computation, state, BaseNode.TailStatus.NOT_TAIL);
    } finally {
      objects.removeKey(data, key);
    }
  }
}
