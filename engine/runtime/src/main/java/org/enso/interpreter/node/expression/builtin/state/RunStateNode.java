package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "State",
    name = "run",
    description = "Runs a stateful computation in a local state environment.",
    autoRegister = false,
    inlineable = true)
@ReportPolymorphism
public abstract class RunStateNode extends Node {
  static RunStateNode build() {
    return RunStateNodeGen.create();
  }

  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  abstract Object execute(
      VirtualFrame frame, State state, Object key, Object local_state, @Suspend Object computation);

  @Specialization(guards = "objects.containsKey(data, key)")
  Object doExisting(
      VirtualFrame frame,
      State state,
      Type key,
      Object local,
      Object computation,
      @Bind("state.getContainer()") State.Container data,
      @Shared("dynamicObjectLib") @CachedLibrary(limit = "10") DynamicObjectLibrary objects) {
    var old = objects.getOrDefault(data, key, null);
    objects.put(data, key, local);
    try {
      return thunkExecutorNode.executeThunk(
          frame, computation, state, BaseNode.TailStatus.NOT_TAIL);
    } finally {
      objects.put(state.getContainer(), key, old);
    }
  }

  @Specialization(guards = "!objects.containsKey(data, key)")
  Object doFresh(
      VirtualFrame frame,
      State state,
      Type key,
      Object local,
      Object computation,
      @Bind("state.getContainer()") State.Container data,
      @Shared("dynamicObjectLib") @CachedLibrary(limit = "10") DynamicObjectLibrary objects) {
    objects.put(data, key, local);
    try {
      return thunkExecutorNode.executeThunk(
          frame, computation, state, BaseNode.TailStatus.NOT_TAIL);
    } finally {
      objects.removeKey(data, key);
    }
  }

  @Fallback
  Object doNonType(State state, Object key, Object local, Object computation) {
    var b = EnsoContext.get(this).getBuiltins();
    var payload = b.error().makeUnsupportedArgumentsError(new Object[] {key}, "Use type as a key.");
    throw new PanicException(payload, this);
  }
}
