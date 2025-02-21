package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.PanicException;

/** Use this node to manipulate {@link State}. */
@BuiltinMethod(
    type = "State",
    name = "put",
    description = "Updates the value of monadic state.",
    autoRegister = false)
@ReportPolymorphism
public abstract class PutStateNode extends Node {
  public static PutStateNode build() {
    return PutStateNodeGen.create();
  }

  PutStateNode() {}

  final Object execute(Object key, Object newState) {
    return executePut(key, newState);
  }

  /**
   * Updates a value in the {@link State}. The node never defines new state key!
   *
   * @param key the key in the state as defined by the {@link RunStateNode#execute}
   * @param newState new value to associate with the key
   * @return the {@code newState} value
   * @throws {@link PanicException} if the key hasn't been defined in the {@link State} yet
   */
  public abstract Object executePut(Object key, Object newState);

  final State state() {
    return EnsoContext.get(this).currentState();
  }

  @Specialization(guards = "objects.containsKey(data, key)")
  Object doPut(
      Object key,
      Object new_state,
      @Bind("state().getContainer()") State.Container data,
      @CachedLibrary(limit = "10") DynamicObjectLibrary objects) {
    objects.put(data, key, new_state);
    return new_state;
  }

  @Fallback
  Object doMissing(Object key, Object new_state) {
    throw new PanicException(
        EnsoContext.get(this).getBuiltins().error().makeUninitializedStateError(key), this);
  }
}
