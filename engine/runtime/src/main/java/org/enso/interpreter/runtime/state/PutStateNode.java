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

  public static PutStateNode getUncached() {
    return PutStateNodeGen.getUncached();
  }

  PutStateNode() {}

  final Object execute(Object key, Object newState) {
    return executePut(key, newState);
  }

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
