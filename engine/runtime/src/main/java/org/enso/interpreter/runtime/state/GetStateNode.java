package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
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
    name = "get",
    description = "Returns the current value of monadic state.",
    autoRegister = false)
@ReportPolymorphism
@GenerateUncached
public abstract class GetStateNode extends Node {
  public static GetStateNode build() {
    return GetStateNodeGen.create();
  }

  public static GetStateNode getUncached() {
    return GetStateNodeGen.getUncached();
  }

  GetStateNode() {}

  final Object execute(Object key) {
    return executeGet(key);
  }

  /**
   * Reads value associated with a key from the {@link State}.
   *
   * @param key the key to read the value for
   * @return the value associated with the key
   * @throws {@link PanicException} when there is no such key in the {@link State}
   */
  public abstract Object executeGet(Object key);

  final State state() {
    return EnsoContext.get(this).currentState();
  }

  @Specialization(guards = "objects.containsKey(data, key)")
  Object doRead(
      Object key,
      @Bind("state().getContainer()") State.Container data,
      @CachedLibrary(limit = "10") DynamicObjectLibrary objects) {
    return objects.getOrDefault(data, key, null);
  }

  @Fallback
  Object doMissing(Object key) {
    throw new PanicException(
        EnsoContext.get(this).getBuiltins().error().makeUninitializedStateError(key), this);
  }
}
