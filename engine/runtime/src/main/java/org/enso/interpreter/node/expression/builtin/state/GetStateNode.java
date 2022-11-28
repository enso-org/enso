package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "State",
    name = "get",
    description = "Returns the current value of monadic state.",
    autoRegister = false)
@ReportPolymorphism
public abstract class GetStateNode extends Node {
  static GetStateNode build() {
    return GetStateNodeGen.create();
  }

  abstract Object execute(State state, Object key);

  @Specialization(guards = "objects.containsKey(data, key)")
  Object doRead(
      State state,
      Object key,
      @Bind("state.getContainer()") State.Container data,
      @CachedLibrary(limit = "10") DynamicObjectLibrary objects) {
    return objects.getOrDefault(data, key, null);
  }

  @Fallback
  Object doMissing(State state, Object key) {
    throw new PanicException(
        Context.get(this).getBuiltins().error().makeUninitializedStateError(key), this);
  }
}
