package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.data.EmptyMap;
import org.enso.interpreter.runtime.state.data.SingletonMap;
import org.enso.interpreter.runtime.state.data.SmallMap;

@BuiltinMethod(
    type = "State",
    name = "get",
    description = "Returns the current value of monadic state.")
@ImportStatic(SmallMap.class)
@ReportPolymorphism
public abstract class GetStateNode extends Node {
  static GetStateNode build() {
    return GetStateNodeGen.create();
  }

  abstract Object execute(@MonadicState Object state, Object key);

  @Specialization(guards = {"state.getKey() == key"})
  Object doSingleton(SingletonMap state, Object key) {
    return state.getValue();
  }

  @Specialization(
      guards = {"state.getKeys() == cachedKeys", "key == cachedKey", "idx != NOT_FOUND"})
  Object doMultiCached(
      SmallMap state,
      Object key,
      @Cached("key") Object cachedKey,
      @Cached(value = "state.getKeys()", dimensions = 1) Object[] cachedKeys,
      @Cached("state.indexOf(key)") int idx) {
    return state.getValues()[idx];
  }

  @Specialization
  Object doMultiUncached(SmallMap state, Object key) {
    int idx = state.indexOf(key);
    if (idx == SmallMap.NOT_FOUND) {
      return DataflowError.withoutTrace(
          Context.get(this).getBuiltins().error().makeUninitializedStateError(key), this);
    } else {
      return state.getValues()[idx];
    }
  }

  @Specialization
  Object doEmpty(EmptyMap state, Object key) {
    return DataflowError.withoutTrace(
        Context.get(this).getBuiltins().error().makeUninitializedStateError(key), this);
  }

  @Specialization
  Object doSingletonError(SingletonMap state, Object key) {
    return DataflowError.withoutTrace(
        Context.get(this).getBuiltins().error().makeUninitializedStateError(key), this);
  }
}
