package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.state.data.SingletonMap;
import org.enso.interpreter.runtime.state.data.SmallMap;

@BuiltinMethod(type = "State", name = "put", description = "Updates the value of monadic state.")
@ImportStatic(SmallMap.class)
@ReportPolymorphism
public abstract class PutStateNode extends Node {
  static PutStateNode build() {
    return PutStateNodeGen.create();
  }

  abstract Stateful execute(@MonadicState Object state, Object _this, Object key, Object new_state);

  @Specialization(guards = "state.getKey() == key")
  Stateful doExistingSingleton(SingletonMap state, Object _this, Object key, Object new_state) {
    return new Stateful(new SingletonMap(key, new_state), new_state);
  }

  @Specialization(
      guards = {"state.getKeys() == cachedKeys", "index != NOT_FOUND", "key == cachedKey"})
  Stateful doExistingMultiCached(
      SmallMap state,
      Object _this,
      Object key,
      Object new_state,
      @Cached("key") Object cachedKey,
      @Cached(value = "state.getKeys()", dimensions = 1) Object[] cachedKeys,
      @Cached("state.indexOf(key)") int index) {
    Object[] newVals = new Object[cachedKeys.length];
    System.arraycopy(state.getValues(), 0, newVals, 0, cachedKeys.length);
    newVals[index] = new_state;
    SmallMap newStateMap = new SmallMap(cachedKeys, newVals);
    return new Stateful(newStateMap, new_state);
  }

  @Specialization
  Stateful doMultiUncached(SmallMap state, Object _this, Object key, Object new_state) {
    int index = state.indexOf(key);
    if (index == SmallMap.NOT_FOUND) {
      return new Stateful(
          state,
          DataflowError.withoutTrace(
              Context.get(this).getBuiltins().error().makeUninitializedStateError(key), this));
    } else {
      return doExistingMultiCached(state, _this, key, new_state, key, state.getKeys(), index);
    }
  }

  @Specialization
  Stateful doError(Object state, Object _this, Object key, Object new_state) {
    return new Stateful(
        state,
        DataflowError.withoutTrace(
            Context.get(this).getBuiltins().error().makeUninitializedStateError(key), this));
  }
}
