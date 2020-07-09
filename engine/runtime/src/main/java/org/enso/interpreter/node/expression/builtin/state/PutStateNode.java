package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.*;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.data.EmptyState;
import org.enso.interpreter.runtime.data.SingletonState;
import org.enso.interpreter.runtime.data.SmallMap;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

import java.util.Arrays;

@BuiltinMethod(type = "State", name = "put", description = "Updates the value of monadic state.")
@ImportStatic(SmallMap.class)
@ReportPolymorphism
public abstract class PutStateNode extends Node {
  static PutStateNode build() {
    return PutStateNodeGen.create();
  }

  abstract Stateful execute(@MonadicState Object state, Object _this, Object key, Object new_state);

  @Specialization(guards = "state.getKey() == cachedKey")
  Stateful doSameSingleton(
      SingletonState state,
      Object _this,
      Object key,
      Object new_state,
      @Cached("key") Object cachedKey) {
    return new Stateful(new SingletonState(cachedKey, new_state), new_state);
  }

  @Specialization(
      guards = {"state.getKeys() == cachedKeys", "index != NOT_FOUND", "key == cachedKey"},
      limit = "10")
  Stateful doExisting(
      SmallMap state,
      Object _this,
      Object key,
      Object new_state,
      @Cached("key") Object cachedKey,
      @Cached(value = "state.getKeys()", dimensions = 1) Object[] cachedKeys,
      @Cached("state.indexOf(key)") int index) {
//    SmallMap changedState = state.set(index, new_state);
    Object[] newVals = new Object[cachedKeys.length];
    System.arraycopy(state.getValues(), 0, newVals, 0, cachedKeys.length);
    newVals[index] = new_state;
    SmallMap newState = new SmallMap(cachedKeys, newVals);
    return new Stateful(newState, new_state);// changedState, new_state);
  }


  int init(Object[] keys, Object key) {
    //    System.out.println("Insert: " + key + " into " + keys + " @ " + Arrays.toString(keys) + "
    // from " + this);
    return 0;
  }

  @Specialization
  Stateful doError(Object state, Object _this, Object key, Object new_state) {
    throw new PanicException(
        "Cannot find state for key: " + key + ". Is State.run being used?", this);
  }
}
