package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.*;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.data.SmallMap;
import org.enso.interpreter.runtime.state.Stateful;

import java.util.Arrays;

@BuiltinMethod(type = "State", name = "put", description = "Updates the value of monadic state.")
@ImportStatic(SmallMap.class)
@ReportPolymorphism
public abstract class PutStateNode extends Node {
  static PutStateNode build() {
    return PutStateNodeGen.create();
  }

  abstract Stateful execute(
      @MonadicState SmallMap state, Object _this, Object key, Object new_state);

  @Specialization(
      guards = {"state.getKeys().length == cachedKeys.length", "index != NOT_FOUND", "key == cachedKey"},
      limit = "10")
  Stateful doExisting(
      SmallMap state,
      Object _this,
      Object key,
      Object new_state,
      @Cached("key") Object cachedKey,
      @Cached(value = "state.getKeys()", dimensions = 1) Object[] cachedKeys,
      @Cached("state.indexOf(key)") int index) {
    SmallMap changedState = state.set(index, new_state);
    return new Stateful(changedState, new_state);
  }

  @Specialization(
      guards = {"state.getKeys().length == cachedKeys.length", "key == cachedKey", "index == NOT_FOUND"},
      limit = "10")
  Stateful doNewLocation(
      SmallMap state,
      Object _this,
      Object key,
      Object new_state,
      @Cached("key") Object cachedKey,
      @Cached(value = "state.getKeys()", dimensions = 1) Object[] cachedKeys,
      @Cached("state.indexOf(key)") int index,
      @Cached(value = "state.getSchemaAfterInsert(key)", dimensions = 1) Object[] nextKeys, @Cached("init(cachedKeys, key)") int x) {
    SmallMap changedState = state.insert(nextKeys, new_state);
    return new Stateful(changedState, new_state);
  }

  int init(Object[] keys, Object key) {
//    System.out.println("Insert: " + key + " into " + keys + " @ " + Arrays.toString(keys) + " from " + this);
    return 0;
  }

  @Specialization
  Stateful doFall(SmallMap state, Object _this, Object key, Object new_state) {
    throw new RuntimeException("That's unexpected...");
  }
}
