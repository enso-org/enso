package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Location;
import com.oracle.truffle.api.object.Property;
import com.oracle.truffle.api.object.Shape;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.data.SingletonState;
import org.enso.interpreter.runtime.data.SmallMap;
import org.enso.interpreter.runtime.state.Stateful;

import java.util.Arrays;

@BuiltinMethod(
    type = "State",
    name = "get",
    description = "Returns the current value of monadic state.")
@ReportPolymorphism
public abstract class GetStateNode extends Node {
  static GetStateNode build() {
    return GetStateNodeGen.create();
  }

  abstract Object execute(@MonadicState Object state, Object _this, Object key);

  @Specialization(guards = {"state.getKey() == key"})
  Object doSingleton(SingletonState state, Object _this, Object key) {
    return state.getValue();
  }

  @Specialization(guards = {"state.getKeys() == cachedKeys", "key == cachedKey"})
  Object doExecute(
      SmallMap state,
      Object _this,
      Object key,
      @Cached("key") Object cachedKey,
      @Cached(value = "state.getKeys()", dimensions = 1) Object[] cachedKeys,
//      @Cached("init(key, cachedKeys)") int init,
      @Cached("state.indexOf(key)") int idx) {
    return state.getValues()[idx];
  }

  public int init(Object key, Object[] keys) {
    //    System.out.println("KEY IS: " + key);
    //    System.out.println("KEYS ARE: " + keys + " @ " + Arrays.toString(keys));
    return 0;
  }

  @Specialization
  Stateful doFall(SmallMap state, Object _this, Object key) {
    //    System.out.println("KEY IS: " + key);
    throw new RuntimeException("That's unexpected...");
  }
}
