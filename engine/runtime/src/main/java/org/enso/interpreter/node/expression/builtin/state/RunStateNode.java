package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.data.EmptyState;
import org.enso.interpreter.runtime.data.SingletonState;
import org.enso.interpreter.runtime.data.SmallMap;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "State",
    name = "run",
    description = "Runs a stateful computation in a local state environment.")
@ReportPolymorphism
public abstract class RunStateNode extends Node {
  static RunStateNode build() {
    return RunStateNodeGen.create();
  }

  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  abstract Stateful execute(
      @MonadicState Object state, Object _this, Object key, Object local_state, Thunk computation);

  @Specialization
  Stateful doEmpty(
      EmptyState state, Object _this, Object key, Object local_state, Thunk computation) {
    SingletonState localStateMap = new SingletonState(key, local_state);
    Object result = thunkExecutorNode.executeThunk(computation, localStateMap, false).getValue();
    return new Stateful(state, result);
  }

  @Specialization(
      guards = {
        "key == cachedNewKey",
        "state.getKey() == cachedOldKey",
        "cachedOldKey != cachedNewKey"
      })
  Stateful doSingletonCached(
      SingletonState state,
      Object _this,
      Object key,
      Object local_state,
      Thunk computation,
      @Cached("key") Object cachedNewKey,
      @Cached("state.getKey()") Object cachedOldKey,
//      @Cached("init(state, key, cachedNewKey, cachedOldKey)") Object foo,
      @Cached(value = "buildSmallKeys(cachedNewKey, cachedOldKey)", dimensions = 1) Object[] newKeys) {
    SmallMap localStateMap = new SmallMap(newKeys, new Object[] {local_state, state.getValue()});
    Stateful res = thunkExecutorNode.executeThunk(computation, localStateMap, false);
//    SmallMap newStateMap = (SmallMap) res.getState();
    return new Stateful(local_state, res.getValue());
  }

  Object init(SingletonState st, Object key, Object newKey, Object oldKey) {
    System.out.println("INIT STATE");
    System.out.println(st.getKey());
    System.out.println(st.getValue());
    System.out.println(key);
    System.out.println(newKey);
    System.out.println(oldKey);
    return null;
  }

  Object[] buildSmallKeys(Object k1, Object k2) {
    return new Object[] {k1, k2};
  }
}
