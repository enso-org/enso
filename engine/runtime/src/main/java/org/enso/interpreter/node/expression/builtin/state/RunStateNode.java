package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.state.data.EmptyMap;
import org.enso.interpreter.runtime.state.data.SingletonMap;
import org.enso.interpreter.runtime.state.data.SmallMap;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "State",
    name = "run",
    description = "Runs a stateful computation in a local state environment.")
@ReportPolymorphism
@ImportStatic(SmallMap.class)
public abstract class RunStateNode extends Node {
  static RunStateNode build() {
    return RunStateNodeGen.create();
  }

  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  abstract Stateful execute(
      @MonadicState Object state,
      Object _this,
      Object key,
      Object local_state,
      @Suspend Object computation);

  @Specialization
  Stateful doEmpty(
      EmptyMap state, Object _this, Object key, Object local_state, Object computation) {
    SingletonMap localStateMap = new SingletonMap(key, local_state);
    Object result =
        thunkExecutorNode
            .executeThunk(computation, localStateMap, BaseNode.TailStatus.NOT_TAIL)
            .getValue();
    return new Stateful(state, result);
  }

  @Specialization(guards = {"state.getKey() == key"})
  Stateful doSingletonSameKey(
      SingletonMap state, Object _this, Object key, Object local_state, Object computation) {
    SingletonMap localStateContainer = new SingletonMap(state.getKey(), local_state);
    Stateful res =
        thunkExecutorNode.executeThunk(
            computation, localStateContainer, BaseNode.TailStatus.NOT_TAIL);
    return new Stateful(state, res.getValue());
  }

  @Specialization(
      guards = {
        "key == cachedNewKey",
        "state.getKey() == cachedOldKey",
        "cachedOldKey != cachedNewKey"
      })
  Stateful doSingletonNewKeyCached(
      SingletonMap state,
      Object _this,
      Object key,
      Object local_state,
      Object computation,
      @Cached("key") Object cachedNewKey,
      @Cached("state.getKey()") Object cachedOldKey,
      @Cached(value = "buildSmallKeys(cachedNewKey, cachedOldKey)", dimensions = 1)
          Object[] newKeys) {
    SmallMap localStateMap = new SmallMap(newKeys, new Object[] {local_state, state.getValue()});
    Stateful res =
        thunkExecutorNode.executeThunk(computation, localStateMap, BaseNode.TailStatus.NOT_TAIL);
    Object newStateVal = ((SmallMap) res.getState()).getValues()[1];
    return new Stateful(new SingletonMap(cachedOldKey, newStateVal), res.getValue());
  }

  @Specialization
  Stateful doSingletonNewKeyUncached(
      SingletonMap state, Object _this, Object key, Object local_state, Object computation) {
    return doSingletonNewKeyCached(
        state,
        _this,
        key,
        local_state,
        computation,
        key,
        state.getKey(),
        buildSmallKeys(key, state.getKey()));
  }

  Object[] buildSmallKeys(Object k1, Object k2) {
    return new Object[] {k1, k2};
  }

  @Specialization(
      guards = {"key == cachedNewKey", "state.getKeys() == cachedOldKeys", "index == NOT_FOUND"})
  Stateful doMultiNewKeyCached(
      SmallMap state,
      Object _this,
      Object key,
      Object local_state,
      Object computation,
      @Cached("key") Object cachedNewKey,
      @Cached(value = "state.getKeys()", dimensions = 1) Object[] cachedOldKeys,
      @Cached("state.indexOf(key)") int index,
      @Cached(value = "buildNewKeys(cachedNewKey, cachedOldKeys)", dimensions = 1)
          Object[] newKeys) {
    Object[] newValues = new Object[newKeys.length];
    System.arraycopy(state.getValues(), 0, newValues, 1, cachedOldKeys.length);
    newValues[0] = local_state;
    SmallMap localStateMap = new SmallMap(newKeys, newValues);
    Stateful res =
        thunkExecutorNode.executeThunk(computation, localStateMap, BaseNode.TailStatus.NOT_TAIL);
    SmallMap resultStateMap = (SmallMap) res.getState();
    Object[] resultValues = new Object[cachedOldKeys.length];
    System.arraycopy(resultStateMap.getValues(), 1, resultValues, 0, cachedOldKeys.length);
    return new Stateful(new SmallMap(cachedOldKeys, resultValues), res.getValue());
  }

  @Specialization(
      guards = {"key == cachedNewKey", "state.getKeys() == cachedOldKeys", "index != NOT_FOUND"})
  Stateful doMultiExistingKeyCached(
      SmallMap state,
      Object _this,
      Object key,
      Object local_state,
      Object computation,
      @Cached("key") Object cachedNewKey,
      @Cached(value = "state.getKeys()", dimensions = 1) Object[] cachedOldKeys,
      @Cached("state.indexOf(key)") int index) {
    Object[] newValues = new Object[cachedOldKeys.length];
    System.arraycopy(state.getValues(), 0, newValues, 0, cachedOldKeys.length);
    newValues[index] = local_state;
    SmallMap localStateMap = new SmallMap(cachedOldKeys, newValues);
    Stateful res =
        thunkExecutorNode.executeThunk(computation, localStateMap, BaseNode.TailStatus.NOT_TAIL);
    SmallMap resultStateMap = (SmallMap) res.getState();
    Object[] resultValues = new Object[cachedOldKeys.length];
    System.arraycopy(resultStateMap.getValues(), 0, resultValues, 0, cachedOldKeys.length);
    resultValues[index] = state.getValues()[index];
    return new Stateful(new SmallMap(cachedOldKeys, resultValues), res.getValue());
  }

  @Specialization
  Stateful doMultiUncached(
      SmallMap state, Object _this, Object key, Object local_state, Object computation) {
    int idx = state.indexOf(key);
    if (idx == SmallMap.NOT_FOUND) {
      return doMultiNewKeyCached(
          state,
          _this,
          key,
          local_state,
          computation,
          key,
          state.getKeys(),
          idx,
          buildNewKeys(key, state.getKeys()));
    } else {
      return doMultiExistingKeyCached(
          state, _this, key, local_state, computation, key, state.getKeys(), idx);
    }
  }

  Object[] buildNewKeys(Object newKey, Object[] oldKeys) {
    Object[] result = new Object[oldKeys.length + 1];
    System.arraycopy(oldKeys, 0, result, 1, oldKeys.length);
    result[0] = newKey;
    return result;
  }
}
