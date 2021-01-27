package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
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

  abstract Object execute(@MonadicState Object state, Object _this, Object key);

  @Specialization(guards = {"state.getKey() == key"})
  Object doSingleton(SingletonMap state, Object _this, Object key) {
    return state.getValue();
  }

  @Specialization(
      guards = {"state.getKeys() == cachedKeys", "key == cachedKey", "idx != NOT_FOUND"})
  Object doMultiCached(
      SmallMap state,
      Object _this,
      Object key,
      @Cached("key") Object cachedKey,
      @Cached(value = "state.getKeys()", dimensions = 1) Object[] cachedKeys,
      @Cached("state.indexOf(key)") int idx) {
    return state.getValues()[idx];
  }

  @Specialization
  Object doMultiUncached(
      SmallMap state,
      Object _this,
      Object key,
      @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> ctxRef) {
    int idx = state.indexOf(key);
    if (idx == SmallMap.NOT_FOUND) {
      return DataflowError.withDefaultTrace(
          ctxRef.get().getBuiltins().error().uninitializedState().newInstance(key), this);
    } else {
      return state.getValues()[idx];
    }
  }

  @Specialization
  Object doEmpty(
      EmptyMap state, Object _this, Object key, @CachedContext(Language.class) Context ctx) {
    return DataflowError.withDefaultTrace(
        ctx.getBuiltins().error().uninitializedState().newInstance(key), this);
  }

  @Specialization
  Object doSingletonError(
      SingletonMap state, Object _this, Object key, @CachedContext(Language.class) Context ctx) {
    return DataflowError.withDefaultTrace(
        ctx.getBuiltins().error().uninitializedState().newInstance(key), this);
  }
}
