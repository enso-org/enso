package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Location;
import com.oracle.truffle.api.object.Property;
import com.oracle.truffle.api.object.Shape;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.EmptyState;
import org.enso.interpreter.runtime.data.SingletonState;
import org.enso.interpreter.runtime.data.SmallMap;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

import java.util.Arrays;

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
  Object doSingleton(SingletonState state, Object _this, Object key) {
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
      throw new PanicException(
          ctxRef.get().getBuiltins().error().unitializedState().newInstance(key), this);
    } else {
      return state.getValues()[idx];
    }
  }

  @Specialization
  Object doEmpty(
      EmptyState state, Object _this, Object key, @CachedContext(Language.class) Context ctx) {
    throw new PanicException(ctx.getBuiltins().error().unitializedState().newInstance(key), this);
  }

  @Specialization
  Object doSingletonError(
      SingletonState state, Object _this, Object key, @CachedContext(Language.class) Context ctx) {
    throw new PanicException(ctx.getBuiltins().error().unitializedState().newInstance(key), this);
  }
}
