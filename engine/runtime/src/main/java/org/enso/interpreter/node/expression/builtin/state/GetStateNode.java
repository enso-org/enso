package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Location;
import com.oracle.truffle.api.object.Property;
import com.oracle.truffle.api.object.Shape;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "State",
    name = "get",
    description = "Returns the current value of monadic state.")
public abstract class GetStateNode extends Node {
  static GetStateNode build() {
    return GetStateNodeGen.create();
  }

  abstract Object execute(@MonadicState DynamicObject state, Object _this, Object key);

  @Specialization(
      guards = {"state.getShape() == cachedShape", "key == cachedKey"},
      assumptions = {"cachedShape.getValidAssumption()"})
  Object doExecute(
      DynamicObject state,
      Object _this,
      Object key,
      @Cached("key") Object cachedKey,
      @Cached("key.toString()") String keyStr,
      @Cached("state.getShape()") Shape cachedShape,
      @Cached("getLocation(keyStr, cachedShape)") Location location) {
    return location.get(state, cachedShape);
  }

  @Specialization
  Stateful doFall(DynamicObject state, Object _this, Object key) {
    throw new RuntimeException("That's unexpected...");
  }

  @CompilerDirectives.TruffleBoundary
  Location getLocation(String key, Shape shape) {
    CompilerAsserts.neverPartOfCompilation();
    Property prop = shape.getProperty(key);
    if (prop == null) {
      return null;
    }
    return prop.getLocation();
  }
}
