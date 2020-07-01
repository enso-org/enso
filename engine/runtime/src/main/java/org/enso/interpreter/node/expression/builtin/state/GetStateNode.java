package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Location;
import com.oracle.truffle.api.object.Property;
import com.oracle.truffle.api.object.Shape;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;

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
      @Cached("state.getShape()") Shape cachedShape,
      @Cached("getLocation(key, cachedShape)") Location location) {
    return location.get(state, cachedShape);
  }

  Location getLocation(Object key, Shape shape) {
    CompilerAsserts.neverPartOfCompilation();
    Property prop = shape.getProperty(key);
    if (prop == null) {
      return null;
    }
    return prop.getLocation();
  }
}
