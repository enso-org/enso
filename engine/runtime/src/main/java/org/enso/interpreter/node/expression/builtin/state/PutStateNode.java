package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.*;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(type = "State", name = "put", description = "Updates the value of monadic state.")
@ReportPolymorphism
public abstract class PutStateNode extends Node {
  static PutStateNode build() {
    return PutStateNodeGen.create();
  }

  abstract Stateful execute(
      @MonadicState DynamicObject state, Object _this, Object key, Object new_state);

  @Specialization(
      guards = {
        "state.getShape() == cachedShape",
        "key == cachedKey",
        "location != null",
        "location.canSet(new_state)"
      },
      assumptions = {"cachedShape.getValidAssumption()"})
  Stateful doExisting(
      DynamicObject state,
      Object _this,
      Object key,
      Object new_state,
      @Cached("key") Object cachedKey,
      @Cached("state.getShape()") Shape cachedShape,
      @Cached("getLocation(key, cachedShape, new_state)") Location location) {
    DynamicObject changedState = state.copy(cachedShape);
    try {
      location.set(changedState, new_state, cachedShape);
    } catch (IncompatibleLocationException | FinalLocationException e) {
      throw new IllegalStateException("Impossible.");
    }
    return new Stateful(changedState, new_state);
  }

  @Specialization(
      guards = {
        "state.getShape() == cachedShape",
        "key == cachedKey",
        "oldLocation == null",
        "newLocation.canStore(new_state)"
      },
      assumptions = {"cachedShape.getValidAssumption()", "nextShape.getValidAssumption()"})
  Stateful doNewLocation(
      DynamicObject state,
      Object _this,
      Object key,
      Object new_state,
      @Cached("key") Object cachedKey,
      @Cached("state.getShape()") Shape cachedShape,
      @Cached("getLocation(key, cachedShape, new_state)") Location oldLocation,
      @Cached("defineProp(cachedShape, key, new_state)") Shape nextShape,
      @Cached("getLocation(key, nextShape)") Location newLocation) {
    DynamicObject changedState = state.copy(cachedShape);
    try {
      newLocation.set(changedState, new_state, cachedShape, nextShape);
    } catch (IncompatibleLocationException e) {
      throw new IllegalStateException("Impossible.");
    }
    return new Stateful(changedState, new_state);
  }

  Location getLocation(Object key, Shape shape) {
    CompilerAsserts.neverPartOfCompilation();
    Property property = shape.getProperty(key);
    if (property == null) return null;
    return property.getLocation();
  }

  Location getLocation(Object key, Shape shape, Object value) {
    Location loc = getLocation(key, shape);
    if (loc == null || !loc.canSet(value)) {
      return null;
    }
    return loc;
  }

  Shape defineProp(Shape shape, Object key, Object value) {
    return shape.defineProperty(key, value, 0);
  }
}
