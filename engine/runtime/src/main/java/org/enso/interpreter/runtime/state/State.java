package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import com.oracle.truffle.api.object.Shape;
import org.enso.interpreter.runtime.Context;

public class State {
  private Container container;

  public State(Container container) {
    this.container = container;
  }

  public State duplicate() {
    return this;
  }

  public Container getContainer() {
    return container;
  }

  public void setContainer(Container container) {
    this.container = container;
  }

  public static class Container extends DynamicObject {
    public Container(Shape shape) {
      super(shape);
    }

    public Container backup(DynamicObjectLibrary objects, Context context) {
      var result = context.emptyState().getContainer();
      var keys = objects.getKeyArray(this);
      for (var key : keys) {
        objects.put(result, key, objects.getOrDefault(this, key, null));
      }
      return result;
    }
  }
}
