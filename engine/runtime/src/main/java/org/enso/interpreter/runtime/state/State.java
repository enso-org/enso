package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Shape;
import org.enso.interpreter.runtime.EnsoContext;

public class State {
  private final Container container;

  public State(Container container) {
    this.container = container;
  }

  public Container getContainer() {
    return container;
  }

  public static State create(EnsoContext context) {
    return new State(Container.create(context));
  }

  public static class Container extends DynamicObject {
    private Container(Shape shape) {
      super(shape);
    }

    public static Container create(EnsoContext context) {
      return new Container(context.getRootStateShape());
    }
  }
}
