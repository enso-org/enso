package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Shape;
import org.enso.interpreter.runtime.EnsoContext;

public final class State {

  private final Container container;

  private State(Container container) {
    this.container = container;
  }

  Container getContainer() {
    return container;
  }

  public static Shape newShape() {
    return Shape.newBuilder().layout(State.Container.class).build();
  }

  public static State create(EnsoContext context) {
    return new State(Container.create(context));
  }

  static final class Container extends DynamicObject {
    private Container(Shape shape) {
      super(shape);
    }

    static Container create(EnsoContext context) {
      return new Container(context.getRootStateShape());
    }
  }
}
