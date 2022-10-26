package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Shape;
import org.enso.interpreter.runtime.Context;

public class State {
  private final Container container;
  private final IOPermissions ioPermissions;

  public State(Container container, IOPermissions ioPermissions) {
    this.container = container;
    this.ioPermissions = ioPermissions;
  }

  public Container getContainer() {
    return container;
  }

  public IOPermissions getIoPermissions() {
    return ioPermissions;
  }

  public static State create(Context context) {
    return new State(Container.create(context), context.getRootIOPermissions());
  }

  public State withInputAllowedIn(String name) {
    return new State(container, ioPermissions.allowInputIn(name));
  }

  public State withOutputAllowedIn(String name) {
    return new State(container, ioPermissions.allowOutputIn(name));
  }

  public static class Container extends DynamicObject {
    private Container(Shape shape) {
      super(shape);
    }

    public static Container create(Context context) {
      return new Container(context.getRootStateShape());
    }
  }
}
