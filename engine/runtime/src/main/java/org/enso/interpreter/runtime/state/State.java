package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Shape;

public record State(Container container, IOPermissions ioPermissions) {

  public State withInputAllowedIn(String name) {
    return new State(container, ioPermissions.allowInputIn(name));
  }

  public State withOutputAllowedIn(String name) {
    return new State(container, ioPermissions.allowOutputIn(name));
  }

  public static class Container extends DynamicObject {
    public Container(Shape shape) {
      super(shape);
    }
  }

}
