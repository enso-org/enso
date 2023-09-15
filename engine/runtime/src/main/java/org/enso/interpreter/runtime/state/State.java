package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Shape;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;

public class State {
  private final Container container;

  private final ExecutionEnvironment executionEnvironment;

  public State(Container container, ExecutionEnvironment executionEnvironment) {
    this.container = container;
    this.executionEnvironment = executionEnvironment;
  }

  public Container getContainer() {
    return container;
  }

  public ExecutionEnvironment currentEnvironment() {
    return executionEnvironment;
  }

  public static State create(EnsoContext context) {
    return new State(Container.create(context), context.getExecutionEnvironment());
  }

  public State withContextEnabledIn(Atom context, String environmentName) {
    if (executionEnvironment.getName().equals(environmentName)) {
      return new State(container, executionEnvironment.withContextEnabled(context));
    } else {
      return this;
    }
  }

  public State withContextDisabledIn(Atom context, String environmentName) {
    if (executionEnvironment.getName().equals(environmentName)) {
      return new State(container, executionEnvironment.withContextDisabled(context));
    } else {
      return this;
    }
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
