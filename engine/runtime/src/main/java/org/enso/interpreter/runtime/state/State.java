package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.object.DynamicObject;
import com.oracle.truffle.api.object.Shape;
import java.lang.invoke.MethodHandles;
import org.enso.interpreter.runtime.EnsoContext;

/**
 * Represents a <em>thread local</em> state associated with execution of the program. Use nodes:
 *
 * <ul>
 *   <li>{@link RunStateNode} to run execution with some state
 *   <li>{@link GetStateNode} to read value in a state
 *   <li>{@link PutStateNode} to change value in a state
 * </ul>
 *
 * First and foremost use {@link RunStateNode} to define a new key in the {@link State}. Then use
 * the other nodes to read and update the value. The key and its value is removed from the {@link
 * State} when {@link RunStateNode#execute} method returns. Current state can be obtained by {@link
 * EnsoContext#currentState} method.
 */
public final class State {
  private static final EnsoContext.Extra<Shape> ROOT_STATE_SHAPE =
      new EnsoContext.Extra<>(
          Shape.class,
          (ctx) ->
              Shape.newBuilder().layout(State.Container.class, State.Container.lookup()).build());

  private final Container container;

  private State(Container container) {
    this.container = container;
  }

  /**
   * Creates new, empty state for given context.
   *
   * @param context the context
   * @return new instance of the state for the context
   */
  public static State create(EnsoContext context) {
    return new State(Container.create(context));
  }

  Container getContainer() {
    return container;
  }

  static final class Container extends DynamicObject {
    private Container(Shape shape) {
      super(shape);
    }

    static Container create(EnsoContext context) {
      return new Container(ROOT_STATE_SHAPE.get(context));
    }

    static MethodHandles.Lookup lookup() {
      return MethodHandles.lookup();
    }
  }
}
