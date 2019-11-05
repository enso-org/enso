package org.enso.interpreter.runtime.state;

/** Represents the result of a stateful computation. */
public class Stateful {
  private final Object state;
  private final Object value;

  /**
   * Creates a new stateful value.
   *
   * @param state the new state to be used by further computations
   * @param value the actual (returned) result of the computation
   */
  public Stateful(Object state, Object value) {
    this.state = state;
    this.value = value;
  }

  /**
   * Returns the new state value.
   *
   * @return the new state value
   */
  public Object getState() {
    return state;
  }

  /**
   * Returns the result value.
   *
   * @return the result value
   */
  public Object getValue() {
    return value;
  }
}
