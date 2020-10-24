package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.TruffleObject;

/** A mutable reference type. */
public class Ref implements TruffleObject {
  private Object value;

  /**
   * Creates a new reference.
   *
   * @param value the initial value to store in the reference.
   */
  public Ref(Object value) {
    this.value = value;
  }

  /** @return the current value of the reference. */
  public Object getValue() {
    return value;
  }

  /**
   * Stores a new value in the reference.
   *
   * @param value the value to store.
   */
  public void setValue(Object value) {
    this.value = value;
  }
}
