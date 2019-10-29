package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.TruffleObject;

/** A runtime object representing an arbitrary, user-created error. */
public class RuntimeError implements TruffleObject {
  private final Object payload;

  /**
   * Creates an instance of this error.
   *
   * @param payload arbitrary, user-provided payload to be carried by this object.
   */
  public RuntimeError(Object payload) {
    this.payload = payload;
  }

  /**
   * Returns the user provided payload carried by this object.
   *
   * @return the payload object
   */
  public Object getPayload() {
    return payload;
  }

  /**
   * Represents this object as a string.
   *
   * @return a string representation of this object
   */
  @Override
  public String toString() {
    return "Error:" + getPayload().toString();
  }
}
