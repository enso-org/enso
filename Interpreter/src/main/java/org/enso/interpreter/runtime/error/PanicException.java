package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/** An exception type for user thrown panic exceptions. */
public class PanicException extends RuntimeException implements TruffleException {
  private final Object payload;
  private final Node location;

  /**
   * Creates an instance of this class.
   *
   * @param payload arbitrary, user-provided payload carried by this exception
   * @param location the node throwing this exception, for use in guest stack traces
   */
  public PanicException(Object payload, Node location) {
    this.payload = payload;
    this.location = location;
  }

  /**
   * Returns the location where this exception was thrown.
   *
   * @return the original throw location
   */
  @Override
  public Node getLocation() {
    return location;
  }

  /**
   * Returns the payload carried by this exception.
   *
   * @return the payload object
   */
  @Override
  public Object getExceptionObject() {
    return payload;
  }
}
