package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/** Thrown when a given Object does not implement the requested Enso-level method. */
public class MethodDoesNotExistException extends RuntimeException implements TruffleException {
  private final Node node;

  /**
   * Creates the error.
   *
   * @param target the construct that was attempted to be invoked
   * @param node the node where the erroneous invocation took place
   */
  public MethodDoesNotExistException(Object target, String name, Node node) {
    super("Object " + target + " does not define method " + name + ".");
    this.node = node;
  }

  /**
   * The location where the erroneous invocation took place.
   *
   * @return the node where the error occurred
   */
  @Override
  public Node getLocation() {
    return node;
  }
}
