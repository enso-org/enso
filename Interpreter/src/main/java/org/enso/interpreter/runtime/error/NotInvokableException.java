package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/** An exception thrown when execution attempts to invoke something that cannot be invoked. */
public class NotInvokableException extends RuntimeException implements TruffleException {
  private final Node node;

  /**
   * Creates the error.
   *
   * @param target the construct that was attempted to be invoked
   * @param node the node where the erroneous invocation took place
   */
  public NotInvokableException(Object target, Node node) {
    super("Object " + target + " is not invokable.");
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
