package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/**
 * An exception thrown on a type mismatch.
 */
public class TypeError extends RuntimeException implements TruffleException {
  private final Node node;

  /**
   * Creates the error.
   *
   * @param message an informative error message
   * @param node the node where the error occurred
   */
  public TypeError(String message, Node node) {
    super(message);
    this.node = node;
  }

  /**
   * Gets the location of the error.
   *
   * @return the node where the error occurred.
   */
  @Override
  public Node getLocation() {
    return node;
  }
}
