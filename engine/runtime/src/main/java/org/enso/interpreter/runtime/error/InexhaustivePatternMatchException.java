package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/**
 * An error thrown when a pattern match is is inexhaustive and execution cannot find a branch that
 * is able to be executed.
 */
public class InexhaustivePatternMatchException extends RuntimeException
    implements TruffleException {
  private final Node node;

  /**
   * Creates a new error.
   *
   * @param node the node where the fallthrough occurred
   */
  public InexhaustivePatternMatchException(Node node, Object unhandledScrutinee) {
    super("Inexhaustive pattern match: the " + unhandledScrutinee + " case is not handled.");
    this.node = node;
  }

  /**
   * Gets the location where the error occurred.
   *
   * @return the node where the error occurred
   */
  @Override
  public Node getLocation() {
    return node;
  }
}
