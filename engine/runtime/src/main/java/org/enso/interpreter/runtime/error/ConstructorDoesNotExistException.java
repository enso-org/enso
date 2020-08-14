package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/** Thrown when a given module does not implement the requested constructor. */
public class ConstructorDoesNotExistException extends RuntimeException implements TruffleException {
  private final Node node;

  /**
   * Creates a new instance of this error.
   *
   * @param moduleName the qualified module name.
   * @param consName the name of the non-existent constructor.
   * @param node the node where the erroneous invocation took place
   */
  public ConstructorDoesNotExistException(String moduleName, String consName, Node node) {
    super("Module " + moduleName + " does not define constructor " + consName + ".");
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
