package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/** Thrown when a constructor was requested for execution but could not be found. */
public class ConstructorDoesNotExistException extends RuntimeException implements TruffleException {
  /**
   * Creates a new instance of this error.
   *
   * @param name the qualified name of the non-existent constructor.
   */
  public ConstructorDoesNotExistException(String name) {
    super("Constructor " + name + " does not exist.");
  }

  /**
   * Reports a null location for this exception.
   *
   * @return null.
   */
  @Override
  public Node getLocation() {
    return null;
  }
}
