package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/** An error thrown when the program attempts to read from a variable that has not been defined. */
public class VariableDoesNotExistException extends RuntimeException implements TruffleException {

  /**
   * Creates the error.
   *
   * @param name the name of the undefined variable
   */
  public VariableDoesNotExistException(String name) {
    super("Variable " + name + " is not defined.");
  }

  /**
   * Gets the location of the error.
   *
   * @return the node where the error occurred.
   */
  @Override
  public Node getLocation() {
    return null;
  }
}
