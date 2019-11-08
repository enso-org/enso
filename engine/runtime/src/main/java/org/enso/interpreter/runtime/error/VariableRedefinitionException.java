package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/**
 * An exception thrown when a program attempts to redefine a variable that has already been defined.
 */
public class VariableRedefinitionException extends RuntimeException implements TruffleException {

  /**
   * Creates the error.
   *
   * @param name the name of the already-defined variable
   */
  public VariableRedefinitionException(String name) {
    super("Variable " + name + " was already defined in this scope.");
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
