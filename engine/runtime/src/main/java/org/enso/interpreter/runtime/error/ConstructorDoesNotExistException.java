package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/** Thrown when a constructor was requested for execution but could not be found. */
public class ConstructorDoesNotExistException extends RuntimeException implements TruffleException {
  /**
   * Creates a new instance of this error.
   *
   * @param moduleName the qualified module name.
   * @param consName the name of the non-existent constructor.
   */
  public ConstructorDoesNotExistException(String moduleName, String consName) {
    super("Module " + moduleName + " does not define constructor " + consName + ".");
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
