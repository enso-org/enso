package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

/** Thrown when a module was requested for importing but could not be found. */
public class ModuleDoesNotExistException extends RuntimeException implements TruffleException {
  /**
   * Creates a new instance of this error.
   *
   * @param name the qualified name of the non-existent module
   */
  public ModuleDoesNotExistException(String name) {
    super("Module " + name + " does not exist.");
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
