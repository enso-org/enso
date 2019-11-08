package org.enso.interpreter.runtime.error;

/**
 * An exception thrown when a function is defined with duplicate argument names.
 */
public class DuplicateArgumentNameException extends RuntimeException {

  /**
   * Creates a new error.
   *
   * @param name the name defined multiple times
   */
  public DuplicateArgumentNameException(String name) {
    super("A function cannot have multiple arguments called " + name);
  }
}
