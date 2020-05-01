package org.enso.interpreter.runtime.error;

/** An exception thrown when the program tries to redefine an already-defined method */
public class RedefinedMethodException extends RuntimeException {

  /**
   * Creates a new error.
   *
   * @param atom the method of the atom on which {@code method} is being defined
   * @param method the name of the method being redefined
   */
  public RedefinedMethodException(String atom, String method) {
    super("Methods cannot be overloaded, but you have tried to overload " + atom + "." + method);
  }
}
