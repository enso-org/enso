package org.enso.interpreter.runtime.error;

/**
 * An exception that is thrown whenever a call is made to a {@link
 * org.enso.interpreter.runtime.callable.Callable} with the wrong number of arguments.
 */
public class ArityException extends RuntimeException {

  /**
   * Creates a new error.
   *
   * @param expected the expected number of arguments
   * @param actual the provided number of arguments
   */
  public ArityException(int expected, int actual) {
    super("Wrong number of arguments. Expected: " + expected + " but got: " + actual + ".");
  }
}
