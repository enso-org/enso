package org.enso.interpreter.service.error;

/** Thrown when the source code of the execution item does not exist. */
public class SourceNotFoundException extends RuntimeException implements ServiceException {

  /**
   * Create new instance of this error.
   *
   * @param item the item which source code is missing.
   */
  public SourceNotFoundException(Object item) {
    super("The " + item + " source not found.");
  }

  /**
   * Create new instance of this error.
   *
   * @param item the item which source code is missing.
   * @param cause the cause of the exception.
   */
  public SourceNotFoundException(Object item, Throwable cause) {
    super("The " + item + " source not found.", cause);
  }
}
