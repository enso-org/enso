package org.enso.interpreter.service.error;

/** Thrown when the source code of the execution item does not exist. */
public class SourceNotFoundException extends RuntimeException implements ServiceException {

  /**
   * Create new instance of this error.
   *
   * @param item the item which source code is missing.
   */
  public SourceNotFoundException(String item) {
    super("The " + item + " source not found.");
  }
}
