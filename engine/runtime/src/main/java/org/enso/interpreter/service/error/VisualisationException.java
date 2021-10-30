package org.enso.interpreter.service.error;

/** Thrown when the execution of the visualisation expression fails. */
public class VisualisationException extends RuntimeException implements ServiceException {

  /**
   * Create new instance of this error.
   *
   * @param message the error message.
   */
  public VisualisationException(String message) {
    super(message);
  }
}
