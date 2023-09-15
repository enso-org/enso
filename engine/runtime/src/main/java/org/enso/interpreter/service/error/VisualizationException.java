package org.enso.interpreter.service.error;

/** Thrown when the execution of the visualization expression fails. */
public class VisualizationException extends RuntimeException implements ServiceException {

  /**
   * Create new instance of this error.
   *
   * @param message the error message.
   */
  public VisualizationException(String message) {
    super(message);
  }
}
